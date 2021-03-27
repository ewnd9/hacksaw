pub mod parse_args;
pub mod parse_format;

extern crate structopt;
extern crate xcb;

use structopt::StructOpt;

use self::parse_format::FormatToken;
use self::parse_args::Opt;
use xcb::shape;

pub const CURSOR_GRAB_TRIES: i32 = 5;
const ESC_KEYSYM: u32 = 0xff1b;

/// Since MOD_MASK_ANY is apparently bug-ridden, we instead exploit the fact
/// that the modifier masks NONE to MOD_MASK_5 are 0, 1, 2, 4, 8, ... 128.
/// Then we grab on every possible combination of these masks by iterating
/// through all the integers 0 to 255. This allows us to grab Esc, Shift+Esc,
/// CapsLock+Shift+Esc, or any other combination.
const KEY_GRAB_MASK_MAX: xcb::ModMask = (xcb::MOD_MASK_5 * 2) - 1;

#[derive(Clone, Copy)]
pub struct HacksawResult {
    pub window: u32,
    pub rect: xcb::Rectangle,
}

impl HacksawResult {
    pub fn x(&self) -> i16 {
        self.rect.x()
    }
    pub fn y(&self) -> i16 {
        self.rect.y()
    }
    pub fn width(&self) -> u16 {
        self.rect.width()
    }
    pub fn height(&self) -> u16 {
        self.rect.height()
    }

    pub fn relative_to(&self, parent: HacksawResult) -> HacksawResult {
        HacksawResult {
            window: self.window,
            rect: xcb::Rectangle::new(
                parent.x() + self.x(),
                parent.y() + self.y(),
                self.width(),
                self.height(),
            ),
        }
    }

    fn contains(&self, point: xcb::Point) -> bool {
        // TODO negative x/y offsets from bottom or right?
        self.x() < point.x()
            && self.y() < point.y()
            && point.x() - self.x() <= self.width() as i16
            && point.y() - self.y() <= self.height() as i16
    }

    pub fn fill_format_string(&self, format: &[FormatToken]) -> String {
        format
            .iter()
            .map(|token| match token {
                FormatToken::WindowId => self.window.to_string(),
                FormatToken::Geometry => format!(
                    "{}x{}+{}+{}",
                    self.width(),
                    self.height(),
                    self.x(),
                    self.y(),
                ),
                FormatToken::Width => self.width().to_string(),
                FormatToken::Height => self.height().to_string(),
                FormatToken::X => self.x().to_string(),
                FormatToken::Y => self.y().to_string(),
                FormatToken::Literal(s) => s.to_string(),
            })
            .collect::<Vec<_>>()
            .join("")
    }
}

pub fn set_shape(conn: &xcb::Connection, window: xcb::Window, rects: &[xcb::Rectangle]) {
    shape::rectangles(
        &conn,
        shape::SO_SET as u8,
        shape::SK_BOUNDING as u8,
        0,
        window,
        0,
        0,
        &rects,
    );
}

pub fn set_title(conn: &xcb::Connection, window: xcb::Window, title: &str) {
    xcb::change_property(
        &conn,
        xcb::PROP_MODE_REPLACE as u8,
        window,
        xcb::ATOM_WM_NAME,
        xcb::ATOM_STRING,
        8,
        title.as_bytes(),
    );
}

pub fn grab_pointer_set_cursor(conn: &xcb::Connection, root: u32) -> bool {
    let font = conn.generate_id();
    xcb::open_font(&conn, font, "cursor");

    // TODO: create cursor with a Pixmap
    // https://stackoverflow.com/questions/40578969/how-to-create-a-cursor-in-x11-from-raw-data-c
    let cursor = conn.generate_id();
    xcb::create_glyph_cursor(&conn, cursor, font, font, 0, 30, 0, 0, 0, 0, 0, 0);

    for i in 0..CURSOR_GRAB_TRIES {
        let reply = xcb::grab_pointer(
            &conn,
            true,
            root,
            (xcb::EVENT_MASK_BUTTON_RELEASE
                | xcb::EVENT_MASK_BUTTON_PRESS
                | xcb::EVENT_MASK_BUTTON_MOTION
                | xcb::EVENT_MASK_POINTER_MOTION) as u16,
            xcb::GRAB_MODE_ASYNC as u8,
            xcb::GRAB_MODE_ASYNC as u8,
            xcb::NONE,
            cursor,
            xcb::CURRENT_TIME,
        )
        .get_reply()
        .unwrap();

        if reply.status() as u32 == xcb::GRAB_STATUS_SUCCESS {
            return true;
        } else if i < CURSOR_GRAB_TRIES - 1 {
            std::thread::sleep(std::time::Duration::from_millis(100));
        }
    }

    false
}

pub fn find_escape_keycode(conn: &xcb::Connection) -> xcb::Keycode {
    // https://stackoverflow.com/questions/18689863/obtain-keyboard-layout-and-keysyms-with-xcb
    let setup = conn.get_setup();
    let cookie = xcb::get_keyboard_mapping(
        &conn,
        setup.min_keycode(),
        setup.max_keycode() - setup.min_keycode() + 1,
    );
    let reply = cookie.get_reply().expect("failed to get keyboard mapping");

    let escape_index = reply
        .keysyms()
        .iter()
        .position(|&keysym| keysym == ESC_KEYSYM)
        .expect("failed to find escape keysym");
    (escape_index / reply.keysyms_per_keycode() as usize) as u8 + setup.min_keycode()
}

pub fn grab_key(conn: &xcb::Connection, root: u32, keycode: u8) {
    for mask in 0..=KEY_GRAB_MASK_MAX {
        xcb::grab_key(
            &conn,
            true,
            root,
            mask as u16,
            keycode,
            xcb::GRAB_MODE_ASYNC as u8,
            xcb::GRAB_MODE_ASYNC as u8,
        );
    }
}

pub fn ungrab_key(conn: &xcb::Connection, root: u32, keycode: u8) {
    for mask in 0..=KEY_GRAB_MASK_MAX {
        xcb::ungrab_key(&conn, keycode, root, mask as u16);
    }
}

fn viewable(conn: &xcb::Connection, win: xcb::Window) -> bool {
    let attrs = xcb::get_window_attributes(conn, win).get_reply().unwrap();
    (attrs.map_state() & xcb::MAP_STATE_VIEWABLE as u8) != 0
}

pub fn input_output(conn: &xcb::Connection, win: xcb::Window) -> bool {
    let attrs = xcb::get_window_attributes(conn, win).get_reply().unwrap();
    (attrs.class() & xcb::WINDOW_CLASS_INPUT_OUTPUT as u16) != 0
}

pub fn get_window_geom(conn: &xcb::Connection, win: xcb::Window) -> HacksawResult {
    let geom = xcb::get_geometry(conn, win).get_reply().unwrap();

    HacksawResult {
        window: win,
        rect: xcb::Rectangle::new(
            geom.x(),
            geom.y(),
            geom.width() + 2 * geom.border_width(),
            geom.height() + 2 * geom.border_width(),
        ),
    }
}

pub fn get_window_at_point(
    conn: &xcb::Connection,
    win: xcb::Window,
    pt: xcb::Point,
    remove_decorations: u32,
) -> Option<HacksawResult> {
    let tree = xcb::query_tree(conn, win).get_reply().unwrap();
    let children = tree
        .children()
        .iter()
        .filter(|&child| viewable(conn, *child))
        .filter(|&child| input_output(conn, *child))
        .filter_map(|&child| {
            let geom = get_window_geom(conn, child);
            if geom.contains(pt) {
                Some(geom)
            } else {
                None
            }
        })
        .collect::<Vec<_>>();

    if children.is_empty() {
        return None;
    }

    let mut window = children[children.len() - 1];
    for _ in 0..remove_decorations {
        let tree = xcb::query_tree(conn, window.window).get_reply().unwrap();
        if tree.children_len() == 0 {
            break;
        }
        let firstborn = tree.children()[0];
        window = get_window_geom(conn, firstborn).relative_to(window);
    }

    Some(window)
}

fn min_max(a: i16, b: i16) -> (i16, i16) {
    if a < b {
        (a, b)
    } else {
        (b, a)
    }
}

fn build_guides(screen: xcb::Rectangle, pt: xcb::Point, width: u16) -> [xcb::Rectangle; 2] {
    [
        xcb::Rectangle::new(
            pt.x() - width as i16 / 2,
            screen.x(),
            width,
            screen.height(),
        ),
        xcb::Rectangle::new(screen.y(), pt.y() - width as i16 / 2, screen.width(), width),
    ]
}

pub fn run_hacksaw() -> Result<HacksawResult, String> {
    let opt = Opt::from_args();

    let line_width = opt.select_thickness;
    let guide_width = opt.guide_thickness;
    let line_colour = opt.line_colour;
    let format = opt.format;

    let (conn, screen_num) = xcb::Connection::connect(None).unwrap();
    let setup = conn.get_setup();
    let screen = setup.roots().nth(screen_num as usize).unwrap();
    let root = screen.root();

    let window = conn.generate_id();

    // TODO fix pointer-grab? bug where hacksaw hangs if mouse held down before run
    if !grab_pointer_set_cursor(&conn, root) {
        return Err(format!(
            "Failed to grab cursor after {} tries, giving up",
            CURSOR_GRAB_TRIES
        ));
    }

    let escape_keycode = find_escape_keycode(&conn);
    grab_key(&conn, root, escape_keycode);

    let screen_rect =
        xcb::Rectangle::new(0, 0, screen.width_in_pixels(), screen.height_in_pixels());

    // TODO event handling for expose/keypress
    let values = [
        // ?RGB. First 4 bytes appear to do nothing
        (xcb::CW_BACK_PIXEL, line_colour),
        (
            xcb::CW_EVENT_MASK,
            xcb::EVENT_MASK_EXPOSURE
            | xcb::EVENT_MASK_KEY_PRESS // we'll need this later
            | xcb::EVENT_MASK_STRUCTURE_NOTIFY
            | xcb::EVENT_MASK_SUBSTRUCTURE_NOTIFY,
        ),
        (xcb::CW_OVERRIDE_REDIRECT, 1u32), // Don't be window managed
    ];

    xcb::create_window(
        &conn,
        xcb::COPY_FROM_PARENT as u8, // usually 32?
        window,
        root,
        screen_rect.x(),
        screen_rect.y(),
        screen_rect.width(),
        screen_rect.height(),
        0,
        xcb::WINDOW_CLASS_INPUT_OUTPUT as u16,
        screen.root_visual(),
        &values,
    );

    set_title(&conn, window, "hacksaw");

    set_shape(&conn, window, &[xcb::Rectangle::new(0, 0, 0, 0)]);

    xcb::map_window(&conn, window);

    if !opt.no_guides {
        let pointer = xcb::query_pointer(&conn, root).get_reply().unwrap();
        set_shape(
            &conn,
            window,
            &build_guides(
                screen_rect,
                xcb::Point::new(pointer.root_x(), pointer.root_y()),
                guide_width,
            ),
        );
    }

    conn.flush();

    let mut start_pt = xcb::Point::new(0, 0);
    let mut selection = xcb::Rectangle::new(0, 0, 0, 0);

    let mut in_selection = false;
    let mut ignore_next_release = false;

    // TODO draw rectangle around window under cursor
    loop {
        let ev = conn
            .wait_for_event()
            .ok_or_else(|| "Error getting X event, quitting.".to_string())?;

        match ev.response_type() {
            xcb::BUTTON_PRESS => {
                let button_press: &xcb::ButtonPressEvent = unsafe { xcb::cast_event(&ev) };

                let detail = button_press.detail();
                if detail == 3 {
                    return Err("Exiting due to right click".into());
                } else {
                    set_shape(&conn, window, &[]);
                    conn.flush();
                    start_pt = xcb::Point::new(button_press.event_x(), button_press.event_y());

                    in_selection = !(detail == 4 || detail == 5);
                    ignore_next_release = detail == 4 || detail == 5;
                }
            }
            xcb::KEY_PRESS => {
                // This will only happen with an escape key since we only grabbed escape
                return Err("Exiting due to ESC key press".into());
            }
            xcb::MOTION_NOTIFY => {
                let motion: &xcb::MotionNotifyEvent = unsafe { xcb::cast_event(&ev) };

                let (left_x, right_x) = min_max(motion.event_x(), start_pt.x());
                let (top_y, bottom_y) = min_max(motion.event_y(), start_pt.y());
                let width = (right_x - left_x) as u16;
                let height = (bottom_y - top_y) as u16;

                // only save the width and height if we are selecting a
                // rectangle, since we then use these (non-zero width/height)
                // to determine if a selection was made.
                if in_selection {
                    selection = xcb::Rectangle::new(left_x, top_y, width, height);
                } else {
                    selection = xcb::Rectangle::new(left_x, top_y, 0, 0);
                }

                if in_selection {
                    let rects = [
                        // Selection rectangle
                        xcb::Rectangle::new(
                            left_x - line_width as i16,
                            top_y,
                            line_width,
                            height + line_width,
                        ),
                        xcb::Rectangle::new(
                            left_x - line_width as i16,
                            top_y - line_width as i16,
                            width + line_width,
                            line_width,
                        ),
                        xcb::Rectangle::new(
                            right_x,
                            top_y - line_width as i16,
                            line_width,
                            height + line_width,
                        ),
                        xcb::Rectangle::new(left_x, bottom_y, width + line_width, line_width),
                    ];
                    set_shape(&conn, window, &rects);
                } else if !opt.no_guides {
                    let rects = build_guides(
                        screen_rect,
                        xcb::Point::new(motion.event_x(), motion.event_y()),
                        guide_width,
                    );

                    set_shape(&conn, window, &rects);
                }

                conn.flush();
            }
            xcb::BUTTON_RELEASE => {
                let motion: &xcb::ButtonReleaseEvent = unsafe { xcb::cast_event(&ev) };
                let detail = motion.detail();
                if detail == 4 || detail == 5 {
                    continue; // Scroll wheel up/down release
                } else if ignore_next_release {
                    ignore_next_release = false;
                    continue;
                } else {
                    break;
                }
                // Move on after mouse released
            }
            _ => continue,
        };
    }

    xcb::ungrab_pointer(&conn, xcb::CURRENT_TIME);
    ungrab_key(&conn, root, escape_keycode);
    xcb::unmap_window(&conn, window);
    xcb::destroy_window(&conn, window);
    conn.flush();

    loop {
        let ev = conn
            .wait_for_event()
            .ok_or_else(|| "Error getting X event, quitting.".to_string())?;

        match ev.response_type() {
            xcb::UNMAP_NOTIFY => {
                break;
            }
            xcb::DESTROY_NOTIFY => {
                break;
            }
            _ => (),
        }
    }
    std::thread::sleep(std::time::Duration::from_millis(40));

    let result;
    if selection.width() == 0 && selection.height() == 0 {
        // Grab window under cursor
        result = match get_window_at_point(&conn, root, start_pt, opt.remove_decorations) {
            Some(r) => r,
            None => get_window_geom(&conn, screen.root()),
        }
    } else {
        result = HacksawResult {
            window: root,
            rect: selection,
        };
    }

    Ok(result)
}
