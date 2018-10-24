use itertools::{Zip, Itertools};
use num_traits::{FromPrimitive, ToPrimitive};

use super::{
    WorldState, Board, Robot, Charset, Palette, Sensor, Thing, OverlayMode, Coordinate, Size,
    CharId, Explosion,
};

pub trait Renderer {
    fn put_pixel(
        &mut self,
        x: usize,
        y: usize,
        r: u8,
        g: u8,
        b: u8,
    );
    fn clear(&mut self);
}

pub fn render<R: Renderer>(
    w: &WorldState,
    display: (Coordinate<u8>, Size<u8>),
    viewport: Coordinate<u16>,
    board: &Board,
    robots: &[Robot],
    renderer: &mut R,
    is_title_screen: bool,
) {
    let charset = &w.charset;
    let palette = &w.palette;
    let num_colors = palette.colors.len() as u8;

    renderer.clear();

    let mut empty_overlay = vec![];
    let overlay = match board.overlay {
        Some((OverlayMode::Static, ref data)) |
        Some((OverlayMode::Normal, ref data)) => data,
        _ => {
            empty_overlay.reserve(board.width * board.height);
            for _ in 0..(board.width * board.height) {
                empty_overlay.push((32, 0x07));
            }
            &empty_overlay
        }
    };

    let level = Itertools::flatten(
        board
            .level
            .chunks(board.width) // per-row
            .skip(viewport.1 as usize) // ignore rows outside of viewport
            .take((display.1).1 as usize) // ignore rows outside of viewport
            .map(|row| row.iter().skip(viewport.0 as usize).take((display.1).0 as usize))
    );

    let under = Itertools::flatten(
        board
            .under
            .chunks(board.width) // per-row
            .skip(viewport.1 as usize) // ignore rows outside of viewport
            .take((display.1).1 as usize) // ignore rows outside of viewport
            .map(|row| row.iter().skip(viewport.0 as usize).take((display.1).0 as usize))
    );

    let is_static = board.overlay.as_ref().map_or(false, |(o, _)| *o == OverlayMode::Static);
    let overlay_viewport = if is_static {
        Coordinate(0, 0)
    } else {
        viewport
    };

    let overlay = Itertools::flatten(
        overlay
            .chunks(board.width) // per-row
            .skip(overlay_viewport.1 as usize) // ignore pre-viewport
            .take((display.1).1 as usize) // only iterate rows in viewport
            .map(|row| row.iter().skip(overlay_viewport.0 as usize).take((display.1).0 as usize))
    );

    for (pos, (level, under, overlay)) in Zip::new((level, under, overlay)).enumerate()
    {
        let &(id, color, param) = level;
        let &(_under_id, under_color, _under_param) = under;
        let &(overlay_char, overlay_color) = overlay;

        let xpos = pos as u16 % (display.1).0 as u16;
        let ypos = pos as u16 / (display.1).0 as u16;

        let mut color = match Thing::from_u8(id).unwrap() {
            Thing::Player => if is_title_screen {
                0
            } else {
                w.char_id(CharId::PlayerColor)
            },
            Thing::Fire => w.char_id_offset(CharId::FireColor1, param),
            Thing::Explosion => w.char_id_offset(
                CharId::ExplosionStage1,
                Explosion::from_param(param).stage
            ),
            _ => color,
        };

        let overlay_visible = overlay_char != b' ';
        let overlay_see_through = overlay_color / num_colors == 0 && overlay_color != 0x00;
        let ch = if !overlay_visible {
            let board_x = viewport.0 + xpos;
            let board_y = viewport.1 + ypos;
            char_from_id(
                id,
                param,
                &robots,
                &board.sensors,
                &w.idchars,
                board_x.checked_sub(1).map(|x| board.thing_at(&Coordinate(x, board_y))),
                if (board_x as usize) < board.width - 1 { Some(board.thing_at(&Coordinate(board_x + 1, board_y))) } else { None },
                board_y.checked_sub(1).map(|y| board.thing_at(&Coordinate(board_x, y))),
                if (board_y as usize) < board.height - 1 { Some(board.thing_at(&Coordinate(board_x, board_y + 1))) } else { None },
            )
        } else {
            overlay_char
        };
        if color / num_colors == 0 {
            color = under_color / num_colors * num_colors + color;
        }
        if overlay_visible {
            if overlay_see_through {
                color = color / num_colors * num_colors + overlay_color;
            } else {
                color = overlay_color;
            }
        }
        draw_char(
            ch,
            color % num_colors,
            color / num_colors,
            ((display.0).0 as u16 + xpos) as usize,
            ((display.0).1 as u16 + ypos) as usize,
            charset,
            palette,
            renderer
        );
    }

    const WINDOW_SIZE: usize = 80;
    if board.remaining_message_cycles > 0 {
        let message_len = board.message_line.text_len() + if w.message_edge { 2 } else { 0 };
        let mut msg_x = board.message_col.unwrap_or_else(|| if message_len < WINDOW_SIZE {
            (WINDOW_SIZE - message_len) / 2
        } else {
            0
        } as u8);

        if w.message_edge {
            draw_char(
                b' ',
                0x00,
                0x00,
                msg_x as usize,
                board.message_row as usize,
                charset,
                palette,
                renderer
            );
            msg_x += 1;
        }
        for (chars, bg, fg) in board.message_line.color_text() {
            for &c in chars {
                draw_char(
                    c,
                    fg.unwrap_or(w.message_color),
                    bg.unwrap_or(0x00),
                    msg_x as usize,
                    board.message_row as usize,
                    charset,
                    palette,
                    renderer,
                );
                msg_x += 1;
            }
        }
        if w.message_edge {
            draw_char(
                b' ',
                0x00,
                0x00,
                msg_x as usize,
                board.message_row as usize,
                charset,
                palette,
                renderer
            );
        }
    }
}

fn draw_char<R: Renderer>(
    ch: u8,
    fg_color: u8,
    bg_color: u8,
    x: usize,
    y: usize,
    charset: &Charset,
    palette: &Palette,
    renderer: &mut R
) {
    let char_bytes = charset.nth(ch);
    for (y_off, byte) in char_bytes.iter().enumerate() {
        for bit in 1..9 {
            let &(ref color, ref intensity) = if byte & (1 << (bit - 1)) != 0 {
                &palette.colors[fg_color as usize]
            } else {
                &palette.colors[bg_color as usize]
            };
            renderer.put_pixel(
                (x + 1) * 8 - bit,
                y * 14 + y_off,
                ((color.r * 4) as f32 * intensity) as u8,
                ((color.g * 4) as f32 * intensity) as u8,
                ((color.b * 4) as f32 * intensity) as u8,
            );
        }
    }
}

fn char_from_id(
    id: u8,
    param: u8,
    robots: &[Robot],
    sensors: &[Sensor],
    idchars: &[u8],
    left: Option<Thing>,
    right: Option<Thing>,
    top: Option<Thing>,
    bottom: Option<Thing>,
) -> u8 {
    let thing = Thing::from_u8(id).expect("invalid thing");
    match thing {
        Thing::Space => b' ',
        Thing::Normal => 178,
        Thing::Solid => 219,
        Thing::Tree => 6,
        Thing::Line | Thing::ThickWeb => {
            let (has_left, has_right, has_top, has_bottom) = if thing == Thing::ThickWeb {
                let check = |t| t != Thing::Space;
                (
                    left.map_or(true, check),
                    right.map_or(true, check),
                    top.map_or(true, check),
                    bottom.map_or(true, check),
                )
            } else {
                let check = |t| t == Thing::Line;
                (
                    left.map_or(true, check),
                    right.map_or(true, check),
                    top.map_or(true, check),
                    bottom.map_or(true, check),
                )
            };
            match (has_left, has_right, has_top, has_bottom) {
                (false, false, false, false) => 249,
                (_, _, false, false) => 205,
                (false, false, _, _) => 186,
                (true, false, true, false) => 188,
                (false, true, true, false) => 200,
                (true, false, false, true) => 187,
                (false, true, false, true) => 201,
                (true, false, true, true) => 185,
                (false, true, true, true) => 204,
                (true, true, true, false) => 202,
                (true, true, false, true) => 203,
                (true, true, true, true) => 206,
            }
        }
        Thing::Web => {
            let check = |t| t != Thing::Space;
            let has_left = left.map_or(true, check);
            let has_right = right.map_or(true, check);
            let has_top = top.map_or(true, check);
            let has_bottom = bottom.map_or(true, check);
            match (has_left, has_right, has_top, has_bottom) {
                (false, false, false, false) => 249,
                (_, _, false, false) => 196,
                (false, false, _, _) => 179,
                (true, false, true, false) => 217,
                (false, true, true, false) => 192,
                (true, false, false, true) => 191,
                (false, true, false, true) => 218,
                (true, false, true, true) => 180,
                (false, true, true, true) => 195,
                (true, true, true, false) => 193,
                (true, true, false, true) => 194,
                (true, true, true, true) => 197,
            }
        }
        Thing::CustomBlock => param,
        Thing::Breakaway => 177,
        Thing::CustomBreak => param,
        Thing::Boulder => 233,
        Thing::Crate => 254,
        Thing::CustomPush => param,
        Thing::Box => 254,
        Thing::CustomBox => param,
        Thing::Fake => 178,
        Thing::Carpet => 177,
        Thing::Floor => 176,
        Thing::Tiles => 254,
        Thing::CustomFloor => param,
        Thing::StillWater => 176,
        Thing::NWater => 24,
        Thing::SWater => 25,
        Thing::EWater => 26,
        Thing::WWater => 27,


        Thing::Chest => 160,
        Thing::Gem => 4,
        Thing::MagicGem => 4,
        Thing::Health => 3,
        Thing::Ring => 9,
        Thing::Potion => 150,
        Thing::Energizer => 7,
        Thing::Goop => 176,

        Thing::Bomb => 11,

        Thing::Explosion => 177,
        Thing::Key => 12,
        Thing::Lock => 10,


        Thing::Stairs => 240,
        Thing::Cave => 239,


        Thing::Gate => 22,
        Thing::OpenGate => 95,

        Thing::Coin => 7,
        Thing::NMovingWall => param,
        Thing::SMovingWall => param,
        Thing::EMovingWall => param,
        Thing::WMovingWall => param,
        Thing::Pouch => 229,

        Thing::SliderNS => 18,
        Thing::SliderEW => 29,

        Thing::LazerGun => 206,



        Thing::Fire => idchars[CharId::FireAnim1.to_usize().unwrap() + param as usize],
        Thing::Forest => 178,

        Thing::Whirlpool1 => 54,
        Thing::Whirlpool2 => 64,
        Thing::Whirlpool3 => 57,
        Thing::Whirlpool4 => 149,
        Thing::InvisibleWall => b' ',

        Thing::Ricochet => 42,


        Thing::CustomHurt => param,
        Thing::Text => param,


        Thing::Snake => 235,
        Thing::Eye => 236,
        Thing::Thief => 1,
        Thing::SlimeBlob => 42,
        Thing::Runner => 2,
        Thing::Ghost => 234,
        Thing::Dragon => 21,
        Thing::Fish => 224,
        Thing::Shark => 94,
        Thing::Spider => 15,
        Thing::Goblin => 5,
        Thing::SpittingTiger => 227,


        Thing::Bear => 153,
        Thing::BearCub => 148,

        Thing::Sensor => sensors[param as usize - 1].ch,
        Thing::RobotPushable | Thing::Robot => robots[param as usize - 1].ch,
        Thing::Sign => 226,
        Thing::Scroll => 232,
        Thing::Player => idchars[CharId::PlayerSouth.to_usize().unwrap()], //FIXME: use current playerdir

        _ => b'!',
    }
}
