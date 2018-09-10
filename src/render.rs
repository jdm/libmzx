use itertools::Zip;
use num_traits::FromPrimitive;

use super::{WorldState, Board, Robot, Charset, Palette, Sensor, Thing, OverlayMode};

pub trait Renderer {
    fn put_pixel(
        &mut self,
        x: usize,
        y: usize,
        r: u8,
        g: u8,
        b: u8,
    );
}

pub fn render<R: Renderer>(
    w: &WorldState,
    board: &Board,
    robots: &[Robot],
    renderer: &mut R
) {
    let charset = &w.charset;
    let palette = &w.palette;
    let num_colors = palette.colors.len() as u8;

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

    for (pos, (&(id, mut color, param),
               &(_under_id, under_color, _under_param),
               &(overlay_char, overlay_color)))
        in Zip::new((&board.level, &board.under, overlay)).enumerate()
    {
        let overlay_visible = overlay_char != b' ';
        let overlay_see_through = overlay_color / num_colors == 0 && overlay_color != 0x00;
        let ch = if !overlay_visible {
            char_from_id(id, param, &robots, &board.sensors)
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
            pos % board.width,
            pos / board.width,
            charset,
            palette,
            renderer
        );
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
            let color = if byte & (1 << (bit - 1)) != 0 {
                &palette.colors[fg_color as usize]
            } else {
                &palette.colors[bg_color as usize]
            };
            renderer.put_pixel(
                (x + 1) * 8 - bit,
                y * 14 + y_off,
                color.r * 4,
                color.g * 4,
                color.b * 4,
            );
        }
    }
}

fn char_from_id(id: u8, param: u8, robots: &[Robot], sensors: &[Sensor]) -> u8 {
    match Thing::from_u8(id).expect("invalid thing") {
        Thing::Space => b' ',
        Thing::Normal => 178,
        Thing::Solid => 219,
        Thing::Tree => 6,

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
        Thing::Player => 0x02,

        _ => b'!',
    }
}
