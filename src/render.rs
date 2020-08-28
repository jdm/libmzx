use itertools::{Itertools, Zip};
use num_traits::{FromPrimitive, ToPrimitive};

use super::{
    bullet_from_param, door_from_param, Board, BulletType, ByteString, CardinalDirection, CharId,
    Charset, Coordinate, DoorOrientation, Explosion, MessageBoxLineType, OverlayMode, Palette,
    Robot, Sensor, Size, Thing, WorldState,
};

pub trait Renderer {
    fn put_pixel(&mut self, x: usize, y: usize, r: u8, g: u8, b: u8);
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
        Some((OverlayMode::Static, ref data)) | Some((OverlayMode::Normal, ref data)) => data,
        _ => {
            empty_overlay.reserve(board.width * board.height);
            for _ in 0..(board.width * board.height) {
                empty_overlay.push((32, 0x07));
            }
            &empty_overlay
        }
    };

    //assert_ne!(board.width, 0);
    if board.width == 0 {
        return;
    }

    let level = Itertools::flatten(
        board
            .level
            .chunks(board.width) // per-row
            .skip(viewport.1 as usize) // ignore rows outside of viewport
            .take((display.1).1 as usize) // ignore rows outside of viewport
            .map(|row| {
                row.iter()
                    .skip(viewport.0 as usize)
                    .take((display.1).0 as usize)
            }),
    );

    let under = Itertools::flatten(
        board
            .under
            .chunks(board.width) // per-row
            .skip(viewport.1 as usize) // ignore rows outside of viewport
            .take((display.1).1 as usize) // ignore rows outside of viewport
            .map(|row| {
                row.iter()
                    .skip(viewport.0 as usize)
                    .take((display.1).0 as usize)
            }),
    );

    let is_static = board
        .overlay
        .as_ref()
        .map_or(false, |(o, _)| *o == OverlayMode::Static);
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
            .map(|row| {
                row.iter()
                    .skip(overlay_viewport.0 as usize)
                    .take((display.1).0 as usize)
            }),
    );

    for (pos, (level, under, overlay)) in Zip::new((level, under, overlay)).enumerate() {
        let &(id, color, param) = level;
        let &(_under_id, under_color, _under_param) = under;
        let &(overlay_char, overlay_color) = overlay;

        let xpos = pos as u16 % (display.1).0 as u16;
        let ypos = pos as u16 / (display.1).0 as u16;

        let mut color = match Thing::from_u8(id).unwrap() {
            Thing::Player => {
                if is_title_screen {
                    0
                } else {
                    w.char_id(CharId::PlayerColor)
                }
            }
            Thing::Fire => w.char_id_offset(CharId::FireColor1, param),
            Thing::Missile => w.char_id(CharId::MissileColor),
            Thing::Explosion => {
                w.char_id_offset(CharId::ExplosionStage1, Explosion::from_param(param).stage)
            }
            Thing::Bullet => match bullet_from_param(param).0 {
                BulletType::Player => w.char_id(CharId::PlayerBulletColor),
                BulletType::Enemy => w.char_id(CharId::EnemyBulletColor),
                BulletType::Neutral => w.char_id(CharId::NeutralBulletColor),
            },
            Thing::Scroll => w.message_color,
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
                board_x
                    .checked_sub(1)
                    .map(|x| board.thing_at(&Coordinate(x, board_y))),
                if (board_x as usize) < board.width - 1 {
                    Some(board.thing_at(&Coordinate(board_x + 1, board_y)))
                } else {
                    None
                },
                board_y
                    .checked_sub(1)
                    .map(|y| board.thing_at(&Coordinate(board_x, y))),
                if (board_y as usize) < board.height - 1 {
                    Some(board.thing_at(&Coordinate(board_x, board_y + 1)))
                } else {
                    None
                },
                w.player_face_dir,
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
            renderer,
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
                renderer,
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
                renderer,
            );
        }
    }
}

pub enum MessageBoxLine {
    Text(ByteString, MessageBoxLineType),
    Option { label: ByteString, text: ByteString },
}

pub fn draw_messagebox<R: Renderer>(
    w: &WorldState,
    title: &ByteString,
    lines: &[MessageBoxLine],
    pos: usize,
    renderer: &mut R,
) {
    let empty_title = ByteString("Interaction".to_owned().into_bytes());
    let title = if title.is_empty() {
        &empty_title
    } else {
        title
    };
    const DIALOG_Y: usize = 3;
    const DIALOG_X: usize = 5;
    const DIALOG_W: usize = 79 - DIALOG_X * 2;
    const DIALOG_H: usize = 25 - DIALOG_Y * 2;
    const CONTENT_H: usize = DIALOG_H - 6;

    let mut y = DIALOG_Y;
    draw_char(
        0xDA, 0x00, 0x08, DIALOG_X, y, &w.charset, &w.palette, renderer,
    ); // .-
    for x in 1..DIALOG_W {
        draw_char(
            0xC4,
            0x00,
            0x08,
            DIALOG_X + x,
            y,
            &w.charset,
            &w.palette,
            renderer,
        ); // -
    }
    draw_char(
        0xBF,
        0x07,
        0x08,
        DIALOG_X + DIALOG_W,
        y,
        &w.charset,
        &w.palette,
        renderer,
    ); // -.

    y += 1;
    draw_char(
        0xB3, 0x00, 0x08, DIALOG_X, y, &w.charset, &w.palette, renderer,
    ); // |
    for x in 1..DIALOG_W {
        draw_char(
            0x20,
            0x00,
            0x08,
            DIALOG_X + x,
            y,
            &w.charset,
            &w.palette,
            renderer,
        ); // ' '
    }
    draw_char(
        0xB3,
        0x0F,
        0x08,
        DIALOG_X + DIALOG_W,
        y,
        &w.charset,
        &w.palette,
        renderer,
    ); // |
    for (x, c) in title.iter().enumerate() {
        draw_char(
            *c,
            0x0F,
            0x08,
            DIALOG_X + (DIALOG_W - title.len()) / 2 + x,
            y,
            &w.charset,
            &w.palette,
            renderer,
        );
    }

    y += 1;
    draw_char(
        0xDA, 0x0F, 0x08, DIALOG_X, y, &w.charset, &w.palette, renderer,
    ); // .-
    for x in 1..DIALOG_W {
        draw_char(
            0xC4,
            0x0F,
            0x08,
            DIALOG_X + x,
            y,
            &w.charset,
            &w.palette,
            renderer,
        ); // -
    }
    draw_char(
        0xD9,
        0x0F,
        0x08,
        DIALOG_X + DIALOG_W,
        y,
        &w.charset,
        &w.palette,
        renderer,
    ); // -/

    y += 1;

    for y_off in 0..CONTENT_H {
        draw_char(
            0xB3,
            0x0F,
            0x08,
            DIALOG_X,
            y + y_off,
            &w.charset,
            &w.palette,
            renderer,
        ); // |
        for x in 1..DIALOG_W {
            draw_char(
                0x20,
                0x00,
                0x08,
                DIALOG_X + x,
                y + y_off,
                &w.charset,
                &w.palette,
                renderer,
            );
        }
        if y_off == CONTENT_H / 2 {
            draw_char(
                0x10,
                0x00,
                0x08,
                DIALOG_X + 1,
                y + y_off,
                &w.charset,
                &w.palette,
                renderer,
            ); // >
            draw_char(
                0x11,
                0x00,
                0x08,
                DIALOG_X + DIALOG_W - 1,
                y + y_off,
                &w.charset,
                &w.palette,
                renderer,
            ); // <
        }
        draw_char(
            0xB3,
            0x00,
            0x08,
            DIALOG_X + DIALOG_W,
            y + y_off,
            &w.charset,
            &w.palette,
            renderer,
        ); // |
    }

    y += CONTENT_H;
    draw_char(
        0xDA, 0x00, 0x08, DIALOG_X, y, &w.charset, &w.palette, renderer,
    ); // .-
    for x in 1..DIALOG_W {
        draw_char(
            0xC4,
            0x00,
            0x08,
            DIALOG_X + x,
            y,
            &w.charset,
            &w.palette,
            renderer,
        ); // -
    }
    draw_char(
        0xD9,
        0x00,
        0x08,
        DIALOG_X + DIALOG_W,
        y,
        &w.charset,
        &w.palette,
        renderer,
    ); // -/

    y += 1;
    draw_char(
        0xB3, 0x00, 0x08, DIALOG_X, y, &w.charset, &w.palette, renderer,
    ); // |
    for x in 1..DIALOG_W {
        draw_char(
            0x20,
            0x00,
            0x08,
            DIALOG_X + x,
            y,
            &w.charset,
            &w.palette,
            renderer,
        ); // -
    }
    draw_char(
        0xB3,
        0x0F,
        0x08,
        DIALOG_X + DIALOG_W,
        y,
        &w.charset,
        &w.palette,
        renderer,
    ); // |

    y += 1;
    draw_char(
        0xC0, 0x07, 0x08, DIALOG_X, y, &w.charset, &w.palette, renderer,
    ); // \-
    for x in 1..DIALOG_W {
        draw_char(
            0xC4,
            0x0F,
            0x08,
            DIALOG_X + x,
            y,
            &w.charset,
            &w.palette,
            renderer,
        ); // -
    }
    draw_char(
        0xD9,
        0x0F,
        0x08,
        DIALOG_X + DIALOG_W,
        y,
        &w.charset,
        &w.palette,
        renderer,
    ); // -/

    let start = (pos as isize - CONTENT_H as isize / 2).max(0) as usize;
    let end = (start + (CONTENT_H / 2 + pos + 1).min(CONTENT_H)).min(lines.len());
    let y_start = DIALOG_Y + 3 + CONTENT_H / 2 - pos.min(CONTENT_H / 2);

    for (y_off, line) in lines[start..end].iter().enumerate() {
        // FIXME: limit maximum line characters
        const START_X: usize = DIALOG_X + 3;
        const LIMIT: usize = DIALOG_W - 4;
        match line {
            MessageBoxLine::Text(ref s, MessageBoxLineType::Plain) => {
                for (x, ch) in s.iter().enumerate() {
                    draw_char(
                        *ch,
                        0x0F,
                        0x08,
                        START_X + x,
                        y_start + y_off,
                        &w.charset,
                        &w.palette,
                        renderer,
                    );
                }
            }
            MessageBoxLine::Text(ref s, MessageBoxLineType::Color) => draw_fancy_message_box_line(
                s,
                false,
                false,
                START_X,
                y_start + y_off,
                LIMIT,
                &w.charset,
                &w.palette,
                renderer,
            ),
            MessageBoxLine::Text(ref s, MessageBoxLineType::Center) => draw_fancy_message_box_line(
                s,
                true,
                false,
                START_X,
                y_start + y_off,
                LIMIT,
                &w.charset,
                &w.palette,
                renderer,
            ),
            MessageBoxLine::Option { ref text, .. } => draw_fancy_message_box_line(
                text,
                false,
                true,
                START_X,
                y_start + y_off,
                LIMIT,
                &w.charset,
                &w.palette,
                renderer,
            ),
        };
    }
}

fn draw_fancy_message_box_line<R: Renderer>(
    text: &ByteString,
    center: bool,
    option: bool,
    x: usize,
    y: usize,
    limit: usize,
    charset: &Charset,
    palette: &Palette,
    renderer: &mut R,
) {
    let mut x_off = if option { 2 } else { 0 };
    if option {
        draw_char(0x10, 0x0E, 0x08, x, y, charset, palette, renderer);
    }

    if center {
        let len = text
            .color_text()
            .fold(0, |acc, (chars, _bg, _fg)| acc + chars.len());
        x_off += (limit - len) / 2;
    }

    for (chars, bg, fg) in text.color_text() {
        for &c in chars {
            draw_char(
                c,
                fg.unwrap_or(0x0F),
                bg.unwrap_or(0x08),
                x + x_off,
                y,
                charset,
                palette,
                renderer,
            );
            x_off += 1;
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
    renderer: &mut R,
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
    player_face_dir: i32,
) -> u8 {
    let thing = Thing::from_u8(id).expect("invalid thing");
    let char_id = match thing {
        Thing::Space => CharId::Space,
        Thing::Normal => CharId::Normal,
        Thing::Solid => CharId::Solid,
        Thing::Tree => CharId::Tree,
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
            return match (has_left, has_right, has_top, has_bottom) {
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
            };
        }
        Thing::Web => {
            let check = |t| t != Thing::Space;
            let has_left = left.map_or(true, check);
            let has_right = right.map_or(true, check);
            let has_top = top.map_or(true, check);
            let has_bottom = bottom.map_or(true, check);
            return match (has_left, has_right, has_top, has_bottom) {
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
            };
        }
        Thing::CustomBlock => return param,
        Thing::Breakaway => CharId::Breakaway,
        Thing::CustomBreak => return param,
        Thing::Boulder => CharId::Boulder,
        Thing::Crate => CharId::Crate,
        Thing::CustomPush => return param,
        Thing::Box => CharId::Box,
        Thing::CustomBox => return param,
        Thing::Fake => CharId::Fake,
        Thing::Carpet => CharId::Carpet,
        Thing::Floor => CharId::Floor,
        Thing::Tiles => CharId::Tiles,
        Thing::CustomFloor => return param,
        Thing::StillWater => CharId::StillWater,
        Thing::NWater => CharId::NorthWater,
        Thing::SWater => CharId::SouthWater,
        Thing::EWater => CharId::EastWater,
        Thing::WWater => CharId::WestWater,
        Thing::Ice => return idchars[CharId::BlankIce.to_usize().unwrap() + param as usize],
        Thing::Lava => CharId::LavaAnim1, //TODO: lava animation,
        Thing::Chest => CharId::Chest,
        Thing::Gem => CharId::Gem,
        Thing::MagicGem => CharId::MagicGem,
        Thing::Health => CharId::Health,
        Thing::Ring => CharId::Ring,
        Thing::Potion => CharId::Potion,
        Thing::Energizer => CharId::Energizer,
        Thing::Goop => CharId::Goop,
        Thing::Ammo => {
            if param < 10 {
                CharId::SmallAmmo
            } else {
                CharId::LargeAmmo
            }
        }
        Thing::Bomb => CharId::Bomb,
        Thing::LitBomb => CharId::LitBombAnim1, // TODO: lit bomb animation
        Thing::Explosion => CharId::Explosion,
        Thing::Key => CharId::Key,
        Thing::Lock => CharId::Lock,
        Thing::Door => {
            let (orientation, _dir, _status) = door_from_param(param);
            match orientation {
                DoorOrientation::Horizontal => CharId::HorizontalDoor,
                DoorOrientation::Vertical => CharId::VerticalDoor,
            }
        }
        Thing::OpenDoor => {
            return idchars[CharId::OpenDoorStart.to_usize().unwrap() + (param & 0x1F) as usize];
        }
        Thing::Stairs => CharId::Stairs,
        Thing::Cave => CharId::Cave,
        Thing::CWRotate => CharId::CwAnim1,   //TODO animate
        Thing::CCWRotate => CharId::CcwAnim1, //TODO animate
        Thing::Gate => CharId::Gate,
        Thing::OpenGate => CharId::OpenGate,
        Thing::Transport => match param {
            //TODO animate
            0 => CharId::NTransportAnim1,
            1 => CharId::STransportAnim1,
            2 => CharId::ETransportAnim1,
            3 => CharId::WTransportAnim1,
            4 => CharId::AnyTransportAnim1,
            _ => unreachable!("unexpected transport param: {}", param),
        },
        Thing::Coin => CharId::Coin,
        Thing::NMovingWall => return param,
        Thing::SMovingWall => return param,
        Thing::EMovingWall => return param,
        Thing::WMovingWall => return param,
        Thing::Pouch => CharId::Pouch,
        Thing::Pusher | Thing::Missile | Thing::Spike => match param {
            0 => CharId::NThickArrow,
            1 => CharId::SThickArrow,
            2 => CharId::EThickArrow,
            3 => CharId::WThickArrow,
            _ => unreachable!("unexpected param for {:?}: {}", thing, param),
        },
        Thing::SliderNS => CharId::SliderNS,
        Thing::SliderEW => CharId::SliderEW,
        Thing::Lazer => CharId::HorizontalLazerAnim1, //TODO: differentiate horizontal/vertical
        Thing::LazerGun => CharId::LazerGun,
        Thing::Bullet => match bullet_from_param(param) {
            (BulletType::Player, CardinalDirection::North) => CharId::NPlayerBullet,
            (BulletType::Player, CardinalDirection::South) => CharId::SPlayerBullet,
            (BulletType::Player, CardinalDirection::East) => CharId::EPlayerBullet,
            (BulletType::Player, CardinalDirection::West) => CharId::WPlayerBullet,
            (BulletType::Enemy, CardinalDirection::North) => CharId::NEnemyBullet,
            (BulletType::Enemy, CardinalDirection::South) => CharId::SEnemyBullet,
            (BulletType::Enemy, CardinalDirection::East) => CharId::EEnemyBullet,
            (BulletType::Enemy, CardinalDirection::West) => CharId::WEnemyBullet,
            (BulletType::Neutral, CardinalDirection::North) => CharId::NNeutralBullet,
            (BulletType::Neutral, CardinalDirection::South) => CharId::SNeutralBullet,
            (BulletType::Neutral, CardinalDirection::East) => CharId::ENeutralBullet,
            (BulletType::Neutral, CardinalDirection::West) => CharId::WNeutralBullet,
        },
        Thing::Fire => return idchars[CharId::FireAnim1.to_usize().unwrap() + param as usize],
        Thing::Forest => CharId::Forest,
        Thing::Life => CharId::LifeAnim1, //TODO animate
        Thing::Whirlpool1 => CharId::Whirlpool1,
        Thing::Whirlpool2 => CharId::Whirlpool2,
        Thing::Whirlpool3 => CharId::Whirlpool3,
        Thing::Whirlpool4 => CharId::Whirlpool4,
        Thing::InvisibleWall => CharId::InvisibleWall,
        Thing::RicochetPanel => match param {
            0 => CharId::RicochetPanel1,
            1 => CharId::RicochetPanel2,
            _ => unreachable!("unexpected ricochet panel param: {}", param),
        },
        Thing::Ricochet => CharId::Ricochet,
        Thing::Mine => CharId::MineAnim1, //TODO animate
        Thing::CustomHurt => return param,
        Thing::Text => return param,
        Thing::ShootingFire => CharId::SpitFireAnim1, //TODO animate
        Thing::Seeker => CharId::SeekerAnim1,         //TODO animate
        Thing::Snake => CharId::Snake,
        Thing::Eye => CharId::Eye,
        Thing::Thief => CharId::Thief,
        Thing::SlimeBlob => CharId::SlimeBlob,
        Thing::Runner => CharId::Runner,
        Thing::Ghost => CharId::Ghost,
        Thing::Dragon => CharId::Dragon,
        Thing::Fish => CharId::Fish,
        Thing::Shark => CharId::Shark,
        Thing::Spider => CharId::Spider,
        Thing::Goblin => CharId::Goblin,
        Thing::SpittingTiger => CharId::SpittingTiger,
        Thing::BulletGun => CharId::NThinArrow, //TODO differentiate
        Thing::SpinningGun => CharId::NThinArrow, //TODO differentiate
        Thing::Bear => CharId::Bear,
        Thing::BearCub => CharId::BearCub,
        Thing::MissileGun => CharId::NThickArrow, //TODO differentiate
        Thing::Sensor => return sensors[param as usize - 1].ch,
        Thing::RobotPushable | Thing::Robot => return robots[param as usize - 1].ch,
        Thing::Sign => CharId::Sign,
        Thing::Scroll => CharId::Scroll,
        Thing::Sprite => {
            warn!("no physical spites should exist");
            CharId::Space
        }
        Thing::SpriteCollision => {
            warn!("no physical sprite collisions should exist");
            CharId::Space
        }
        Thing::ImageFile => {
            warn!("no physical image files should exist");
            CharId::Space
        }
        Thing::NoId => {
            warn!("no physical noid objects should exist");
            CharId::Space
        }
        Thing::Player => match player_face_dir {
            0 => CharId::PlayerNorth,
            1 => CharId::PlayerSouth,
            2 => CharId::PlayerEast,
            _ => CharId::PlayerWest,
        },
    };
    idchars[char_id.to_usize().unwrap()]
}
