#![feature(slice_patterns)]

extern crate byteorder;
extern crate itertools;

use byteorder::{ByteOrder, LittleEndian};
use itertools::Zip;
use std::str;

pub struct World {
    pub title: String,
    pub charset: Charset,
    pub palette: Palette,
    pub boards: Vec<Board>,
}

pub struct Board {
    pub title: String,
    pub width: usize,
    pub height: usize,
    pub overlay: Option<(OverlayMode, Vec<(u8, u8)>)>,
    pub level: Vec<(u8, u8, u8)>,
    pub under: Vec<(u8, u8, u8)>,
    pub mod_file: String,
    pub upper_left_viewport: (u8, u8),
    pub viewport_size: (u8, u8),
    pub can_shoot: bool,
    pub can_bomb: bool,
    pub fire_burns_brown: bool,
    pub fire_burns_space: bool,
    pub fire_burns_fakes: bool,
    pub fire_burns_trees: bool,
    pub explosion_result: ExplosionResult,
    pub save_restriction: SaveRestriction,
    pub collect_bombs: bool,
    pub fire_burns_forever: bool,
    pub exits: (Option<u8>, Option<u8>, Option<u8>, Option<u8>),
    pub restart_when_zapped: bool,
    pub time_limit: u16,
}

pub enum OverlayMode {
    Normal,
    Static,
    Transparent,
}

pub enum ExplosionResult {
    Nothing,
    Ash,
    Fire,
}

pub enum SaveRestriction {
    NoSave,
    OnlyOnSensor,
    Unrestricted,
}

const CHARSET_BUFFER_SIZE: usize = 14 * 256;

pub struct Charset {
    data: [u8; CHARSET_BUFFER_SIZE],
}

impl Charset {
    pub fn nth(&self, n: u8) -> &[u8] {
        &self.data[(n as usize * 14)..((n+1) as usize * 14)]
    }
}

pub struct Color {
    pub r: u8,
    pub g: u8,
    pub b: u8,
}

pub struct Palette {
    pub colors: Vec<Color>,
}

#[derive(Debug)]
pub enum WorldError<'a> {
    NoNullInTitle,
    InvalidUTF8Title,
    Protected,
    UnrecognizedVersion(&'a [u8]),
    CharsetTooSmall,
    UnhandledSFX,
    TooManyBoards(u8),
    Board(BoardError),
}

#[derive(Debug)]
pub enum BoardError {
    InvalidUTF8Title,
    NoNullInTitle,
    UnexpectedSize(u8),
    UnknownOverlayMode(u8),
}

impl<'a> From<BoardError> for WorldError<'a> {
    fn from(err: BoardError) -> WorldError<'a> {
        WorldError::Board(err)
    }
}

fn decode_runs(buffer: &[u8]) -> (Vec<u8>, &[u8], usize, usize) {
    let mut consumed = 0;
    let mut result = vec![];
    let mut num_bytes: Option<u8> = None;

    let (max_w, buffer) = buffer.split_at(2);
    let (max_h, buffer) = buffer.split_at(2);
    let max_w = LittleEndian::read_u16(max_w) as usize;
    let max_h = LittleEndian::read_u16(max_h) as usize;
    let max = max_w * max_h;

    while result.len() < max {
        let byte = buffer[consumed];
        consumed += 1;
        if num_bytes.is_none() && byte & 0x80 == 0 {
            num_bytes = Some(1);
        }
        if let Some(bytes) = num_bytes {
            for _ in 0..bytes as usize {
                result.push(byte);
            }
            num_bytes = None;
        } else {
            num_bytes = Some(byte & 0x7F);
        }
    }

    assert_eq!(result.len(), max);
    (result, &buffer[consumed..], max_w, max_h)
}

fn load_board(title: String, buffer: &[u8]) -> Result<Board, BoardError> {
    let (sizing, mut buffer) = buffer.split_at(1);
    let (_width, _height) = match sizing[0] {
        0 => (60, 166),
        1 => (80, 125),
        2 => (100, 100),
        3 => (200, 50),
        4 => (400, 25),
        _ => return Err(BoardError::UnexpectedSize(sizing[0])),
    };

    let overlay = if buffer[0] == 0 {
        let (overlay_mode, new_buffer) = buffer[1..].split_at(1);
        let overlay_mode = match overlay_mode[0] {
            1 => OverlayMode::Normal,
            2 => OverlayMode::Static,
            3 => OverlayMode::Transparent,
            c => return Err(BoardError::UnknownOverlayMode(c)),
        };
        let (chars, new_buffer, _, _) = decode_runs(new_buffer);
        let (colors, new_buffer, _, _) = decode_runs(new_buffer);
        buffer = new_buffer;
        Some((overlay_mode, Zip::new((chars.into_iter(), colors.into_iter())).collect()))
    } else {
        None
    };

    let (ids, buffer, width, height) = decode_runs(buffer);
    let (colors, buffer, _, _) = decode_runs(buffer);
    let (params, buffer, _, _) = decode_runs(buffer);
    assert_eq!(ids.len(), colors.len());
    assert_eq!(ids.len(), params.len());

    let (under_ids, buffer, _, _) = decode_runs(buffer);
    let (under_colors, buffer, _, _) = decode_runs(buffer);
    let (under_params, _buffer, _, _) = decode_runs(buffer);
    assert_eq!(under_ids.len(), ids.len());
    assert_eq!(under_ids.len(), under_colors.len());
    assert_eq!(under_ids.len(), under_params.len());

    Ok(Board {
        title: title.into(),
        width: width,
        height: height,
        overlay: overlay,
        level: Zip::new((ids.into_iter(), colors.into_iter(), params.into_iter())).collect(),
        under: Zip::new((under_ids.into_iter(), under_colors.into_iter(), under_params.into_iter())).collect(),
        mod_file: "".into(),
        upper_left_viewport: (0, 0),
        viewport_size: (0, 0),
        can_shoot: false,
        can_bomb: false,
        fire_burns_brown: false,
        fire_burns_space: false,
        fire_burns_fakes: false,
        fire_burns_trees: false,
        explosion_result: ExplosionResult::Nothing,
        save_restriction: SaveRestriction::NoSave,
        collect_bombs: false,
        fire_burns_forever: false,
        exits: (None, None, None, None),
        restart_when_zapped: false,
        time_limit: 0,
    })
}

fn is_bool(byte: &[u8]) -> bool {
    byte.len() == 1 && (byte[0] == 0 || byte[0] == 1)
}

pub fn load_world<'a>(buffer: &'a [u8]) -> Result<World, WorldError<'a>> {
    let original_buffer = buffer;

    let (title, buffer) = buffer.split_at(25);
    let title_end = title.iter().position(|b| *b == 0);
    let title = match title_end {
        Some(idx) => try!(str::from_utf8(&title[0..idx]).map_err(|_| WorldError::InvalidUTF8Title)),
        None => return Err(WorldError::NoNullInTitle),
    };

    let (protection, buffer) = buffer.split_at(1);
    assert!(is_bool(protection));
    if protection != &[0] {
        return Err(WorldError::Protected);
    }

    let (_signature, buffer) = buffer.split_at(3);
    /*match signature {
        [b'M', b'Z', b'X'] | [b'M', b'Z', b'2'] => (),
        _ => return Err(WorldError::UnrecognizedVersion(signature)),
    }*/

    let (charset_data, buffer) = buffer.split_at(14 * 256);
    if charset_data.len() < CHARSET_BUFFER_SIZE {
        return Err(WorldError::CharsetTooSmall);
    }
    let mut charset = Charset { data: [0; CHARSET_BUFFER_SIZE] };
    charset.data.copy_from_slice(charset_data);

    let (_idchars, buffer) = buffer.split_at(455);

    let (_status_counters, buffer) = buffer.split_at(6 * 15);

    let (_edge_border, buffer) = buffer.split_at(1);
    let (_starting_board_number, buffer) = buffer.split_at(1);
    let (_end_game_board, buffer) = buffer.split_at(1);
    let (_death_board, buffer) = buffer.split_at(1);
    let (_end_game_x, buffer) = buffer.split_at(2);
    let (_end_game_y, buffer) = buffer.split_at(2);
    let (game_over_sfx, buffer) = buffer.split_at(1);
    assert!(is_bool(game_over_sfx));
    let (_death_x, buffer) = buffer.split_at(2);
    let (_death_y, buffer) = buffer.split_at(2);
    let (_starting_lives, buffer) = buffer.split_at(2);
    let (_limit_lives, buffer) = buffer.split_at(2);
    let (_starting_health, buffer) = buffer.split_at(2);
    let (_limit_health, buffer) = buffer.split_at(2);
    let (enemies_hurt_enemies, buffer) = buffer.split_at(1);
    assert!(is_bool(enemies_hurt_enemies));
    let (clear_messages_and_projectiles, buffer) = buffer.split_at(1);
    assert!(is_bool(clear_messages_and_projectiles));
    let (only_play_via_swap_world, buffer) = buffer.split_at(1);
    assert!(is_bool(only_play_via_swap_world));

    let (palette_color_data, buffer) = buffer.split_at(16 * 3);
    let mut colors = vec![];
    for rgb in palette_color_data.chunks(3) {
        assert!(rgb.iter().all(|&b| b < 64u8));
        colors.push(Color { r: rgb[0], g: rgb[1], b: rgb[2] });
    }
    assert_eq!(colors.len(), 16);

    let (_global_robot_pos, buffer) = buffer.split_at(4);
    let (sfx, mut buffer) = buffer.split_at(1);
    let num_boards = if sfx[0] == 0 {
        let (len, new_buffer) = buffer.split_at(2);
        let (_sfx, new_buffer) = new_buffer.split_at(LittleEndian::read_u16(len) as usize);
        let (num_boards, new_buffer) = new_buffer.split_at(1);
        buffer = new_buffer;
        num_boards[0]
    } else {
        if sfx[0] > 150 {
            return Err(WorldError::TooManyBoards(sfx[0]));
        }
        sfx[0]
    };

    let mut titles = vec![];
    let mut boards = vec![];
    for _ in 0..num_boards {
        let (title, new_buffer) = buffer.split_at(25);
        let title_end = title.iter().position(|b| *b == 0);
        let title = match title_end {
            Some(idx) => try!(str::from_utf8(&title[0..idx]).map_err(|_| WorldError::InvalidUTF8Title)),
            None => return Err(WorldError::NoNullInTitle),
        };
        buffer = new_buffer;

        titles.push(title.to_owned());
    }

    for title in titles {
        let (byte_length, new_buffer) = buffer.split_at(4);
        let byte_length = LittleEndian::read_u32(byte_length) as usize;

        let (board_pos, new_buffer) = new_buffer.split_at(4);
        let board_pos = LittleEndian::read_u32(board_pos) as usize;

        if byte_length == 0 {
            continue;
        }

        let end_board_pos = board_pos + byte_length;
        let board = try!(load_board(title,
                                    &original_buffer[board_pos..end_board_pos]));
        boards.push(board);
        buffer = new_buffer;
    }

    Ok(World {
        title: title.into(),
        charset: charset,
        palette: Palette { colors: colors },
        boards: boards,
    })
}

#[cfg(test)]
mod tests {
    #[test]
    fn it_works() {
        use std::fs::File;
        use std::io::Read;
        use super::load_world;

        let mut f = File::open("BERNARD.MZX").unwrap();
        let mut v = vec![];
        f.read_to_end(&mut v).unwrap();
        let world = load_world(&v).unwrap();
        assert_eq!(world.title, "~d@8Bernard the Bard");
    }
}
