#![feature(slice_patterns)]

extern crate byteorder;
extern crate itertools;

use byteorder::{ByteOrder, LittleEndian};
use itertools::Zip;
use std::str;

pub struct BoardId(pub u8);
pub struct ColorValue(pub u8);
pub struct Coordinate(pub (u16, u16));

pub struct World {
    pub title: String,
    pub charset: Charset,
    pub palette: Palette,
    pub boards: Vec<Board>,
    pub edge_border: ColorValue,
    pub starting_board_number: BoardId,
    pub end_game_board: BoardId,
    pub death_board: BoardId,
    pub end_game_pos: Coordinate,
    pub game_over_sfx: bool,
    pub death_pos: Coordinate,
    pub starting_lives: u16,
    pub limit_lives: u16,
    pub starting_health: u16,
    pub limit_health: u16,
    pub enemies_hurt_enemies: bool,
    pub clear_messages_and_projectiles: bool,
    pub only_play_via_swap_world: bool,
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
        let n = n as usize;
        &self.data[(n * 14)..((n + 1) * 14)]
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
    InvalidModFile,
}

impl<'a> From<BoardError> for WorldError<'a> {
    fn from(err: BoardError) -> WorldError<'a> {
        WorldError::Board(err)
    }
}

fn get_null_terminated_string(buffer: &[u8], max_length: usize) -> Result<(String, &[u8]), ()> {
    let (s, buffer) = buffer.split_at(max_length);
    let end = s.iter().position(|b| *b == 0);
    match end {
        Some(idx) => Ok((try!(str::from_utf8(&s[0..idx]).map_err(|_| ())).into(), buffer)),
        None => Err(()),
    }
}

fn get_bool(buffer: &[u8]) -> (bool, &[u8]) {
    let (byte, buffer) = get_byte(buffer);
    assert!(byte == 0 || byte == 1);
    (byte == 1, buffer)
}

fn get_byte(buffer: &[u8]) -> (u8, &[u8]) {
    let (byte, buffer) = buffer.split_at(1);
    (byte[0], buffer)
}

fn get_word(buffer: &[u8]) -> (u16, &[u8]) {
    let (word, buffer) = buffer.split_at(2);
    (LittleEndian::read_u16(word), buffer)
}

fn get_dword(buffer: &[u8]) -> (u32, &[u8]) {
    let (dword, buffer) = buffer.split_at(4);
    (LittleEndian::read_u32(dword), buffer)
}

fn decode_runs(buffer: &[u8]) -> (Vec<u8>, &[u8], usize, usize) {
    let mut consumed = 0;
    let mut result = vec![];
    let mut num_bytes: Option<u8> = None;

    let (max_w, buffer) = get_word(buffer);
    let max_w = max_w as usize;
    let (max_h, buffer) = get_word(buffer);
    let max_h = max_h as usize;
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
    let (sizing, mut buffer) = get_byte(buffer);
    let (_width, _height) = match sizing {
        0 => (60, 166),
        1 => (80, 125),
        2 => (100, 100),
        3 => (200, 50),
        4 => (400, 25),
        _ => return Err(BoardError::UnexpectedSize(sizing)),
    };

    let overlay = if buffer[0] == 0 {
        let (overlay_mode, new_buffer) = get_byte(&buffer[1..]);
        let overlay_mode = match overlay_mode {
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
    let (under_params, buffer, _, _) = decode_runs(buffer);
    assert_eq!(under_ids.len(), ids.len());
    assert_eq!(under_ids.len(), under_colors.len());
    assert_eq!(under_ids.len(), under_params.len());

    let (mod_file, _buffer) = try!(get_null_terminated_string(buffer, 13).map_err(|_| BoardError::InvalidModFile));

    Ok(Board {
        title: title,
        width: width,
        height: height,
        overlay: overlay,
        level: Zip::new((ids.into_iter(), colors.into_iter(), params.into_iter())).collect(),
        under: Zip::new((under_ids.into_iter(), under_colors.into_iter(), under_params.into_iter())).collect(),
        mod_file: mod_file,
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

pub fn load_world<'a>(buffer: &'a [u8]) -> Result<World, WorldError<'a>> {
    let original_buffer = buffer;

    let (title, buffer) = try!(get_null_terminated_string(buffer, 25).map_err(|_| WorldError::InvalidUTF8Title));

    let (protection, buffer) = get_bool(buffer);
    if protection {
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

    let (edge_border, buffer) = get_byte(buffer);
    let (starting_board_number, buffer) = get_byte(buffer);
    let (end_game_board, buffer) = get_byte(buffer);
    let (death_board, buffer) = get_byte(buffer);
    let (end_game_x, buffer) = get_word(buffer);
    let (end_game_y, buffer) = get_word(buffer);
    let (game_over_sfx, buffer) = get_bool(buffer);
    let (death_x, buffer) = get_word(buffer);
    let (death_y, buffer) = get_word(buffer);
    let (starting_lives, buffer) = get_word(buffer);
    let (limit_lives, buffer) = get_word(buffer);
    let (starting_health, buffer) = get_word(buffer);
    let (limit_health, buffer) = get_word(buffer);
    let (enemies_hurt_enemies, buffer) = get_bool(buffer);
    let (clear_messages_and_projectiles, buffer) = get_bool(buffer);
    let (only_play_via_swap_world, buffer) = get_bool(buffer);

    let (palette_color_data, buffer) = buffer.split_at(16 * 3);
    let mut colors = vec![];
    for rgb in palette_color_data.chunks(3) {
        assert!(rgb.iter().all(|&b| b < 64u8));
        colors.push(Color { r: rgb[0], g: rgb[1], b: rgb[2] });
    }
    assert_eq!(colors.len(), 16);

    let (_global_robot_pos, buffer) = get_dword(buffer);
    let (sfx, mut buffer) = get_byte(buffer);
    let num_boards = if sfx == 0 {
        let (len, new_buffer) = get_word(buffer);
        let (_sfx, new_buffer) = new_buffer.split_at(len as usize);
        let (num_boards, new_buffer) = get_byte(new_buffer);
        buffer = new_buffer;
        num_boards
    } else {
        if sfx > 150 {
            return Err(WorldError::TooManyBoards(sfx));
        }
        sfx
    };

    let mut titles = vec![];
    let mut boards = vec![];
    for _ in 0..num_boards {
        let (title, new_buffer) = try!(get_null_terminated_string(buffer, 25).map_err(|_| WorldError::InvalidUTF8Title));
        buffer = new_buffer;

        titles.push(title);
    }

    for title in titles {
        let (byte_length, new_buffer) = get_dword(buffer);
        let byte_length = byte_length as usize;

        let (board_pos, new_buffer) = get_dword(new_buffer);
        let board_pos = board_pos as usize;

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
        title: title,
        charset: charset,
        palette: Palette { colors: colors },
        boards: boards,
        edge_border: ColorValue(edge_border),
        starting_board_number: BoardId(starting_board_number),
        end_game_board: BoardId(end_game_board),
        death_board: BoardId(death_board),
        end_game_pos: Coordinate((end_game_x, end_game_y)),
        game_over_sfx: game_over_sfx,
        death_pos: Coordinate((death_x, death_y)),
        starting_lives: starting_lives,
        limit_lives: limit_lives,
        starting_health: starting_health,
        limit_health: limit_health,
        enemies_hurt_enemies: enemies_hurt_enemies,
        clear_messages_and_projectiles: clear_messages_and_projectiles,
        only_play_via_swap_world: only_play_via_swap_world,
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
