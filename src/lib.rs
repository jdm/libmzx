#![feature(slice_patterns)]

extern crate byteorder;

use byteorder::{ByteOrder, LittleEndian};
use std::ptr;
use std::str;

pub struct World {
    title: String,
    charset: Charset,
    palette: Palette,
    boards: Vec<Board>,
}

pub struct Board {
    title: String,
    width: u8,
    height: u8,
    overlay: Option<(OverlayMode, Vec<u8>)>,
    level: Vec<(u8, u8, u8)>,
    under: Vec<(u8, u8, u8)>,
    mod_file: String,
    upper_left_viewport: (u8, u8),
    viewport_size: (u8, u8),
    can_shoot: bool,
    can_bomb: bool,
    fire_burns_brown: bool,
    fire_burns_space: bool,
    fire_burns_fakes: bool,
    fire_burns_trees: bool,
    explosion_result: ExplosionResult,
    save_restriction: SaveRestriction,
    collect_bombs: bool,
    fire_burns_forever: bool,
    exits: (Option<u8>, Option<u8>, Option<u8>, Option<u8>),
    restart_when_zapped: bool,
    time_limit: u16,    
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
        &self.data[(n*14) as usize..((n+1)*14) as usize]
    }
}

pub struct Color {
    pub r: u8,
    pub g: u8,
    pub b: u8,
}

pub struct Palette {
    colors: Vec<Color>,
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
}

impl<'a> From<BoardError> for WorldError<'a> {
    fn from(err: BoardError) -> WorldError<'a> {
        WorldError::Board(err)
    }
}

fn load_board<'a>(buffer: &'a [u8]) -> Result<(Board, &'a [u8]), BoardError> {
    let (title, buffer) = buffer.split_at(25);
    let title_end = title.iter().position(|b| *b == 0);
    let title = match title_end {
        Some(idx) => try!(str::from_utf8(&title[0..idx]).map_err(|_| BoardError::InvalidUTF8Title)),
        None => return Err(BoardError::NoNullInTitle),
    };
    
    
    Ok((Board {
        title: title.into(),
        width: 0,
        height: 0,
        overlay: None,
        level: vec![],
        under: vec![],
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
    }, buffer))
}

fn is_bool(byte: &[u8]) -> bool {
    byte.len() == 1 && (byte[0] == 0 || byte[0] == 1)
}

pub fn load_world<'a>(buffer: &'a [u8]) -> Result<World, WorldError<'a>> {
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

    let mut boards = vec![];
    for _ in 0..num_boards {
        let (board, new_buffer) = try!(load_board(buffer));
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
