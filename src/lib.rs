extern crate byteorder;
extern crate env_logger;
#[macro_use]
extern crate log;
extern crate itertools;

use byteorder::{ByteOrder, LittleEndian};
use itertools::Zip;
use std::str;

pub struct BoardId(pub u8);
pub struct ColorValue(pub u8);
pub struct Coordinate<T>(pub (T, T));
pub struct Size<T>(pub (T, T));

pub struct World {
    pub version: u32,
    pub title: String,
    pub charset: Charset,
    pub palette: Palette,
    pub boards: Vec<Board>,
    pub edge_border: ColorValue,
    pub starting_board_number: BoardId,
    pub end_game_board: BoardId,
    pub death_board: BoardId,
    pub end_game_pos: Coordinate<u16>,
    pub game_over_sfx: bool,
    pub death_pos: Coordinate<u16>,
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
    pub upper_left_viewport: Coordinate<u8>,
    pub viewport_size: Size<u8>,
    pub can_shoot: bool,
    pub can_bomb: bool,
    pub fire_burns_brown: bool,
    pub fire_burns_space: bool,
    pub fire_burns_fakes: bool,
    pub fire_burns_trees: bool,
    pub explosion_result: ExplosionResult,
    pub forest_becomes_floor: bool,
    pub save_restriction: SaveRestriction,
    pub collect_bombs: bool,
    pub fire_burns_forever: bool,
    pub exits: (Option<BoardId>, Option<BoardId>, Option<BoardId>, Option<BoardId>),
    pub restart_when_zapped: bool,
    pub time_limit: u16,
    pub robots: Vec<Robot>,
}

#[derive(Debug)]
pub enum Direction {
    Idle = 0,
    North = 1,
    South = 2,
    East = 3,
    West = 4,
}

#[derive(Debug)]
pub enum BulletType {
    Player,
    Neutral,
    Enemy,
}

#[derive(Debug)]
pub enum OverlayMode {
    Normal,
    Static,
    Transparent,
}

#[derive(Debug)]
pub enum ExplosionResult {
    Nothing,
    Ash,
    Fire,
}

#[derive(Debug)]
pub enum SaveRestriction {
    Unrestricted,
    NoSave,
    OnlyOnSensor,
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

pub struct Robot {
    pub name: String,
    pub ch: u8,
    pub current_line: u16,
    pub current_loc: u8,
    pub cycle: u8,
    pub cycle_count: u8,
    pub bullet_type: BulletType,
    pub locked: bool,
    pub lavawalking: bool,
    pub walk: Direction,
    pub last_touched: Direction,
    pub last_shot: Direction,
    pub position: Coordinate<u16>,
    pub reserved: [u8; 3],
    pub onscreen: bool,
    pub loop_count: u16,
    pub program: Vec<u8>,
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
    UnknownExplosionResult(u8),
    UnknownSaveRestriction(u8),
    InvalidInputString,
    InvalidMessage,
}

impl<'a> From<BoardError> for WorldError<'a> {
    fn from(err: BoardError) -> WorldError<'a> {
        WorldError::Board(err)
    }
}

fn get_string_with_preceding_length(buffer: &[u8]) -> Result<(String, &[u8]), ()> {
    let (length, buffer) = get_word(buffer);
    let length = length as usize;
    let (s, buffer) = buffer.split_at(length);
    Ok((try!(str::from_utf8(&s[0..length]).map_err(|_| ())).into(), buffer))
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
    if byte != 0 && byte != 1 {
        assert_eq!(byte, 0);
    }
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

fn get_direction(buffer: &[u8]) -> (Direction, &[u8]) {
    let (byte, buffer) = get_byte(buffer);
    let dir = match byte {
        0 => Direction::Idle,
        1 => Direction::North,
        2 => Direction::South,
        3 => Direction::East,
        4 => Direction::West,
        n => panic!("found non-direction value ({})", n),
    };
    (dir, buffer)
}

fn maybe_get_board(buffer: &[u8]) -> (Option<BoardId>, &[u8]) {
    let (board, buffer) = get_byte(buffer);
    (if board == 255 {
        None
    } else {
        Some(BoardId(board))
    }, buffer)
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

fn load_robot(buffer: &[u8]) -> (Robot, &[u8]) {
    let (program_length, buffer) = get_word(buffer);
    let (_, buffer) = get_word(buffer);
    let (name, buffer) = get_null_terminated_string(buffer, 15).unwrap();
    let (ch, buffer) = get_byte(buffer);
    let (current_line, buffer) = get_word(buffer);
    let (current_loc, buffer) = get_byte(buffer);
    let (cycle, buffer) = get_byte(buffer);
    let (cycle_count, buffer) = get_byte(buffer);
    let (bullet_type, buffer) = get_byte(buffer);
    let bullet_type = match bullet_type {
        0 => BulletType::Player,
        1 => BulletType::Neutral,
        2 => BulletType::Enemy,
        _ => panic!(),
    };
    let (locked, buffer) = get_bool(buffer);
    let (lavawalking, buffer) = get_bool(buffer);
    let (walk, buffer) = get_direction(buffer);
    let (last_touched, buffer) = get_direction(buffer);
    let (last_shot, buffer) = get_direction(buffer);
    let (x_pos, buffer) = get_word(buffer);
    let (y_pos, buffer) = get_word(buffer);
    let (_reserved, buffer) = buffer.split_at(3);
    let (onscreen, buffer) = get_bool(buffer);
    let (loop_count, buffer) = get_word(buffer);
    let (program, buffer) = buffer.split_at(program_length as usize);

    let robot = Robot {
        name: name,
        ch: ch,
        current_line: current_line,
        current_loc: current_loc,
        cycle: cycle,
        cycle_count: cycle_count,
        bullet_type: bullet_type,
        locked: locked,
        lavawalking: lavawalking,
        walk: walk,
        last_touched: last_touched,
        last_shot: last_shot,
        position: Coordinate((x_pos, y_pos)),
        reserved: [0, 0, 0],
        onscreen: onscreen,
        loop_count: loop_count,
        program: program.into(),
    };
    (robot, buffer)
}

fn load_board(title: String, version: u32, buffer: &[u8]) -> Result<Board, BoardError> {
    debug!("loading {}", title);
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
        let (chars, new_buffer, w, h) = decode_runs(new_buffer);
        let (colors, new_buffer, w2, h2) = decode_runs(new_buffer);
        assert_eq!(w, w2);
        assert_eq!(h, h2);
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

    let string = if version < 0x253 {
        get_null_terminated_string(buffer, 13)
    } else {
        get_string_with_preceding_length(buffer)
    };
    let (mod_file, buffer) = try!(string.map_err(|_| BoardError::InvalidModFile));

    let (upper_view_x, buffer) = get_byte(buffer);
    let (upper_view_y, buffer) = get_byte(buffer);
    let (view_width, buffer) = get_byte(buffer);
    let (view_height, buffer) = get_byte(buffer);
    let (can_shoot, buffer) = get_bool(buffer);
    let (can_bomb, buffer) = get_bool(buffer);
    let (fire_burns_brown, buffer) = get_bool(buffer);
    let (fire_burns_space, buffer) = get_bool(buffer);
    let (fire_burns_fakes, buffer) = get_bool(buffer);
    let (fire_burns_trees, buffer) = get_bool(buffer);
    let (explosion_result, buffer) = get_byte(buffer);
    let explosion_result = match explosion_result {
        0 => ExplosionResult::Nothing,
        1 => ExplosionResult::Ash,
        2 => ExplosionResult::Fire,
        _ => return Err(BoardError::UnknownExplosionResult(explosion_result)),
    };
    let (save_restriction, buffer) = get_byte(buffer);
    let save_restriction = match save_restriction {
        0 => SaveRestriction::Unrestricted,
        1 => SaveRestriction::NoSave,
        2 => SaveRestriction::OnlyOnSensor,
        _ => return Err(BoardError::UnknownSaveRestriction(save_restriction)),
    };
    let (forest_becomes_floor, buffer) = get_bool(buffer);
    let (collect_bombs, buffer) = get_bool(buffer);
    let (fire_burns_forever, buffer) = get_bool(buffer);
    let (north_board, buffer) = maybe_get_board(buffer);
    let (south_board, buffer) = maybe_get_board(buffer);
    let (east_board, buffer) = maybe_get_board(buffer);
    let (west_board, buffer) = maybe_get_board(buffer);
    let (restart_when_zapped, buffer) = get_bool(buffer);
    let (time_limit, mut buffer) = get_word(buffer);

    if version < 0x253 {
        let (_last_key, new_buffer) = get_byte(buffer);
        let (_last_input, new_buffer) = get_word(new_buffer);
        let (_last_input_length, new_buffer) = get_byte(new_buffer);
        let (_last_input_string, new_buffer) = try!(get_null_terminated_string(new_buffer, 81).map_err(|_| BoardError::InvalidInputString));
        let (_last_player_dir, new_buffer) = get_byte(new_buffer);
        let (_current_message, new_buffer) = try!(get_null_terminated_string(new_buffer, 81).map_err(|_| BoardError::InvalidMessage));
        let (_cycles_until_disappear, new_buffer) = get_byte(new_buffer);
        let (_lazer_wall_timer, new_buffer) = get_byte(new_buffer);
        let (_message_row, new_buffer) = get_byte(new_buffer);
        let (_message_col, new_buffer) = get_byte(new_buffer);
        let (_x_scroll, new_buffer) = get_word(new_buffer);
        let (_y_scroll, new_buffer) = get_word(new_buffer);
        let (_x_screen_pos, new_buffer) = get_word(new_buffer);
        let (_y_screen_pos, new_buffer) = get_word(new_buffer);
        buffer = new_buffer;
    }

    let (_player_locked_ns, buffer) = get_byte(buffer);
    let (_player_locked_ew, buffer) = get_byte(buffer);
    let (_player_locked_attack, mut buffer) = get_byte(buffer);
    if version < 0x253 {
        let (_mod_volume, new_buffer) = get_byte(buffer);
        let (_mod_volume_change, new_buffer) = get_byte(new_buffer);
        let (_mod_volume_target, new_buffer) = get_byte(new_buffer);
        buffer = new_buffer;
    }
    let (num_robots, mut buffer) = get_byte(buffer);

    let mut robots = vec![];
    for _ in 0..num_robots {
        let (robot, new_buffer) = load_robot(buffer);
        robots.push(robot);
        buffer = new_buffer;
    }

    Ok(Board {
        title: title,
        width: width,
        height: height,
        overlay: overlay,
        level: Zip::new((ids.into_iter(), colors.into_iter(), params.into_iter())).collect(),
        under: Zip::new((under_ids.into_iter(), under_colors.into_iter(), under_params.into_iter())).collect(),
        mod_file: mod_file,
        upper_left_viewport: Coordinate((upper_view_x, upper_view_y)),
        viewport_size: Size((view_width, view_height)),
        can_shoot: can_shoot,
        can_bomb: can_bomb,
        fire_burns_brown: fire_burns_brown,
        fire_burns_space: fire_burns_space,
        fire_burns_fakes: fire_burns_fakes,
        fire_burns_trees: fire_burns_trees,
        explosion_result: explosion_result,
        save_restriction: save_restriction,
        forest_becomes_floor: forest_becomes_floor,
        collect_bombs: collect_bombs,
        fire_burns_forever: fire_burns_forever,
        exits: (north_board, south_board, east_board, west_board),
        restart_when_zapped: restart_when_zapped,
        time_limit: time_limit,
        robots: robots,
    })
}

pub fn load_world<'a>(buffer: &'a [u8]) -> Result<World, WorldError<'a>> {
    let original_buffer = buffer;

    let (title, buffer) = try!(get_null_terminated_string(buffer, 25).map_err(|_| WorldError::InvalidUTF8Title));

    let (protection, buffer) = get_bool(buffer);
    if protection {
        return Err(WorldError::Protected);
    }

    let (signature, buffer) = buffer.split_at(3);
    let version = match (signature[0], signature[1], signature[2]) {
        (b'M', b'Z', b'X') => 0x0100,
        (b'M', b'Z', b'2') => 0x0205,
        (b'M', b'Z', b'A') => 0x0208,
        (b'M', a, b) if a > 1 && a < 10 => ((a as u32) << 8) + (b as u32),
        _ => return Err(WorldError::UnrecognizedVersion(signature)),
    };

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
        let board = try!(load_board(title, version,
                                    &original_buffer[board_pos..end_board_pos]));
        boards.push(board);
        buffer = new_buffer;
    }

    Ok(World {
        version: version,
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
