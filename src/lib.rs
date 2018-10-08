extern crate byteorder;
#[macro_use]
extern crate enum_primitive;
#[macro_use]
extern crate enum_primitive_derive;
extern crate itertools;
#[macro_use]
extern crate log;
extern crate num_traits;

mod render;
mod robotic;

pub use self::render::{Renderer, render};
pub use self::robotic::{
    Command, Resolve, Operator, ExtendedParam, ExtendedColorValue, RelativePart, SignedNumeric,
    ModifiedDirection,
};

use byteorder::{ByteOrder, LittleEndian};
use itertools::Zip;
use num_traits::{ToPrimitive, FromPrimitive};
use self::robotic::parse_program;
use std::collections::HashMap;
use std::default::Default;
use std::fmt;
use std::ops::Deref;
use std::str;

#[derive(Copy, Clone, Debug, PartialEq)]
pub struct BoardId(pub u8);
#[derive(Copy, Clone, Debug, PartialEq)]
pub struct ColorValue(pub u8);
#[derive(Copy, Clone, Debug, PartialEq)]
pub struct ParamValue(pub u8);
#[derive(Copy, Clone, Debug, PartialEq)]
pub struct Coordinate<T>(pub T, pub T);
#[derive(Copy, Clone, Debug)]
pub struct Size<T>(pub T, pub T);

const LEGACY_WORLD_VERSION: u32 = 0x0254;

pub struct World {
    pub version: u32,
    pub title: ByteString,
    pub state: WorldState,
    pub boards: Vec<Board>,
    pub board_robots: Vec<Vec<Robot>>,
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

pub struct WorldState {
    pub charset: Charset,
    pub palette: Palette,
    pub idchars: Box<[u8]>,
    pub saved_positions: [(usize, Coordinate<u16>); 10],
}

impl WorldState {
    pub fn set_char_id(&mut self, id: CharId, val: u8) {
        self.idchars[id.to_usize().unwrap()] = val;
    }

    pub fn char_id(&self, id: CharId) -> u8 {
        self.idchars[id.to_usize().unwrap()]
    }

    pub fn char_id_offset(&self, id: CharId, offset: u8) -> u8 {
        self.idchars[id.to_usize().unwrap() + offset as usize]
    }
}

pub struct Board {
    pub title: ByteString,
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
    pub scrolls: Vec<Scroll>,
    pub sensors: Vec<Sensor>,
    pub player_pos: Coordinate<u16>,
    pub scroll_offset: Coordinate<u16>,
}

impl Board {
    fn init(&mut self, robots: &mut [Robot]) {
        for (idx, &(thing, _, param)) in self.level.iter().enumerate() {
            if thing == Thing::Robot.to_u8().unwrap() {
                robots[param as usize - 1].position = Coordinate(
                    (idx % self.width) as u16,
                    (idx / self.width) as u16
                );
            } else if thing == Thing::Player.to_u8().unwrap() {
                self.player_pos = Coordinate(
                    (idx % self.width) as u16,
                    (idx / self.width) as u16
                );
            }
        }
    }

    pub fn thing_at(&self, pos: &Coordinate<u16>) -> Thing {
        Thing::from_u8(self.level_at(pos).0).expect("invalid thing value")
    }

    pub fn level_at(&self, pos: &Coordinate<u16>) -> &(u8, u8, u8) {
        let idx = self.width * pos.1 as usize + pos.0 as usize;
        &self.level[idx]
    }

    pub fn level_at_mut(&mut self, pos: &Coordinate<u16>) -> &mut (u8, u8, u8) {
        let idx = self.width * pos.1 as usize + pos.0 as usize;
        &mut self.level[idx]
    }

    pub fn under_at(&self, pos: &Coordinate<u16>) -> &(u8, u8, u8) {
        let idx = self.width * pos.1 as usize + pos.0 as usize;
        &self.under[idx]
    }

    pub fn under_at_mut(&mut self, pos: &Coordinate<u16>) -> &mut (u8, u8, u8) {
        let idx = self.width * pos.1 as usize + pos.0 as usize;
        &mut self.under[idx]
    }

    pub fn copy(&mut self, src: Coordinate<u16>, block: Size<u16>, dest: Coordinate<u16>) {
        let mut yiter = if src.1 > dest.1 {
            Box::new(0..block.1) as Box<Iterator<Item=u16>>
        } else {
            Box::new((0..block.1).rev()) as Box<Iterator<Item=u16>>
        };

        while let Some(j) = yiter.next() {
            let mut xiter = if src.0 > dest.0 {
                Box::new(0..block.0) as Box<Iterator<Item=u16>>
            } else {
                Box::new((0..block.0).rev()) as Box<Iterator<Item=u16>>
            };
            while let Some(i) = xiter.next() {
                let src_coord = Coordinate(src.0 + i, src.1 + j);
                let dest_coord = Coordinate(dest.0 + i, dest.1 + j);
                if src_coord.0 as usize > self.width || dest_coord.0 as usize > self.width ||
                    src_coord.1 as usize > self.height || dest_coord.1 as usize > self.height
                {
                    continue;
                }

                let src = *self.level_at(&src_coord);
                *self.level_at_mut(&dest_coord) = src;
                let src = *self.under_at(&src_coord);
                *self.under_at_mut(&dest_coord) = src;
            }
        }
    }

    pub fn move_level(&mut self, pos: &Coordinate<u16>, xdiff: i8, ydiff: i8) {
        self.move_level_to(
            pos,
            &Coordinate(
                (pos.0 as i16 + xdiff as i16) as u16,
                (pos.1 as i16 + ydiff as i16) as u16
            )
        );
    }

    pub fn move_level_to(&mut self, pos: &Coordinate<u16>, new_pos: &Coordinate<u16>) {
        let old_idx = (pos.1 * self.width as u16 + pos.0) as usize;
        let new_idx = (new_pos.1 * self.width as u16 + new_pos.0) as usize;
        self.under[new_idx] = self.level[new_idx];
        self.level[new_idx] = self.level[old_idx];
        self.level[old_idx] = self.under[old_idx];
    }

    pub fn put_at(&mut self, pos: &Coordinate<u16>, thing: u8, color: u8, param: u8) {
        let idx = (pos.1 * self.width as u16 + pos.0) as usize;
        self.under[idx] = self.level[idx];
        self.level[idx] = (thing, color, param);
    }

    pub fn remove_thing_at(&mut self, pos: &Coordinate<u16>) {
        let idx = (pos.1 * self.width as u16 + pos.0) as usize;
        self.level[idx] = self.under[idx];
    }

    pub fn write_overlay(&mut self, pos: &Coordinate<u16>, s: &ByteString, color: u8) {
        let overlay = match self.overlay {
            Some((_mode, ref mut overlay)) => overlay,
            None => return,
        };

        let diff = self.width as isize - (pos.0 as usize + s.len()) as isize;
        let end_offset = if diff > 0 {
            s.len()
        } else if diff.abs() as usize > s.len() {
            return;
        } else {
            (s.len() as isize + diff) as usize
        };

        let start_idx = (pos.1 * self.width as u16 + pos.0) as usize;
        let end_idx = start_idx + end_offset;
        for (ch, (och, ocol)) in s[0..end_offset].iter().zip(&mut overlay[start_idx..end_idx]) {
            *och = *ch;
            *ocol = color;
        }
    }
}

pub struct Counters {
    counters: HashMap<ByteString, i16>,
}

impl Counters {
    pub fn new() -> Counters {
        Counters {
            counters: HashMap::new(),
        }
    }

    pub fn set(&mut self, name: ByteString, context: &mut Robot, value: i16) {
        if let Some(local) = LocalCounter::from(&name) {
            *context.local_counter_mut(local) = value;
        } else {
            self.counters.insert(name, value);
        }
    }

    pub fn get(&self, name: &ByteString, context: &Robot) -> i16 {
        if let Some(local) = LocalCounter::from(name) {
            *context.local_counter(local)
        } else {
            *self.counters.get(&name).unwrap_or(&0)
        }
    }
}

pub struct Scroll {
    pub num_lines: u16,
    pub text: ByteString,
    pub used: bool,
}

pub struct Sensor {
    pub name: ByteString,
    pub ch: u8,
    pub target: ByteString,
    pub used: bool,
}

#[derive(Clone, Hash, Eq)]
pub struct ByteString(Vec<u8>);

impl Default for ByteString {
    fn default() -> ByteString {
        ByteString(vec![])
    }
}

impl<'a> Into<ByteString> for &'a ByteString {
    fn into(self) -> ByteString {
        self.clone()
    }
}

impl<'a> From<&'a str> for ByteString {
    fn from(v: &'a str) -> ByteString {
        ByteString(v.as_bytes().to_vec())
    }
}

impl PartialEq for ByteString {
    fn eq(&self, other: &ByteString) -> bool {
        self.as_bytes().len() == other.as_bytes().len() &&
        self.as_bytes()
            .iter()
            .zip(other.as_bytes().iter())
            .all(|(a, b)| a.to_ascii_lowercase() == b.to_ascii_lowercase())
    }
}

impl PartialEq<str> for ByteString {
    fn eq(&self, other: &str) -> bool {
        self.as_bytes().len() == other.as_bytes().len() &&
        self.as_bytes()
            .iter()
            .zip(other.as_bytes().iter())
            .all(|(a, b)| a.to_ascii_lowercase() == b.to_ascii_lowercase())
    }
}

impl ByteString {
    pub fn as_bytes(&self) -> &[u8] {
        self.0.as_ref()
    }

    pub fn into_string(self) -> String {
        String::from_utf8(self.0).expect("Invalid UTF8 string")
    }

    pub fn to_string(&self) -> String {
        self.clone().into_string()
    }
}

impl Deref for ByteString {
    type Target = [u8];
    fn deref(&self) -> &[u8] {
        &self.0
    }
}

impl fmt::Debug for ByteString {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", String::from_utf8_lossy(&self.0))
    }
}

#[derive(Copy, Clone, Debug, Primitive, PartialEq)]
pub enum Direction {
    Idle = 0,
    North = 1,
    South = 2,
    East = 3,
    West = 4,
    RandNs = 5,
    RandEw = 6,
    RandNe = 7,
    RandNb = 8,
    Seek = 9,
    RandAny = 10,
    Beneath = 11,
    Anydir = 12,
    Flow = 13,
    NoDir = 14,
    RandB = 15,
    /*RandP = 16,
    Cw = 32,
    Opp = 64,
    RandNot = 128,*/
}

#[derive(Copy, Clone, Debug, PartialEq, Primitive)]
pub enum CardinalDirection {
    North = 1,
    South = 2,
    East = 3,
    West = 4,
}

#[derive(Copy, Clone, Debug, Primitive, PartialEq)]
pub enum CharId {
    ExplosionStage1 = 184,
    ExplosionStage2 = 185,
    ExplosionStage3 = 186,
    ExplosionStage4 = 187,
    FireAnim1 = 266,
    FireAnim2 = 267,
    FireAnim3 = 268,
    FireAnim4 = 269,
    FireAnim5 = 270,
    FireAnim6 = 271,
    FireColor1 = 272,
    FireColor2 = 273,
    FireColor3 = 274,
    FireColor4 = 275,
    FireColor5 = 276,
    FireColor6 = 277,
    PlayerNorth = 318,
    PlayerSouth = 319,
    PlayerEast = 320,
    PlayerWest = 321,
    PlayerColor = 322,
}

#[derive(Copy, Clone, Debug, Primitive, PartialEq)]
pub enum Thing {
    Space = 0,
    Normal = 1,
    Solid = 2,
    Tree = 3,
    Line = 4,
    CustomBlock = 5,
    Breakaway = 6,
    CustomBreak = 7,
    Boulder = 8 ,
    Crate = 9,
    CustomPush = 10,
    Box = 11,
    CustomBox = 12,
    Fake = 13,
    Carpet = 14,
    Floor = 15,
    Tiles = 16,
    CustomFloor = 17,
    Web = 18,
    ThickWeb = 19,
    StillWater = 20,
    NWater = 21,
    SWater = 22,
    EWater = 23,
    WWater = 24,
    Ice = 25,
    Lava = 26,
    Chest = 27,
    Gem = 28,
    MagicGem = 29,
    Health = 30,
    Ring = 31,
    Potion = 32,
    Energizer = 33,
    Goop = 34,
    Ammo = 35,
    Bomb = 36,
    LitBomb = 37,
    Explosion = 38,
    Key = 39,
    Lock = 40,
    Door = 41,
    OpenDoor = 42,
    Stairs = 43,
    Cave = 44,
    CWRotate = 45,
    CCWRotate = 46,
    Gate = 47,
    OpenGate = 48,
    Transport = 49,
    Coin = 50,
    NMovingWall = 51,
    SMovingWall = 52,
    EMovingWall = 53,
    WMovingWall = 54,
    Pouch = 55,
    Pusher = 56,
    SliderNS = 57,
    SliderEW = 58,
    Lazer = 59,
    LazerGun = 60,
    Bullet = 61,
    Missile = 62,
    Fire = 63,
    Forest = 65,
    Life = 66,
    Whirlpool1 = 67,
    Whirlpool2 = 68,
    Whirlpool3 = 69,
    Whirlpool4 = 70,
    InvisibleWall = 71,
    RicochetPanel = 72,
    Ricochet = 73,
    Mine = 74,
    Spike = 75,
    CustomHurt = 76,
    Text = 77,
    ShootingFire = 78,
    Seeker = 79,
    Snake = 80,
    Eye = 81,
    Thief = 82,
    SlimeBlob = 83,
    Runner = 84,
    Ghost = 85,
    Dragon = 86,
    Fish = 87,
    Shark = 88,
    Spider = 89,
    Goblin = 90,
    SpittingTiger = 91,
    BulletGun = 92,
    SpinningGun = 93,
    Bear = 94,
    BearCub = 95,
    MissileGun = 97,
    Sprite = 98,
    SpriteCollision = 99,
    ImageFile = 100,
    Sensor = 122,
    RobotPushable = 123,
    Robot = 124,
    Sign = 125,
    Scroll = 126,
    Player = 127,
    NoId = 255,
}

impl Thing {
    pub fn is_solid(&self) -> bool {
        match *self {
            Thing::Solid |
            Thing::Line |
            Thing::CustomBlock |
            Thing::Breakaway |
            Thing::CustomBreak |
            Thing::Boulder |
            Thing::Crate |
            Thing::CustomPush |
            Thing::Box |
            Thing::CustomBox |
            Thing::Chest |
            Thing::Gem |
            Thing::MagicGem |
            Thing::Health |
            Thing::Ring |
            Thing::Potion |
            Thing::Energizer |
            Thing::Goop |
            Thing::Ammo |
            Thing::Bomb |
            Thing::LitBomb |
            Thing::Key |
            Thing::Lock |
            Thing::Door |
            Thing::Gate |
            Thing::Transport |
            Thing::Coin |
            Thing::Pouch |
            Thing::Pusher |
            Thing::SliderNS |
            Thing::SliderEW |
            Thing::LazerGun |
            Thing::Bullet |
            Thing::Missile |
            Thing::Life |
            Thing::InvisibleWall |
            Thing::Mine |
            Thing::Spike |
            Thing::CustomHurt |
            Thing::Text |
            Thing::Snake |
            Thing::Eye |
            Thing::Thief |
            Thing::SlimeBlob |
            Thing::Runner |
            Thing::Ghost |
            Thing::Dragon |
            Thing::Fish |
            Thing::Shark |
            Thing::Spider |
            Thing::Goblin |
            Thing::SpittingTiger |
            Thing::BulletGun |
            Thing::SpinningGun |
            Thing::Bear |
            Thing::BearCub |
            Thing::MissileGun |
            Thing::RobotPushable |
            Thing::Robot |
            Thing::Sign |
            Thing::Scroll => true,
            _ => false,
        }
    }
}

#[derive(Debug)]
pub enum BulletType {
    Player,
    Neutral,
    Enemy,
}

#[derive(Debug, Copy, Clone, PartialEq)]
pub enum OverlayMode {
    Normal,
    Static,
    Transparent,
}

#[derive(Debug, PartialEq)]
pub enum ExplosionResult {
    Nothing,
    Ash,
    Fire,
}

pub struct Explosion {
    pub stage: u8,
    pub size: u8,
}

impl Explosion {
    pub fn to_param(&self) -> u8 {
        self.stage | (self.size << 4)
    }

    pub fn from_param(p: u8) -> Explosion {
        let stage = p & 0x0F;
        assert!(stage < 4);
        let size = (p & 0xF0) >> 4;
        assert!(size < 16);
        Explosion {
            stage,
            size,
        }
    }
}

#[derive(Debug)]
pub enum SaveRestriction {
    Unrestricted,
    NoSave,
    OnlyOnSensor,
}

pub const CHAR_BYTES: usize = 14;
const CHARSET_BUFFER_SIZE: usize = CHAR_BYTES * 256;

pub struct Charset {
    pub data: [u8; CHARSET_BUFFER_SIZE],
}

impl Charset {
    pub fn nth(&self, n: u8) -> &[u8] {
        let n = n as usize;
        &self.data[(n * CHAR_BYTES)..((n + 1) * CHAR_BYTES)]
    }

    pub fn nth_mut(&mut self, n: u8) -> &mut [u8] {
        let n = n as usize;
        &mut self.data[(n * CHAR_BYTES)..((n + 1) * CHAR_BYTES)]
    }
}

pub struct Color {
    pub r: u8,
    pub g: u8,
    pub b: u8,
}

pub struct Palette {
    pub colors: Vec<(Color, f32)>,
}

pub struct Robot {
    pub name: ByteString,
    pub ch: u8,
    pub current_line: u16,
    pub current_loc: u8,
    pub cycle: u8,
    pub cycle_count: u8,
    pub bullet_type: BulletType,
    pub locked: bool,
    pub lavawalking: i16,
    pub walk: Option<CardinalDirection>,
    pub last_touched: Option<CardinalDirection>,
    pub last_shot: Option<CardinalDirection>,
    pub position: Coordinate<u16>,
    pub reserved: [u8; 3],
    pub onscreen: bool,
    pub loop_count: i16,
    pub program: Vec<Command>,
    pub alive: bool,
    pub status: RunStatus,
    pub local: [i16; 32],
}

pub enum LocalCounter {
    Loopcount,
    Local(u8),
    Lavawalk,
}

impl LocalCounter {
    fn from(name: &ByteString) -> Option<LocalCounter> {
        if name == "loopcount" {
            Some(LocalCounter::Loopcount)
        } else if name == "local" {
                Some(LocalCounter::Local(0))
        } else if {
            let subname = ByteString(name[0..5].to_owned());
            &subname == "local"
        } {
            let suffix = str::from_utf8(&name[5..]).ok().and_then(|s| s.parse::<u16>().ok());
            match suffix {
                Some(n) => Some(LocalCounter::Local((n % 32) as u8)),
                _ => None,
            }
        } else if name == "lava_walk" {
            Some(LocalCounter::Lavawalk)
        } else {
            None
        }
    }
}

impl Robot {
    fn local_counter(&self, counter: LocalCounter) -> &i16 {
        match counter {
            LocalCounter::Loopcount => &self.loop_count,
            LocalCounter::Local(n) => &self.local[n as usize],
            LocalCounter::Lavawalk => &self.lavawalking,
        }
    }

    fn local_counter_mut(&mut self, counter: LocalCounter) -> &mut i16 {
        match counter {
            LocalCounter::Loopcount => &mut self.loop_count,
            LocalCounter::Local(n) => &mut self.local[n as usize],
            LocalCounter::Lavawalk => &mut self.lavawalking,
        }
    }
}

#[derive(PartialEq)]
pub enum RunStatus {
    NotRun,
    FinishedRunning,
}

#[derive(Debug)]
pub enum WorldError<'a> {
    Protected,
    TooNewVersion,
    UnrecognizedVersion(&'a [u8]),
    CharsetTooSmall,
    UnhandledSFX,
    TooManyBoards(u8),
    Board(BoardError),
}

#[derive(Debug)]
pub enum BoardError {
    UnexpectedSize(u8),
    UnknownOverlayMode(u8),
    UnknownExplosionResult(u8),
    UnknownSaveRestriction(u8),
}

impl<'a> From<BoardError> for WorldError<'a> {
    fn from(err: BoardError) -> WorldError<'a> {
        WorldError::Board(err)
    }
}

fn get_objects<T, F>(buffer: &[u8], loader: F) -> (Vec<T>, &[u8])
    where F: Fn(&[u8]) -> (T, &[u8])
{
    let (num_objects, mut buffer) = get_byte(buffer);
    debug!("loading {} objects", num_objects);
    let mut objects = vec![];
    for _ in 0..num_objects {
        let (object, new_buffer) = loader(buffer);
        objects.push(object);
        buffer = new_buffer;
    }
    (objects, buffer)
}

fn get_string_with_preceding_length(buffer: &[u8]) -> (ByteString, &[u8]) {
    let (length, buffer) = get_word(buffer);
    get_string_with_length(buffer, length)
}

fn get_string_with_length(buffer: &[u8], length: u16) -> (ByteString, &[u8]) {
    let (s, buffer) = buffer.split_at(length as usize);
    (ByteString(s.to_vec()), buffer)
}

fn get_null_terminated_string(buffer: &[u8], max_length: usize) -> (ByteString, &[u8]) {
    let (s, buffer) = buffer.split_at(max_length);
    assert_eq!(s.len(), max_length);
    let end = s.iter().position(|b| *b == 0);
    (ByteString(s[0..end.unwrap_or(max_length)].to_vec()), buffer)
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

fn get_direction(buffer: &[u8]) -> (Option<CardinalDirection>, &[u8]) {
    let (byte, buffer) = get_byte(buffer);
    let dir = if byte == 0 || byte == Direction::NoDir as u8 {
        None
    } else {
        Some(CardinalDirection::from_u8(byte).expect(&format!("invalid direction value: {}", byte)))
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
    let (name, buffer) = get_null_terminated_string(buffer, 15);
    debug!("loading robot {:?}", name);
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
    assert_eq!(program.len(), program_length as usize);
    let program = parse_program(&program);

    let robot = Robot {
        name: name,
        ch: ch,
        current_line: current_line.checked_sub(1).unwrap_or(0),
        current_loc: current_loc,
        cycle: cycle,
        cycle_count: cycle_count,
        bullet_type: bullet_type,
        locked: locked,
        lavawalking: if lavawalking { 1 } else { 0 },
        walk: walk,
        last_touched: last_touched,
        last_shot: last_shot,
        position: Coordinate(x_pos, y_pos),
        reserved: [0, 0, 0],
        onscreen: onscreen,
        loop_count: loop_count as i16,
        program: program.into(),
        alive: true,
        status: RunStatus::NotRun,
        local: [0; 32],
    };
    (robot, buffer)
}

fn load_scroll(buffer: &[u8]) -> (Scroll, &[u8]) {
    let (num_lines, buffer) = get_word(buffer);
    let (_junk, buffer) = get_word(buffer);
    let (chars, buffer) = get_word(buffer);
    let (used, buffer) = get_bool(buffer);
    let (text, buffer) = get_string_with_length(buffer, chars);
    debug!("{:?}", text);
    assert_eq!(text.len(), chars as usize);
    (
        Scroll { num_lines, text, used },
        buffer
    )
}

fn load_sensor(buffer: &[u8]) -> (Sensor, &[u8]) {
    let (name, buffer) = get_null_terminated_string(buffer, 15);
    let (ch, buffer) = get_byte(buffer);
    let (target, buffer) = get_null_terminated_string(buffer, 15);
    let (used, buffer) = get_bool(buffer);
    (
        Sensor { name, ch, target, used },
        buffer
    )
}

fn load_board(title: ByteString, version: u32, buffer: &[u8]) -> Result<(Board, Vec<Robot>), BoardError> {
    debug!("loading board {:?}", title);
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

    let (mod_file, buffer) = if version < 0x253 {
        get_null_terminated_string(buffer, 13)
    } else {
        get_string_with_preceding_length(buffer)
    };
    debug!("mod file: {:?}", mod_file);

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

    let (scroll_x, scroll_y) = if version < 0x253 {
        let (_last_key, new_buffer) = get_byte(buffer);
        let (_last_input, new_buffer) = get_word(new_buffer);
        let (_last_input_length, new_buffer) = get_byte(new_buffer);
        let (_last_input_string, new_buffer) = get_null_terminated_string(new_buffer, 81);
        let (_last_player_dir, new_buffer) = get_byte(new_buffer);
        let (_current_message, new_buffer) = get_null_terminated_string(new_buffer, 81);
        let (_cycles_until_disappear, new_buffer) = get_byte(new_buffer);
        let (_lazer_wall_timer, new_buffer) = get_byte(new_buffer);
        let (_message_row, new_buffer) = get_byte(new_buffer);
        let (_message_col, new_buffer) = get_byte(new_buffer);
        let (scroll_x, new_buffer) = get_word(new_buffer);
        let (scroll_y, new_buffer) = get_word(new_buffer);
        let (_x_screen_pos, new_buffer) = get_word(new_buffer);
        let (_y_screen_pos, new_buffer) = get_word(new_buffer);
        buffer = new_buffer;
        (scroll_x, scroll_y)
    } else {
        (0, 0)
    };

    let (_player_locked_ns, buffer) = get_byte(buffer);
    let (_player_locked_ew, buffer) = get_byte(buffer);
    let (_player_locked_attack, mut buffer) = get_byte(buffer);
    if version < 0x253 {
        let (_mod_volume, new_buffer) = get_byte(buffer);
        let (_mod_volume_change, new_buffer) = get_byte(new_buffer);
        let (_mod_volume_target, new_buffer) = get_byte(new_buffer);
        buffer = new_buffer;
    }

    let (robots, buffer) = get_objects(buffer, load_robot);
    debug!("loading scrolls");
    let (scrolls, buffer) = get_objects(buffer, load_scroll);
    debug!("loading sensors");
    let (sensors, _buffer) = get_objects(buffer, load_sensor);
    assert_eq!(_buffer.len(), 0);

    Ok((Board {
        title: title,
        width: width,
        height: height,
        overlay: overlay,
        level: Zip::new((ids.into_iter(), colors.into_iter(), params.into_iter())).collect(),
        under: Zip::new((under_ids.into_iter(), under_colors.into_iter(), under_params.into_iter())).collect(),
        mod_file: mod_file.to_string(),
        upper_left_viewport: Coordinate(upper_view_x, upper_view_y),
        viewport_size: Size(view_width, view_height),
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
        scrolls: scrolls,
        sensors: sensors,
        player_pos: Coordinate(0, 0),
        scroll_offset: Coordinate(scroll_x, scroll_y),
    },
    robots))
}

pub fn load_world<'a>(buffer: &'a [u8]) -> Result<World, WorldError<'a>> {
    let original_buffer = buffer;

    let (title, buffer) = get_null_terminated_string(buffer, 25);
    debug!("loading world {:?}", title);

    let (protection, buffer) = get_bool(buffer);
    if protection {
        return Err(WorldError::Protected);
    }
    // TODO: decrypt protected worlds

    let (signature, buffer) = buffer.split_at(3);
    let version = match (signature[0], signature[1], signature[2]) {
        (b'M', b'Z', b'X') => 0x0100,
        (b'M', b'Z', b'2') => 0x0205,
        (b'M', b'Z', b'A') => 0x0208,
        (b'M', a, b) if a > 1 && a < 10 => ((a as u32) << 8) + (b as u32),
        _ => return Err(WorldError::UnrecognizedVersion(signature)),
    };

    if version > LEGACY_WORLD_VERSION {
        return Err(WorldError::TooNewVersion);
    }

    let (charset_data, buffer) = buffer.split_at(14 * 256);
    if charset_data.len() < CHARSET_BUFFER_SIZE {
        return Err(WorldError::CharsetTooSmall);
    }
    let mut charset = Charset { data: [0; CHARSET_BUFFER_SIZE] };
    charset.data.copy_from_slice(charset_data);

    let (idchars, buffer) = buffer.split_at(455);

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
        colors.push((Color { r: rgb[0], g: rgb[1], b: rgb[2] }, 1.0));
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
    let mut board_robots = vec![];
    for _ in 0..num_boards {
        let (title, new_buffer) = get_null_terminated_string(buffer, 25);
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
        let (mut board, mut robots) = try!(
            load_board(title, version, &original_buffer[board_pos..end_board_pos])
        );
        board.init(&mut robots);
        boards.push(board);
        board_robots.push(robots);
        buffer = new_buffer;
    }

    Ok(World {
        version: version,
        title: title,
        state: WorldState {
            charset: charset,
            palette: Palette { colors: colors },
            idchars: idchars.to_vec().into_boxed_slice(),
            saved_positions: [(0, Coordinate(0, 0)); 10],
        },
        boards: boards,
        board_robots: board_robots,
        edge_border: ColorValue(edge_border),
        starting_board_number: BoardId(starting_board_number),
        end_game_board: BoardId(end_game_board),
        death_board: BoardId(death_board),
        end_game_pos: Coordinate(end_game_x, end_game_y),
        game_over_sfx: game_over_sfx,
        death_pos: Coordinate(death_x, death_y),
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
