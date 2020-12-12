extern crate byteorder;
#[macro_use]
extern crate enum_primitive;
#[macro_use]
extern crate enum_primitive_derive;
extern crate itertools;
#[macro_use]
extern crate log;
extern crate num_traits;
extern crate rand;

pub mod audio;
pub mod board;
mod expression;
mod render;
pub mod robot;
mod robotic;
mod world;

use self::expression::{CounterContextExt, CounterContextMutExt};
pub use self::render::{draw_messagebox, render, MessageBoxLine, Renderer};
use self::robotic::Condition;
pub use self::robotic::{
    Command, ExtendedColorValue, ExtendedParam, MessageBoxLineType, ModifiedDirection, Operator,
    RelativePart, Resolve, SignedNumeric,
};

use self::robotic::parse_program;
use self::robot::{Robots, RobotId};
use byteorder::{ByteOrder, LittleEndian};
use itertools::Zip;
use num_traits::{FromPrimitive, ToPrimitive};
use std::cmp::{Ordering, PartialOrd};
use std::collections::HashMap;
use std::default::Default;
use std::fmt;
use std::ops::Deref;
use std::str;

#[derive(Copy, Clone, Debug, Default, PartialEq)]
pub struct BoardId(pub u8);
#[derive(Copy, Clone, Debug, Default, PartialEq)]
pub struct ColorValue(pub u8);
#[derive(Copy, Clone, Debug, PartialEq)]
pub struct ParamValue(pub u8);
#[derive(Copy, Clone, Debug, Default, PartialEq)]
pub struct Coordinate<T>(pub T, pub T);
#[derive(Copy, Clone, Debug, Default)]
pub struct Size<T>(pub T, pub T);

const LEGACY_WORLD_VERSION: u32 = 0x0254;

#[derive(Default)]
pub struct World {
    pub version: u32,
    pub title: ByteString,
    pub state: WorldState,
    pub boards: Vec<(Board, Vec<Robot>)>,
    pub global_robot: Robot,
    pub edge_border: ColorValue,
    pub starting_board_number: BoardId,
    pub end_game_board: BoardId,
    pub death_board: BoardId,
    pub end_game_pos: Coordinate<u16>,
    pub game_over_sfx: bool,
    pub death_pos: Coordinate<u16>,
    pub starting_lives: u16,
    pub starting_health: u16,
    pub enemies_hurt_enemies: bool,
    pub clear_messages_and_projectiles: bool,
    pub only_play_via_swap_world: bool,
}

pub struct WorldState {
    pub charset: Charset,
    pub initial_charset: Charset,
    pub palette: Palette,
    pub initial_palette: Palette,
    pub idchars: Box<[u8]>,
    pub saved_positions: [(usize, Coordinate<u16>); 10],
    pub scroll_locked: bool,
    pub message_edge: bool,
    pub message_color: u8,
    pub player_face_dir: i32,
    pub key_pressed: i32,
    pub health: i32,
    pub lives: i32,
    pub keys: u32,
    pub ammo: i32,
    pub gems: i32,
    pub lobombs: i32,
    pub hibombs: i32,
    pub coins: i32,
    pub score: i32,
    pub time: i32,
    pub limit_lives: i32,
    pub limit_health: i32,
    pub update_done: Vec<bool>, // FIXME: this belongs in mzxplay, not libmzx
}

impl Default for WorldState {
    fn default() -> WorldState {
        WorldState {
            charset: Default::default(),
            initial_charset: Default::default(),
            palette: Default::default(),
            initial_palette: Default::default(),
            idchars: vec![0; 455].into_boxed_slice(),
            saved_positions: [(0, Coordinate(0, 0)); 10],
            scroll_locked: false,
            message_edge: true,
            message_color: 0,
            player_face_dir: 1,
            key_pressed: 0,
            health: 100,
            lives: 3,
            keys: 0,
            ammo: 0,
            gems: 0,
            lobombs: 0,
            hibombs: 0,
            coins: 0,
            score: 0,
            time: 0,
            limit_health: 32767,
            limit_lives: 99,
            update_done: vec![],
        }
    }
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

    pub fn char_id_cycle(&self, start: CharId, cur: u8, end: CharId) -> u8 {
        let len = end.to_u8().unwrap() - start.to_u8().unwrap();
        assert!(cur < len);
        (cur + 1) % len
    }

    pub fn take_key(&mut self, color: u8) -> Result<(), ()> {
        let has_key = self.keys & (1 << color) != 0;
        self.keys &= !(1 << color);
        if has_key {
            Ok(())
        } else {
            Err(())
        }
    }

    pub fn give_key(&mut self, color: u8) -> Result<(), ()> {
        let has_key = self.keys & (1 << color) != 0;
        self.keys |= 1 << color;
        if has_key {
            Err(())
        } else {
            Ok(())
        }
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
    pub exits: (
        Option<BoardId>,
        Option<BoardId>,
        Option<BoardId>,
        Option<BoardId>,
    ),
    pub restart_when_zapped: bool,
    pub time_limit: u16,
    pub scrolls: Vec<Scroll>,
    pub sensors: Vec<Sensor>,
    pub player_pos: Coordinate<u16>,
    pub scroll_offset: Coordinate<u16>,
    pub message_line: ByteString,
    pub message_row: u8,
    pub message_col: Option<u8>,
    pub remaining_message_cycles: u8,
    pub player_locked_ns: bool,
    pub player_locked_ew: bool,
    pub player_locked_attack: bool,
    pub num_robots: usize,
}

impl Default for Board {
    fn default() -> Board {
        Board {
            title: ByteString::from(""),
            width: 0,
            height: 0,
            overlay: None,
            level: vec![],
            under: vec![],
            mod_file: String::new(),
            upper_left_viewport: Coordinate(0, 0),
            viewport_size: Size(0, 0),
            can_shoot: false,
            can_bomb: false,
            fire_burns_brown: false,
            fire_burns_space: false,
            fire_burns_fakes: false,
            fire_burns_trees: false,
            explosion_result: ExplosionResult::Nothing,
            forest_becomes_floor: false,
            save_restriction: SaveRestriction::Unrestricted,
            collect_bombs: false,
            fire_burns_forever: false,
            exits: (None, None, None, None),
            restart_when_zapped: false,
            time_limit: 0,
            scrolls: vec![],
            sensors: vec![],
            player_pos: Coordinate(0, 0),
            scroll_offset: Coordinate(0, 0),
            message_line: ByteString::from(""),
            message_row: 24,
            message_col: None,
            remaining_message_cycles: 0,
            player_locked_ns: false,
            player_locked_ew: false,
            player_locked_attack: false,
            num_robots: 0,
        }
    }
}

impl Board {
    fn init(&mut self, robots: &mut [Robot]) {
        for (idx, &(thing, _, param)) in self.level.iter().enumerate() {
            if thing == Thing::Robot.to_u8().unwrap() {
                robots[param as usize - 1].position =
                    Coordinate((idx % self.width) as u16, (idx / self.width) as u16);
            } else if thing == Thing::Player.to_u8().unwrap() {
                self.player_pos = Coordinate((idx % self.width) as u16, (idx / self.width) as u16);
            }
        }
    }

    fn whirlpools_match(_id: u8, other: u8) -> bool {
        other >= Thing::Whirlpool1 as u8 && other <= Thing::Whirlpool4 as u8
    }

    fn ids_match(id: u8, other: u8) -> bool {
        id == other
    }

    pub fn find(&self, id: u8, color: u8) -> Option<Coordinate<u16>> {
        let id_match = if id >= Thing::Whirlpool1 as u8 && id <= Thing::Whirlpool4 as u8 {
            Board::whirlpools_match
        } else {
            Board::ids_match
        };

        for (idx, (&(level_thing, level_color, _), &(under_thing, under_color, _))) in
            self.level.iter().zip(self.under.iter()).enumerate()
        {
            if (id_match(id, level_thing) && level_color == color)
                || (id_match(id, under_thing) && under_color == color)
            {
                return Some(Coordinate(
                    (idx % self.width) as u16,
                    (idx / self.width) as u16,
                ));
            }
        }
        None
    }

    pub fn find_extended(
        &self,
        id: u8,
        color: ExtendedColorValue,
        param: ExtendedParam,
    ) -> Option<Coordinate<u16>> {
        for (
            idx,
            (&(level_thing, level_color, level_param), &(under_thing, under_color, under_param)),
        ) in self.level.iter().zip(self.under.iter()).enumerate()
        {
            if (level_thing == id
                && color.matches(ColorValue(level_color))
                && param.matches(ParamValue(level_param)))
                || (under_thing == id
                    && color.matches(ColorValue(under_color))
                    && param.matches(ParamValue(under_param)))
            {
                return Some(Coordinate(
                    (idx % self.width) as u16,
                    (idx / self.width) as u16,
                ));
            }
        }
        None
    }

    pub fn thing_at(&self, pos: &Coordinate<u16>) -> Thing {
        Thing::from_u8(self.level_at(pos).0).expect("invalid thing value")
    }

    pub fn under_thing_at(&self, pos: &Coordinate<u16>) -> Thing {
        Thing::from_u8(self.under_at(pos).0).expect("invalid thing value")
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

    pub fn copy(&mut self, src: Coordinate<u16>, block: Size<u16>, dest: Coordinate<u16>, robots: &mut Robots) {
        let mut yiter = if src.1 > dest.1 {
            Box::new(0..block.1) as Box<dyn Iterator<Item = u16>>
        } else {
            Box::new((0..block.1).rev()) as Box<dyn Iterator<Item = u16>>
        };

        while let Some(j) = yiter.next() {
            let mut xiter = if src.0 > dest.0 {
                Box::new(0..block.0) as Box<dyn Iterator<Item = u16>>
            } else {
                Box::new((0..block.0).rev()) as Box<dyn Iterator<Item = u16>>
            };
            while let Some(i) = xiter.next() {
                let src_coord = Coordinate(src.0 + i, src.1 + j);
                let dest_coord = Coordinate(dest.0 + i, dest.1 + j);
                if src_coord.0 as usize > self.width
                    || dest_coord.0 as usize > self.width
                    || src_coord.1 as usize > self.height
                    || dest_coord.1 as usize > self.height
                {
                    continue;
                }

                let src = *self.level_at(&src_coord);
                *self.level_at_mut(&dest_coord) = src;
                if self.thing_at(&dest_coord).is_robot() {
                    let new_id = robots.duplicate_self(RobotId::from(src.2), dest_coord);
                    self.level_at_mut(&dest_coord).2 = new_id.to_param().unwrap();
                }
                let src = *self.under_at(&src_coord);
                *self.under_at_mut(&dest_coord) = src;
            }
        }
    }

    pub fn copy_overlay(&mut self, src: Coordinate<u16>, block: Size<u16>, dest: Coordinate<u16>) {
        let overlay = match self.overlay {
            Some((_mode, ref mut overlay)) => overlay,
            None => return,
        };

        let mut yiter = if src.1 > dest.1 {
            Box::new(0..block.1) as Box<dyn Iterator<Item = u16>>
        } else {
            Box::new((0..block.1).rev()) as Box<dyn Iterator<Item = u16>>
        };

        while let Some(j) = yiter.next() {
            let mut xiter = if src.0 > dest.0 {
                Box::new(0..block.0) as Box<dyn Iterator<Item = u16>>
            } else {
                Box::new((0..block.0).rev()) as Box<dyn Iterator<Item = u16>>
            };
            while let Some(i) = xiter.next() {
                let src_coord = Coordinate(src.0 + i, src.1 + j);
                let dest_coord = Coordinate(dest.0 + i, dest.1 + j);
                if src_coord.0 as usize >= self.width
                    || dest_coord.0 as usize >= self.width
                    || src_coord.1 as usize >= self.height
                    || dest_coord.1 as usize >= self.height
                {
                    continue;
                }

                let src = overlay[src_coord.1 as usize * self.width + src_coord.0 as usize];
                overlay[dest_coord.1 as usize * self.width + dest_coord.0 as usize] = src;
            }
        }
    }

    pub fn move_level(
        &mut self,
        robots: &mut [Robot],
        pos: &Coordinate<u16>,
        xdiff: i8,
        ydiff: i8,
    ) {
        self.move_level_to(
            robots,
            pos,
            &Coordinate(
                (pos.0 as i16 + xdiff as i16) as u16,
                (pos.1 as i16 + ydiff as i16) as u16,
            ),
        );
    }

    pub fn move_level_to(
        &mut self,
        robots: &mut [Robot],
        pos: &Coordinate<u16>,
        new_pos: &Coordinate<u16>,
    ) {
        let thing = Thing::from_u8(self.level_at(pos).0).unwrap();
        if thing.is_robot() {
            let id = self.level_at(pos).2;
            // FIXME: don't do this calculation directly.
            robots[id as usize - 1].position = *new_pos;
        } else if thing == Thing::Player {
            self.player_pos = *new_pos;
        }
        let (old_thing, _, old_param) = self.level_at(new_pos);
        let old_thing = Thing::from_u8(*old_thing).unwrap();
        let old_idx = (pos.1 * self.width as u16 + pos.0) as usize;
        let new_idx = (new_pos.1 * self.width as u16 + new_pos.0) as usize;
        if old_thing.can_go_under() {
            self.under[new_idx] = self.level[new_idx];
        } else if old_thing.is_robot() {
            // FIXME: don't do this calculation directly.
            robots[*old_param as usize - 1].alive = false;
        }
        self.level[new_idx] = self.level[old_idx];
        self.level[old_idx] = self.under[old_idx];
    }

    pub fn put_at(&mut self, pos: &Coordinate<u16>, thing: u8, color: u8, param: u8) {
        let idx = (pos.1 * self.width as u16 + pos.0) as usize;
        if self.thing_at(pos).can_go_under() {
            self.under[idx] = self.level[idx];
        }
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

        // Can't write beyond boundaries of board.
        if pos.0 as usize >= self.width || pos.1 as usize >= self.height {
            return;
        }

        let end_x = pos.0 as usize + s.len();
        let bytes = if end_x > self.width {
            &s[0..(end_x - self.width)]
        } else {
            &s[..]
        };

        let start_idx = (pos.1 * self.width as u16 + pos.0) as usize;
        for (ch, (och, ocol)) in bytes
            .iter()
            .zip(&mut overlay[start_idx..start_idx + bytes.len()])
        {
            *och = *ch;
            *ocol = color;
        }
    }

    pub fn set_message_line(&mut self, line: ByteString) {
        self.message_line = line;
        self.remaining_message_cycles = 100;
    }
}

#[derive(Copy, Clone)]
pub struct CounterContext<'a> {
    board: &'a Board,
    robot: &'a Robot,
    state: &'a WorldState,
}

impl<'a> CounterContext<'a> {
    pub fn from(board: &'a Board, robot: &'a Robot, state: &'a WorldState) -> CounterContext<'a> {
        CounterContext {
            board,
            robot,
            state,
        }
    }

    fn string_counter(&self, counter: StringCounter) -> ByteString {
        match counter {
            StringCounter::BoardName => self.board.title.clone(),
            StringCounter::RobotName => self.robot.name.clone(),
            StringCounter::ModName => self.board.mod_file.clone().into(),
        }
    }

    fn numeric_counter(&self, counter: NumericCounter) -> i32 {
        counter.eval(self)
    }

    fn local_counter(&self, counter: LocalCounter) -> i32 {
        match counter {
            LocalCounter::Loopcount => self.robot.loop_count,
            LocalCounter::Local(n) => self.robot.local[n as usize],
            LocalCounter::Lavawalk => self.robot.lavawalking,
            LocalCounter::HorizPld => {
                (self.robot.position.0 as i32 - self.board.player_pos.0 as i32).abs() as i32
            }
            LocalCounter::VertPld => {
                (self.robot.position.1 as i32 - self.board.player_pos.1 as i32).abs() as i32
            }
            LocalCounter::PlayerDist => {
                self.local_counter(LocalCounter::HorizPld)
                    + self.local_counter(LocalCounter::VertPld)
            }
            LocalCounter::ThisX => self.robot.position.0 as i32,
            LocalCounter::ThisY => self.robot.position.1 as i32,
            LocalCounter::BulletType => self.robot.bullet_type,
            LocalCounter::ThisColor => self.board.level_at(&self.robot.position).1 as i32,
            LocalCounter::ThisChar => self.robot.ch as i32,
        }
    }
}

pub struct CounterContextMut<'a> {
    board: &'a mut Board,
    robot: &'a mut Robot,
    state: &'a mut WorldState,
}

impl<'a> CounterContextMut<'a> {
    pub fn from(
        board: &'a mut Board,
        robot: &'a mut Robot,
        state: &'a mut WorldState,
    ) -> CounterContextMut<'a> {
        CounterContextMut {
            board,
            robot,
            state,
        }
    }

    pub fn as_immutable(&self) -> CounterContext {
        CounterContext {
            board: self.board,
            robot: self.robot,
            state: self.state,
        }
    }

    fn local_counter_mut(&mut self, counter: LocalCounter) -> Option<&mut i32> {
        match counter {
            LocalCounter::Loopcount => Some(&mut self.robot.loop_count),
            LocalCounter::Local(n) => Some(&mut self.robot.local[n as usize]),
            LocalCounter::Lavawalk => Some(&mut self.robot.lavawalking),
            LocalCounter::BulletType => Some(&mut self.robot.bullet_type),
            /*LocalCounter::PlayerFaceDir => Some(&mut self.state.player_face_dir),
            LocalCounter::Health => Some(&mut self.state.health),
            LocalCounter::Lives => Some(&mut self.state.lives),
            LocalCounter::Ammo => Some(&mut self.state.ammo),*/
            LocalCounter::HorizPld
            | LocalCounter::VertPld
            | LocalCounter::PlayerDist
            | LocalCounter::ThisX
            | LocalCounter::ThisY
            | LocalCounter::ThisColor
            | LocalCounter::ThisChar
            => None,
        }
    }
}

pub struct Counters {
    counters: HashMap<ByteString, i32>,
    strings: HashMap<ByteString, ByteString>,
}

impl Counters {
    pub fn new() -> Counters {
        Counters {
            counters: HashMap::new(),
            strings: HashMap::new(),
        }
    }

    pub fn set_string(
        &mut self,
        name: ByteString,
        context: &mut dyn CounterContextMutExt,
        value: ByteString,
    ) {
        //TODO: handle #N, +N, .N suffixes
        assert!(
            name.is_string_name(),
            "Setting non-string {:?} to \"{:?}\"",
            name,
            value
        );
        // Skip the leading $ when evaluating the string name.
        let name = ByteString(
            name.as_bytes()
                .iter()
                .skip(1)
                .map(|c| c.to_ascii_lowercase())
                .collect(),
        );
        let mut name = name.evaluate(self, context.as_immutable());
        name.0.insert(0, b'$');
        debug!("setting {:?} to \"{:?}\"", name, value);
        self.strings.insert(name, value);
    }

    pub fn set(&mut self, name: ByteString, context: &mut dyn CounterContextMutExt, value: i32) {
        if name.is_string_name() {
            error!("Setting counter {:?} to integer value", name);
        }
        let name = ByteString(
            name.as_bytes()
                .iter()
                .map(|c| c.to_ascii_lowercase())
                .collect(),
        );
        let name = name.evaluate(self, context.as_immutable());
        debug!("setting {:?} to {}", name, value);
        if let Some(local) = LocalCounter::from(&name) {
            if let Some(counter) = context.local_counter_mut(local) {
                debug!("setting local counter");
                *counter = value;
            }
        } else {
            self.counters.insert(name, value);
        }
    }

    pub fn get(&self, name: &ByteString, context: &dyn CounterContextExt) -> i32 {
        debug!("getting counter value for {}", name.to_string());
        if name.is_string_name() {
            error!("Getting string {:?} as integer value", name);
        }
        let name = ByteString(
            name.as_bytes()
                .iter()
                .map(|c| c.to_ascii_lowercase())
                .collect(),
        );
        let result = name.evaluate(self, context);
        if name.as_bytes().first() == Some(&b'(') {
            debug!("return {} as integer value", result.to_string());
            if let Ok(val) = result.to_string().parse::<i32>() {
                return val;
            }
        }

        let v = if let Some(local) = LocalCounter::from(&result) {
            debug!("getting local counter {:?}", local);
            context.local_counter(local)
        } else if let Some(counter) = NumericCounter::from(&result, self, context) {
            debug!("getting special counter {:?}", counter);
            context.numeric_counter(counter)
        } else {
            *self.counters.get(&result).unwrap_or(&0)
        };
        debug!("getting {:?}: {}", result, v);
        v
    }

    pub fn get_string(&self, name: &ByteString, context: &dyn CounterContextExt) -> ByteString {
        //TODO: handle #N, +N, .N suffixes
        assert!(name.is_string_name(), "Getting non-string {:?}", name);
        // Get the name without the leading $
        let name = ByteString(
            name.as_bytes()
                .iter()
                .skip(1)
                .map(|c| c.to_ascii_lowercase())
                .collect(),
        );
        let mut result = name.evaluate(self, context);
        result.0.insert(0, b'$');
        debug!("getting string {:?}", result);
        self.strings
            .get(&result)
            .cloned()
            .unwrap_or(ByteString::default())
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

impl From<ByteString> for Vec<u8> {
    fn from(bytes: ByteString) -> Vec<u8> {
        bytes.0
    }
}

impl<'a> From<&'a str> for ByteString {
    fn from(v: &'a str) -> ByteString {
        ByteString(v.as_bytes().to_vec())
    }
}

impl<'a> From<String> for ByteString {
    fn from(v: String) -> ByteString {
        ByteString(v.into_bytes())
    }
}

impl<'a> From<&[u8]> for ByteString {
    fn from(v: &[u8]) -> ByteString {
        ByteString(v.to_vec())
    }
}

impl From<Vec<u8>> for ByteString {
    fn from(v: Vec<u8>) -> ByteString {
        ByteString(v)
    }
}

impl PartialEq for ByteString {
    fn eq(&self, other: &ByteString) -> bool {
        self.as_bytes().len() == other.as_bytes().len()
            && self
                .as_bytes()
                .iter()
                .zip(other.as_bytes().iter())
                .all(|(a, b)| a.to_ascii_lowercase() == b.to_ascii_lowercase())
    }
}

impl PartialEq<&str> for ByteString {
    fn eq(&self, other: &&str) -> bool {
        self.as_bytes().len() == other.as_bytes().len()
            && self
                .as_bytes()
                .iter()
                .zip(other.as_bytes().iter())
                .all(|(a, b)| a.to_ascii_lowercase() == b.to_ascii_lowercase())
    }
}

impl PartialOrd for ByteString {
    fn partial_cmp(&self, other: &ByteString) -> Option<Ordering> {
        match self.0.len().partial_cmp(&other.0.len()) {
            Some(Ordering::Equal) => {
                for (a, b) in self.as_bytes().iter().zip(other.as_bytes().iter()) {
                    match a.partial_cmp(b) {
                        Some(Ordering::Equal) => {}
                        c => return c,
                    }
                }
                Some(Ordering::Equal)
            }
            c => c,
        }
    }
}

impl ByteString {
    pub fn case_sensitive_eq(&self, other: &ByteString) -> bool {
        self.as_bytes().len() == other.as_bytes().len()
            && self
                .as_bytes()
                .iter()
                .zip(other.as_bytes().iter())
                .all(|(a, b)| a == b)
    }

    pub fn is_string_name(&self) -> bool {
        self.as_bytes().first() == Some(&b'$')
    }

    pub fn as_bytes(&self) -> &[u8] {
        self.0.as_ref()
    }

    pub fn into_string(self) -> String {
        self.0.into_iter().map(|c| c as char).collect()
    }

    pub fn to_string(&self) -> String {
        self.clone().into_string()
    }

    pub fn text_len(&self) -> usize {
        let mut len = 0;
        let mut maybe_skip_next = false;
        for &c in self.as_bytes() {
            if maybe_skip_next {
                maybe_skip_next = false;
                if (c >= b'0' && c <= b'9') || (c >= b'A' && c <= b'F') || (c >= b'a' && c <= b'f')
                {
                    continue;
                }
            }
            if c == b'~' || c == b'@' {
                maybe_skip_next = true;
            } else {
                len += 1;
            }
        }
        len
    }

    pub fn color_text(&self) -> ColorStringIterator {
        ColorStringIterator {
            string: self,
            index: 0,
            fg: None,
            bg: None,
            state: ColorParserState::Text,
        }
    }

    pub fn evaluate(&self, counters: &Counters, context: &dyn CounterContextExt) -> ByteString {
        let result = self.evaluate_for_name(counters, context);
        if let Some(name) = StringCounter::from(&result) {
            return context.string_counter(name);
        }
        if result.is_string_name() {
            return counters.get_string(&result, context);
        }
        result
    }

    pub fn evaluate_for_name(
        &self,
        counters: &Counters,
        context: &dyn CounterContextExt,
    ) -> ByteString {
        let bytes = self.as_bytes();
        let mut new_bytes = vec![];
        let mut start = None;
        let mut expressions = vec![];
        for (idx, &c) in bytes.iter().enumerate() {
            if c == b'(' {
                expressions.push(vec![]);
                continue;
            } else if c == b')' {
                if let Some(expression) = expressions.pop() {
                    let result =
                        match expression::evaluate_expression(&expression, counters, context) {
                            Ok(result) => result,
                            Err(input) => {
                                let mut result = b"(".to_vec();
                                result.extend(input.0.into_iter());
                                result.extend(&b")"[..]);
                                ByteString(result)
                            }
                        };
                    let cur_bytes = expressions.last_mut().unwrap_or(&mut new_bytes);
                    cur_bytes.extend(result.as_bytes());
                    continue;
                }
            }

            let cur_bytes = expressions.last_mut().unwrap_or(&mut new_bytes);

            if c == b'&' {
                if let Some(start_idx) = start {
                    let name = ByteString(bytes[start_idx..idx].to_owned());
                    let value = if name.is_string_name() {
                        counters.get_string(&name, context).into()
                    } else {
                        counters.get(&name, context).to_string().into_bytes()
                    };
                    cur_bytes.extend(&value);
                    start = None;
                } else {
                    start = Some(idx + 1);
                }
            } else if start.is_none() {
                cur_bytes.push(c);
            }
        }
        //TODO: handle outstanding expression bytes

        ByteString(new_bytes)
    }
}

pub struct ColorStringIterator<'a> {
    string: &'a ByteString,
    index: usize,
    fg: Option<u8>,
    bg: Option<u8>,
    state: ColorParserState,
}

#[derive(PartialEq)]
enum ColorParserState {
    Text,
    Foreground,
    Background,
}

impl<'a> Iterator for ColorStringIterator<'a> {
    type Item = (&'a [u8], Option<u8>, Option<u8>);
    fn next(&mut self) -> Option<Self::Item> {
        let mut start = self.index;
        let bytes = self.string.as_bytes();
        while self.index < bytes.len() {
            let byte = bytes[self.index];
            match self.state {
                ColorParserState::Text => {}
                ColorParserState::Foreground | ColorParserState::Background => {
                    let var = if self.state == ColorParserState::Foreground {
                        &mut self.fg
                    } else {
                        &mut self.bg
                    };
                    let skip = {
                        if byte >= b'0' && byte <= b'9' {
                            *var = Some(byte - b'0');
                            true
                        } else if byte >= b'A' && byte <= b'F' {
                            *var = Some(byte - b'A' + 10);
                            true
                        } else if byte >= b'a' && byte <= b'f' {
                            *var = Some(byte - b'a' + 10);
                            true
                        } else {
                            false
                        }
                    };
                    start = if skip { self.index + 1 } else { self.index };
                }
            }
            self.state = ColorParserState::Text;
            if byte == b'~' || byte == b'@' {
                if self.index != start {
                    return Some((&bytes[start..self.index], self.bg, self.fg));
                } else if byte == b'~' {
                    self.state = ColorParserState::Foreground;
                } else {
                    self.state = ColorParserState::Background;
                }
            }
            self.index += 1;
        }
        if self.index != start {
            Some((&bytes[start..self.index], self.bg, self.fg))
        } else {
            None
        }
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

pub fn adjust_coordinate(
    coord: Coordinate<u16>,
    board: &Board,
    dir: CardinalDirection,
) -> Option<Coordinate<u16>> {
    let (xdiff, ydiff) = match dir {
        CardinalDirection::North => (0, -1),
        CardinalDirection::South => (0, 1),
        CardinalDirection::East => (1, 0),
        CardinalDirection::West => (-1, 0),
    };
    adjust_coordinate_diff(coord, board, xdiff, ydiff)
}

pub fn adjust_coordinate_diff(
    coord: Coordinate<u16>,
    board: &Board,
    xdiff: i16,
    ydiff: i16,
) -> Option<Coordinate<u16>> {
    if (coord.0 as i16 + xdiff < 0)
        || ((coord.0 as i16 + xdiff) as usize >= board.width)
        || (coord.1 as i16 + ydiff < 0)
        || ((coord.1 as i16 + ydiff) as usize >= board.height)
    {
        return None;
    }
    Some(Coordinate(
        (coord.0 as i16 + xdiff) as u16,
        (coord.1 as i16 + ydiff) as u16,
    ))
}

pub enum RelativeDirBasis {
    Robot(Coordinate<u16>, Option<CardinalDirection>),
    Player(Coordinate<u16>),
}

impl RelativeDirBasis {
    fn flow(&self) -> Option<CardinalDirection> {
        match *self {
            RelativeDirBasis::Robot(_, ref flow) => flow.clone(),
            RelativeDirBasis::Player(..) => None,
        }
    }

    pub fn from_robot(robot: &Robot) -> RelativeDirBasis {
        RelativeDirBasis::Robot(robot.position, robot.walk.clone())
    }

    pub fn from_player(board: &Board) -> RelativeDirBasis {
        RelativeDirBasis::Player(board.player_pos)
    }
}

pub fn dir_to_cardinal_dir(robot: &Robot, dir: &ModifiedDirection) -> Option<CardinalDirection> {
    dir_to_cardinal_dir_rel(
        RelativeDirBasis::Robot(robot.position, robot.walk.clone()),
        dir,
    )
}

pub fn dir_to_cardinal_dir_rel(
    basis: RelativeDirBasis,
    dir: &ModifiedDirection,
) -> Option<CardinalDirection> {
    // TODO: blocked, not blocked, etc.
    let resolved = match dir.dir {
        Direction::North => Some(CardinalDirection::North),
        Direction::South => Some(CardinalDirection::South),
        Direction::East => Some(CardinalDirection::East),
        Direction::West => Some(CardinalDirection::West),
        Direction::Idle | Direction::NoDir => None,
        Direction::Flow => basis.flow(),
        Direction::RandNs => Some(if rand::random::<bool>() == true {
            CardinalDirection::North
        } else {
            CardinalDirection::South
        }),
        Direction::RandNe => Some(if rand::random::<bool>() == true {
            CardinalDirection::North
        } else {
            CardinalDirection::East
        }),
        Direction::RandEw => Some(if rand::random::<bool>() == true {
            CardinalDirection::East
        } else {
            CardinalDirection::West
        }),
        Direction::RandAny => Some(match rand::random::<u8>() % 4 {
            0 => CardinalDirection::North,
            1 => CardinalDirection::South,
            2 => CardinalDirection::East,
            3 => CardinalDirection::West,
            _ => unreachable!(),
        }),
        Direction::Anydir
        | Direction::Seek
        | Direction::Beneath
        | Direction::RandB
        | Direction::RandNb => None, //TODO
    };
    // TODO: cw, random perpendicular, randnot
    if dir.opp {
        resolved.map(|d| match d {
            CardinalDirection::North => CardinalDirection::South,
            CardinalDirection::South => CardinalDirection::North,
            CardinalDirection::East => CardinalDirection::West,
            CardinalDirection::West => CardinalDirection::East,
        })
    } else {
        resolved
    }
}

#[derive(Copy, Clone, Debug, PartialEq, Primitive)]
pub enum CardinalDirection {
    North = 1,
    South = 2,
    East = 3,
    West = 4,
}

impl Default for CardinalDirection {
    fn default() -> CardinalDirection {
        CardinalDirection::North
    }
}

#[derive(Copy, Clone, Debug, Primitive, PartialEq)]
pub enum CharId {
    Space = 0,
    Normal = 1,
    Solid = 2,
    Tree = 3,
    Breakaway = 6,
    Boulder = 8,
    Crate = 9,
    Box = 10,
    Fake = 13,
    Carpet = 14,
    Floor = 15,
    Tiles = 16,
    StillWater = 20,
    NorthWater = 21,
    SouthWater = 22,
    EastWater = 23,
    WestWater = 24,
    Chest = 27,
    Gem = 28,
    MagicGem = 29,
    Health = 30,
    Ring = 31,
    Potion = 32,
    Energizer = 33,
    Goop = 34,
    Bomb = 36,
    Explosion = 38,
    Key = 39,
    Lock = 40,
    Stairs = 43,
    Cave = 44,
    Gate = 47,
    OpenGate = 48,
    Coin = 50,
    Pouch = 55,
    SliderNS = 57,
    SliderEW = 58,
    LazerGun = 60,
    Forest = 65,
    Whirlpool1 = 67,
    Whirlpool2 = 68,
    Whirlpool3 = 69,
    Whirlpool4 = 70,
    InvisibleWall = 71,
    Ricochet = 73,
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
    Bear = 94,
    BearCub = 95,
    Sign = 125,
    Scroll = 126,
    BlankIce = 160,
    IceAnim1 = 161,
    IceAnim2 = 162,
    IceAnim3 = 163,
    LavaAnim1 = 164,
    LavaAnim2 = 165,
    LavaAnim3 = 166,
    SmallAmmo = 167,
    LargeAmmo = 168,
    LitBombAnim1 = 169,
    EnergizerColor1 = 176,
    EnergizerColor2 = 177,
    EnergizerColor3 = 178,
    EnergizerColor4 = 179,
    EnergizerColor5 = 180,
    EnergizerColor6 = 181,
    EnergizerColor7 = 182,
    EnergizerColor8 = 183,
    ExplosionStage1 = 184,
    ExplosionStage2 = 185,
    ExplosionStage3 = 186,
    ExplosionStage4 = 187,
    HorizontalDoor = 188,
    VerticalDoor = 189,
    CwAnim1 = 190,
    CwAnim2 = 191,
    CwAnim3 = 192,
    CwAnim4 = 193,
    CcwAnim1 = 194,
    CcwAnim2 = 195,
    CcwAnim3 = 196,
    CcwAnim4 = 197,
    OpenDoorStart = 198,
    NTransportAnim1 = 230,
    NTransportAnim2 = 231,
    NTransportAnim3 = 232,
    NTransportAnim4 = 233,
    STransportAnim1 = 234,
    STransportAnim2 = 235,
    STransportAnim3 = 236,
    STransportAnim4 = 237,
    ETransportAnim1 = 238,
    ETransportAnim2 = 239,
    ETransportAnim3 = 240,
    ETransportAnim4 = 241,
    WTransportAnim1 = 242,
    WTransportAnim2 = 243,
    WTransportAnim3 = 244,
    WTransportAnim4 = 245,
    AnyTransportAnim1 = 246,
    AnyTransportAnim2 = 247,
    AnyTransportAnim3 = 248,
    AnyTransportAnim4 = 249,
    NThickArrow = 250,
    SThickArrow = 251,
    EThickArrow = 252,
    WThickArrow = 253,
    NThinArrow = 254,
    SThinArrow = 255,
    EThinArrow = 256,
    WThinArrow = 257,
    HorizontalLazerAnim1 = 258,
    HorizontalLazerAnim2 = 259,
    HorizontalLazerAnim3 = 260,
    HorizontalLazerAnim4 = 261,
    VerticalLazerAnim1 = 262,
    VerticalLazerAnim2 = 263,
    VerticalLazerAnim3 = 264,
    VerticalLazerAnim4 = 265,
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
    LifeAnim1 = 278,
    LifeAnim2 = 279,
    LifeAnim3 = 280,
    LifeAnim4 = 281,
    LifeColor1 = 282,
    LifeColor2 = 283,
    LifeColor3 = 284,
    LifeColor4 = 285,
    RicochetPanel1 = 286,
    RicochetPanel2 = 287,
    MineAnim1 = 288,
    MineAnim2 = 289,
    SpitFireAnim1 = 290,
    SpitFireAnim2 = 291,
    SpitFireColor1 = 292,
    SpitFireColor2 = 293,
    SeekerAnim1 = 294,
    SeekerAnim2 = 295,
    SeekerAnim3 = 296,
    SeekerAnim4 = 297,
    SeekerColor1 = 298,
    SeekerColor2 = 299,
    SeekerColor3 = 300,
    SeekerColor4 = 301,
    WhirlpoolColor1 = 302,
    WhirlpoolColor2 = 303,
    WhirlpoolColor3 = 304,
    WhirlpoolColor4 = 305,
    NPlayerBullet = 306,
    SPlayerBullet = 307,
    EPlayerBullet = 308,
    WPlayerBullet = 309,
    NNeutralBullet = 310,
    SNeutralBullet = 311,
    ENeutralBullet = 312,
    WNeutralBullet = 313,
    NEnemyBullet = 314,
    SEnemyBullet = 315,
    EEnemyBullet = 316,
    WEnemyBullet = 317,
    PlayerNorth = 318,
    PlayerSouth = 319,
    PlayerEast = 320,
    PlayerWest = 321,
    PlayerColor = 322,
    MissileColor = 323,
    PlayerBulletColor = 324,
    NeutralBulletColor = 325,
    EnemyBulletColor = 326,
}

pub enum DoorOrientation {
    Horizontal,
    Vertical,
}

pub enum DoorDir {
    OpensNW,
    OpensNE,
    OpensSW,
    OpensSE,
}

#[derive(PartialEq)]
pub enum DoorStatus {
    Unlocked,
    Locked,
}

pub fn param_from_door(orientation: DoorOrientation, dir: DoorDir, status: DoorStatus) -> u8 {
    (match status {
        DoorStatus::Locked => 1,
        DoorStatus::Unlocked => 0,
    }) << 3
        | (match dir {
            DoorDir::OpensNW => 0,
            DoorDir::OpensNE => 1,
            DoorDir::OpensSW => 2,
            DoorDir::OpensSE => 3,
        }) << 1
        | match orientation {
            DoorOrientation::Horizontal => 0,
            DoorOrientation::Vertical => 1,
        }
}

pub fn door_from_param(param: u8) -> (DoorOrientation, DoorDir, DoorStatus) {
    let orientation = if (param & 0b1) != 0 {
        DoorOrientation::Horizontal
    } else {
        DoorOrientation::Vertical
    };

    let dir = match (param & 0b110) >> 1 {
        0 => DoorDir::OpensNW,
        1 => DoorDir::OpensNE,
        2 => DoorDir::OpensSW,
        3 => DoorDir::OpensSE,
        v => unreachable!("door bits are 0b{:b}", v),
    };

    let status = if (param & 0b1000) != 0 {
        DoorStatus::Locked
    } else {
        DoorStatus::Unlocked
    };

    (orientation, dir, status)
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
    Boulder = 8,
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
    pub fn is_robot(&self) -> bool {
        *self == Thing::Robot || *self == Thing::RobotPushable
    }

    pub fn can_go_under(&self) -> bool {
        match self {
            Thing::Floor |
            Thing::CustomFloor |
            Thing::Carpet |
            Thing::Fake |
            Thing::Tiles |
            Thing::Forest | //XXXjdm: This is kind of a lie.
            Thing::Whirlpool1 |
            Thing::Whirlpool2 |
            Thing::Whirlpool3 |
            Thing::Whirlpool4 |
            Thing::Cave |
            Thing::Web |
            Thing::ThickWeb |
            Thing::Space |
            Thing::Normal |
            Thing::Stairs |
            Thing::StillWater |
            Thing::NWater |
            Thing::SWater |
            Thing::EWater |
            Thing::WWater |
            Thing::Ice |
            Thing::Fire |
            Thing::Lava => true,
            _ => false,
        }
    }

    pub fn is_whirlpool(&self) -> bool {
        [
            Thing::Whirlpool1,
            Thing::Whirlpool2,
            Thing::Whirlpool3,
            Thing::Whirlpool4,
        ]
        .iter()
        .any(|t| t == self)
    }

    pub fn is_teleporter(&self) -> bool {
        *self == Thing::Stairs || *self == Thing::Cave || self.is_whirlpool()
    }

    pub fn is_pushable(&self) -> bool {
        match *self {
            Thing::Crate
            | Thing::CustomPush
            | Thing::RobotPushable
            | Thing::Gem
            | Thing::MagicGem
            | Thing::Bomb
            | Thing::LitBomb
            | Thing::Key
            | Thing::Coin
            | Thing::Player => true,
            _ => false,
        }
    }

    pub fn is_solid(&self) -> bool {
        match *self {
            Thing::Normal
            | Thing::Solid
            | Thing::Tree
            | Thing::Line
            | Thing::CustomBlock
            | Thing::Breakaway
            | Thing::CustomBreak
            | Thing::Boulder
            | Thing::Crate
            | Thing::CustomPush
            | Thing::Box
            | Thing::CustomBox
            | Thing::Chest
            | Thing::Gem
            | Thing::MagicGem
            | Thing::Health
            | Thing::Ring
            | Thing::Potion
            | Thing::Energizer
            | Thing::Goop
            | Thing::Ammo
            | Thing::Bomb
            | Thing::LitBomb
            | Thing::Key
            | Thing::Lock
            | Thing::Door
            | Thing::Gate
            | Thing::Transport
            | Thing::Coin
            | Thing::Pouch
            | Thing::Pusher
            | Thing::SliderNS
            | Thing::SliderEW
            | Thing::LazerGun
            | Thing::Bullet
            | Thing::Missile
            | Thing::Life
            | Thing::InvisibleWall
            | Thing::Mine
            | Thing::Spike
            | Thing::CustomHurt
            | Thing::Text
            | Thing::Snake
            | Thing::Eye
            | Thing::Thief
            | Thing::SlimeBlob
            | Thing::Runner
            | Thing::Ghost
            | Thing::Dragon
            | Thing::Fish
            | Thing::Shark
            | Thing::Spider
            | Thing::Goblin
            | Thing::SpittingTiger
            | Thing::BulletGun
            | Thing::SpinningGun
            | Thing::Bear
            | Thing::BearCub
            | Thing::MissileGun
            | Thing::RobotPushable
            | Thing::Robot
            | Thing::Sign
            | Thing::Scroll
            | Thing::Player => true,
            _ => false,
        }
    }
}

pub enum BulletType {
    Player = 0,
    Neutral = 1,
    Enemy = 2,
}

impl From<i32> for BulletType {
    fn from(v: i32) -> BulletType {
        match v {
            0 => BulletType::Player,
            1 => BulletType::Neutral,
            _ => BulletType::Enemy,
        }
    }
}

pub fn bullet_param(type_: BulletType, dir: CardinalDirection) -> u8 {
    (match dir {
        CardinalDirection::North => 0,
        CardinalDirection::South => 1,
        CardinalDirection::East => 2,
        CardinalDirection::West => 3,
    }) | ((type_ as u8) << 2)
}

pub fn bullet_from_param(param: u8) -> (BulletType, CardinalDirection) {
    let type_ = BulletType::from((param >> 2) as i32);
    let dir = match param & 3 {
        0 => CardinalDirection::North,
        1 => CardinalDirection::South,
        2 => CardinalDirection::East,
        3 => CardinalDirection::West,
        _ => unreachable!(),
    };
    (type_, dir)
}

#[derive(Debug, Copy, Clone, PartialEq)]
pub enum OverlayMode {
    Normal,
    Static,
    Transparent,
}

impl OverlayMode {
    pub(crate) fn from_byte(b: u8) -> Result<OverlayMode, ()> {
        Ok(match b {
            1 => OverlayMode::Normal,
            2 => OverlayMode::Static,
            3 => OverlayMode::Transparent,
            _ => return Err(()),
        })
    }
}

#[derive(Debug, PartialEq)]
pub enum ExplosionResult {
    Nothing,
    Ash,
    Fire,
}

impl Default for ExplosionResult {
    fn default() -> ExplosionResult {
        ExplosionResult::Nothing
    }
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
        let stage = (p & 0x0F).min(3);
        let size = ((p & 0xF0) >> 4).min(15);
        Explosion { stage, size }
    }
}

#[derive(Debug)]
pub enum SaveRestriction {
    Unrestricted,
    NoSave,
    OnlyOnSensor,
}

impl Default for SaveRestriction {
    fn default() -> Self {
        SaveRestriction::Unrestricted
    }
}

pub const CHAR_BYTES: usize = 14;
pub(crate) const CHARSET_BUFFER_SIZE: usize = CHAR_BYTES * 256;

#[derive(Copy, Clone, Debug)]
pub struct Charset {
    pub data: [u8; CHARSET_BUFFER_SIZE],
}

impl Default for Charset {
    fn default() -> Charset {
        Charset {
            data: [0; CHARSET_BUFFER_SIZE],
        }
    }
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

#[derive(Copy, Clone, Debug, Default)]
pub struct Color {
    pub r: u8,
    pub g: u8,
    pub b: u8,
}

#[derive(Clone, Default)]
pub struct Palette {
    pub colors: Vec<(Color, f32)>,
}

pub(crate) fn load_palette(palette_color_data: &[u8]) -> Palette {
    let mut colors = vec![];
    for rgb in palette_color_data.chunks(3) {
        assert!(rgb.iter().all(|&b| b < 64u8));
        colors.push((
            Color {
                r: rgb[0],
                g: rgb[1],
                b: rgb[2],
            },
            1.0,
        ));
    }
    assert_eq!(colors.len(), 16);
    Palette {
        colors,
    }
}

#[derive(Copy, Clone, Debug, PartialEq)]
pub enum KeyPress {
    Up,
    Down,
    Left,
    Right,
    Space,
    Delete,
    Other(u8, Option<u8>),
}

pub struct Robot {
    pub name: ByteString,
    pub ch: u8,
    pub current_line: u16,
    pub current_loc: u8,
    pub cycle: u8,
    pub cycle_count: u8,
    pub bullet_type: i32,
    pub locked: bool,
    pub lavawalking: i32,
    pub walk: Option<CardinalDirection>,
    pub last_touched: Option<CardinalDirection>,
    pub last_shot: Option<CardinalDirection>,
    pub position: Coordinate<u16>,
    pub reserved: [u8; 3],
    pub onscreen: bool,
    pub loop_count: i32,
    pub program: Vec<Command>,
    pub alive: bool,
    pub status: RunStatus,
    pub local: [i32; 32],
    stack: Vec<u16>,
}

impl Default for Robot {
    fn default() -> Robot {
        Robot {
            name: ByteString::from(""),
            ch: 0,
            current_line: 0,
            current_loc: 0,
            cycle: 0,
            cycle_count: 0,
            bullet_type: 0,
            locked: false,
            lavawalking: 0,
            walk: None,
            last_touched: None,
            last_shot: None,
            position: Coordinate(0, 0),
            reserved: [0; 3],
            onscreen: true,
            loop_count: 0,
            program: vec![],
            alive: true,
            status: RunStatus::NotRun,
            local: [0; 32],
            stack: vec![],
        }
    }
}

impl Robot {
    pub fn copy_from(source: &Robot, pos: Coordinate<u16>) -> Robot {
        // FIXME: if source robot is in end state, new robot should
        //        also not run, even if initial program command isn't end.
        Robot {
            name: source.name.clone(),
            ch: source.ch,
            current_line: 0,
            current_loc: 0,
            cycle: 0,
            cycle_count: 0,
            bullet_type: source.bullet_type,
            locked: source.locked,
            lavawalking: source.lavawalking,
            walk: None,
            last_touched: None,
            last_shot: None,
            position: pos,
            reserved: [0; 3],
            onscreen: true,
            loop_count: source.loop_count,
            program: source.program.clone(),
            alive: true,
            status: RunStatus::FinishedRunning,
            local: source.local.clone(),
            stack: vec![], // FIXME: check if copying copies stack
        }
    }

    pub fn is(&self, condition: &Condition, board: &Board, key: Option<KeyPress>) -> bool {
        match condition {
            Condition::Walking => self.walk.is_some(),
            Condition::Swimming => match board.under_thing_at(&self.position) {
                Thing::StillWater
                | Thing::NWater
                | Thing::SWater
                | Thing::EWater
                | Thing::WWater => true,
                _ => false,
            },
            Condition::Firewalking => board.under_thing_at(&self.position) == Thing::Fire,
            Condition::Touching(ref dir) => {
                let is_touching_dir = |d: &CardinalDirection| {
                    let adjusted = adjust_coordinate(self.position, board, *d);
                    adjusted.map_or(false, |pos| board.thing_at(&pos) == Thing::Player)
                };
                match dir_to_cardinal_dir(self, dir) {
                    Some(dir) => is_touching_dir(&dir),
                    None => {
                        if dir.dir == Direction::Anydir {
                            [
                                CardinalDirection::North,
                                CardinalDirection::South,
                                CardinalDirection::East,
                                CardinalDirection::West,
                            ]
                            .iter()
                            .any(is_touching_dir)
                        } else {
                            false
                        }
                    }
                }
            }
            Condition::Blocked(ref dir) => {
                let is_blocked_dir = |d: &CardinalDirection| {
                    let adjusted = adjust_coordinate(self.position, board, *d);
                    adjusted.map_or(false, |pos| board.thing_at(&pos).is_solid())
                };
                match dir_to_cardinal_dir(self, dir) {
                    Some(dir) => is_blocked_dir(&dir),
                    None => {
                        if dir.dir == Direction::Anydir {
                            [
                                CardinalDirection::North,
                                CardinalDirection::South,
                                CardinalDirection::East,
                                CardinalDirection::West,
                            ]
                            .iter()
                            .any(is_blocked_dir)
                        } else {
                            false
                        }
                    }
                }
            }
            Condition::Aligned => {
                board.player_pos.0 == self.position.0 || board.player_pos.1 == self.position.1
            }
            Condition::AlignedNS => board.player_pos.0 == self.position.0,
            Condition::AlignedEW => board.player_pos.1 == self.position.1,
            Condition::LastShot(ref d) => {
                let dir = dir_to_cardinal_dir(self, d);
                match (dir, self.last_shot.as_ref()) {
                    (Some(ref d1), Some(d2)) => d1 == d2,
                    _ => false,
                }
            }
            Condition::LastTouched(ref d) => {
                let dir = dir_to_cardinal_dir(self, d);
                match (dir, self.last_touched.as_ref()) {
                    (Some(ref d1), Some(d2)) => d1 == d2,
                    _ => false,
                }
            }
            Condition::UpPressed => key == Some(KeyPress::Up),
            Condition::DownPressed => key == Some(KeyPress::Down),
            Condition::LeftPressed => key == Some(KeyPress::Left),
            Condition::RightPressed => key == Some(KeyPress::Right),
            Condition::SpacePressed => key == Some(KeyPress::Space),
            Condition::DelPressed => key == Some(KeyPress::Delete),
            Condition::MusicOn | Condition::PcSfxOn => {
                warn!("Unimplemented condition ({:?})", condition);
                false
            }
        }
    }
}

pub enum StringCounter {
    BoardName,
    //BoardScan,
    //Input,
    ModName,
    RobotName,
    //Fread(Option<u32>),
    //Fwrite(Option<u32>),
}

impl StringCounter {
    fn from(name: &ByteString) -> Option<StringCounter> {
        Some(match &**name {
            b"board_name" => StringCounter::BoardName,
            b"robot_name" => StringCounter::RobotName,
            b"mod_name" => StringCounter::ModName,
            _ => return None,
        })
    }
}

#[derive(Debug)]
pub enum NumericCounter {
    Math(MathCounter),
    Player(PlayerCounter),
    Robot(RobotCounter),
    Board(BoardCounter),
    File(FileCounter),
    Sprite(SpriteCounter),
    CharEdit(CharEditCounter),
    Mouse(MouseCounter),
    Defaults(DefaultsCounter),
    VLayer(VLayerCounter),
    Misc(MiscCounter),
}

impl NumericCounter {
    fn from(name: &ByteString, counters: &Counters, context: &dyn CounterContextExt) -> Option<NumericCounter> {
        let name = ByteString(
            name.as_bytes()
                .iter()
                .map(|c| c.to_ascii_lowercase())
                .collect(),
        );

        MathCounter::from(&name, counters, context)
            .map(NumericCounter::Math)
            .or_else(|| {
                PlayerCounter::from(&name)
                    .map(NumericCounter::Player)
            }).or_else(|| {
                RobotCounter::from(&name)
                    .map(NumericCounter::Robot)
            }).or_else(|| {
                BoardCounter::from(&name)
                    .map(NumericCounter::Board)
            }).or_else(|| {
                FileCounter::from(&name)
                    .map(NumericCounter::File)
            }).or_else(|| {
                SpriteCounter::from(&name)
                    .map(NumericCounter::Sprite)
            }).or_else(|| {
                CharEditCounter::from(&name)
                    .map(NumericCounter::CharEdit)
            }).or_else(|| {
                MouseCounter::from(&name)
                    .map(NumericCounter::Mouse)
            }).or_else(|| {
                DefaultsCounter::from(&name)
                    .map(NumericCounter::Defaults)
            }).or_else(|| {
                VLayerCounter::from(&name)
                    .map(NumericCounter::VLayer)
            }).or_else(|| {
                MiscCounter::from(&name)
                    .map(NumericCounter::Misc)
            })
    }

    fn eval(&self, context: &CounterContext) -> i32 {
        match self {
            NumericCounter::Math(c) => c.eval(context),
            NumericCounter::Player(c) => c.eval(context),
            NumericCounter::Robot(c) => c.eval(context),
            NumericCounter::Board(c) => c.eval(context),
            NumericCounter::File(c) => c.eval(context),
            NumericCounter::Sprite(c) => c.eval(context),
            NumericCounter::CharEdit(c) => c.eval(context),
            NumericCounter::Mouse(c) => c.eval(context),
            NumericCounter::Defaults(c) => c.eval(context),
            NumericCounter::VLayer(c) => c.eval(context),
            NumericCounter::Misc(c) => c.eval(context),
        }
    }
}

#[derive(Debug)]
pub enum PlayerCounter {
    Gems,
    Ammo,
    LoBombs,
    HighBombs,
    Coins,
    Lives,
    Health,
    //Invinco,
    Score,
    //PlayerLastDir,
    PlayerFaceDir,
    PlayerX,
    PlayerY,
}

impl PlayerCounter {
    fn from(name: &ByteString) -> Option<PlayerCounter> {
        Some(match &**name {
            b"gems" => PlayerCounter::Gems,
            b"ammo" => PlayerCounter::Ammo,
            b"lobombs" => PlayerCounter::LoBombs,
            b"hibombs" => PlayerCounter::HighBombs,
            b"coins" => PlayerCounter::Coins,
            b"lives" => PlayerCounter::Lives,
            b"health" => PlayerCounter::Health,
            b"score" => PlayerCounter::Score,
            b"playerfacedir" => PlayerCounter::PlayerFaceDir,
            b"playerx" => PlayerCounter::PlayerX,
            b"playery" => PlayerCounter::PlayerY,
            _ => return None,
        })
    }

    fn eval(&self, context: &CounterContext) -> i32 {
        match self {
            PlayerCounter::Gems => context.state.gems as i32,
            PlayerCounter::Ammo => context.state.ammo as i32,
            PlayerCounter::LoBombs => context.state.lobombs as i32,
            PlayerCounter::HighBombs => context.state.hibombs as i32,
            PlayerCounter::Coins => context.state.coins as i32,
            PlayerCounter::Lives => context.state.lives as i32,
            PlayerCounter::Health => context.state.health as i32,
            PlayerCounter::Score => context.state.score as i32,
            PlayerCounter::PlayerFaceDir => context.state.player_face_dir,
            PlayerCounter::PlayerX => context.board.player_pos.0 as i32,
            PlayerCounter::PlayerY => context.board.player_pos.1 as i32,
        }
    }
}

#[derive(Debug)]
pub enum MathCounter {
    Abs(i32),
    Sqrt(i32),
    Min(i32, i32),
    Max(i32, i32),
    //Multiplier,
    //Divider,
    //CDivisions,
    //Sin(i32),
    //Cos(i32),
    //Tan(i32),
    //ASin(i32),
    //ACos(i32),
    //ATan(i32),
    //ArcTan(i32, i32),
}

impl MathCounter {
    fn eval(&self, _context: &CounterContext) -> i32 {
        match self {
            MathCounter::Abs(v) => if *v < 0 { -v } else { *v },
            MathCounter::Sqrt(v) => (*v as f32).sqrt() as i32,
            MathCounter::Min(a, b) => *a.min(&b),
            MathCounter::Max(a, b) => *a.max(&b),
        }
    }

    fn from(name: &ByteString, counters: &Counters, context: &dyn CounterContextExt) -> Option<MathCounter> {
        Some(match name {
            _ if name.starts_with(b"abs") => {
                let name: ByteString = name.as_bytes()[3..].into();
                let result = name.evaluate_for_name(counters, context);
                let val = result.to_string().parse::<i32>().ok()?;
                MathCounter::Abs(val)
            },
            _ if name.starts_with(b"sqrt") => {
                let name: ByteString = name.as_bytes()[4..].into();
                let result = name.evaluate_for_name(counters, context);
                let val = result.to_string().parse::<i32>().ok()?;
                MathCounter::Sqrt(val)
            }
            _ if name.starts_with(b"min") => {
                let parts = &name.as_bytes()[3..];
                if let Some(idx) = parts.iter().position(|b| *b == b',') {
                    let (left, right) = parts.split_at(idx);
                    let left = ByteString::from(left).evaluate_for_name(counters, context);
                    let left = left.to_string().parse::<i32>().ok()?;
                    let right = ByteString::from(&right[1..]).evaluate_for_name(counters, context);
                    let right = right.to_string().parse::<i32>().ok()?;
                    MathCounter::Min(left, right)
                } else {
                    return None;
                }
            }
            _ if name.starts_with(b"max") => {
                let parts = &name.as_bytes()[3..];
                if let Some(idx) = parts.iter().position(|b| *b == b',') {
                    let (left, right) = parts.split_at(idx);
                    let left = ByteString::from(left).evaluate_for_name(counters, context);
                    let left = left.to_string().parse::<i32>().ok()?;
                    let right = ByteString::from(&right[1..]).evaluate_for_name(counters, context);
                    let right = right.to_string().parse::<i32>().ok()?;
                    MathCounter::Max(left, right)
                } else {
                    return None;
                }
            }
            _ => return None,
        })
    }
}

#[derive(Debug)]
pub enum RobotCounter {
    //Commands,
    //OtherLocal(i32, ByteString),
}

impl RobotCounter {
    #[allow(unreachable_code)]
    fn from(name: &ByteString) -> Option<Self> {
        Some(match &**name {
            _ => return None,
        })
    }

    fn eval(&self, _context: &CounterContext) -> i32 {
        unimplemented!()
    }
}

#[derive(Debug)]
pub enum BoardCounter {
    //BoardId,
    //BoardParam,
    BoardW,
    BoardH,
    //Time,
    //TimeReset,
    ScrolledX,
    ScrolledY,
    //BoardX,
    //BoardY,
    //BoardChar,
    //BoardColor,
    //Bch(i32, i32),
    //Bco(i32, i32),
    //Bid(i32, i32),
    //Bpr(i32, i32),
    //Uch(i32, i32),
    //Uco(i32, i32),
    //Uid(i32, i32),
    //Upr(i32, i32),
    //OverlayX,
    //OverlayY,
    //OverlayChar,
    //OverlayColor,
    //Och(i32, i32),
    //Oco(i32, i32),
    //OverlayMode,
    //ModFrequency,
    //ModOrder,
    //ModPosition,
    //ModLength,
    //ModLoopStart,
    //ModLoopEnd,
}

impl BoardCounter {
    fn from(name: &ByteString) -> Option<Self> {
        Some(match &**name {
            b"board_w" => BoardCounter::BoardW,
            b"board_h" => BoardCounter::BoardH,
            b"scrolledx" => BoardCounter::ScrolledX,
            b"scrolledy" => BoardCounter::ScrolledY,
            _ => return None,
        })
    }

    fn eval(&self, context: &CounterContext) -> i32 {
        match self {
            BoardCounter::BoardW => context.board.width as i32,
            BoardCounter::BoardH => context.board.height as i32,
            BoardCounter::ScrolledX => context.board.scroll_offset.0 as i32,
            BoardCounter::ScrolledY => context.board.scroll_offset.1 as i32,
        }
    }
}

#[derive(Debug)]
pub enum FileCounter {
    //FreadOpen,
    //FwriteOpen,
    //FwriteModify,
    //FwriteAppend,
    //FreadCounter,
    //FwriteCounter,
    //FreadPos,
    //FwritePos,
    //FreadDelimiter,
    //FwriteDelimiter,
    //Fread(Option<i32>),
    //Fwrite(Option<i32>),
    //FreadLength,
    //FwriteLength,
    //SaveRobot(Option<i32>),
    //LoadRobot(Option<i32>),
    //SaveBc(Option<i32>),
    //LoadBc(Option<i32>),
    //SaveGame,
    //LoadGame,
    //SaveCounters,
    //LoadCounters,
}

impl FileCounter {
    #[allow(unreachable_code)]
    fn from(name: &ByteString) -> Option<Self> {
        Some(match &**name {
            _ => return None,
        })
    }

    fn eval(&self, _context: &CounterContext) -> i32 {
        unimplemented!()
    }
}

#[derive(Debug)]
pub enum SpriteCounter {
    //SprClist(i32),
    //SprCollisions,
    //SprNum,
    //SprYOrder,
    //SprCCheck(i32),
    //SprCWidth(i32),
    //SprCHeight(i32),
    //SprCList(i32),
    //SprCx(i32),
    //SprCy(i32),
    //SprOff(i32),
    //SprOverlaid(i32),
    //SprOverlay(i32),
    //SprRefX(i32),
    //SprRefY(i32),
    //SprSetView(i32),
    //SprStatic(i32),
    //SprSwap(i32),
    //SprVlayer(i32),
    //SprWidth(i32),
    //SprHeight(i32),
    //SprX(i32),
    //SprY(i32),
    //SprZ(i32),
    //SprUnbound(i32),
    //SprTCol(i32),
    //SprOffset(i32),
}

impl SpriteCounter {
    #[allow(unreachable_code)]
    fn from(name: &ByteString) -> Option<Self> {
        Some(match &**name {
            _ => return None,
        })
    }

    fn eval(&self, _context: &CounterContext) -> i32 {
        unimplemented!()
    }
}

#[derive(Debug)]
pub enum CharEditCounter {
    //CharX,
    //CharY,
    //Pixel,
    //CharByte,
    //Char,
    //Byte,
}

impl CharEditCounter {
    #[allow(unreachable_code)]
    fn from(name: &ByteString) -> Option<Self> {
        Some(match &**name {
            _ => return None,
        })
    }

    fn eval(&self, _context: &CounterContext) -> i32 {
        unimplemented!()
    }
}

#[derive(Debug)]
pub enum MouseCounter {
    //MouseX,
    //MouseY,
    //MousePx,
    //MousePy,
    //MBoardX,
    //MBoardY,
    //Buttons,
    //CursorState,
    //JoyActive(i32),
    //JoyActions(i32, i32),
    //JoySimulateKeys,
}

impl MouseCounter {
    #[allow(unreachable_code)]
    fn from(name: &ByteString) -> Option<Self> {
        Some(match &**name {
            _ => return None,
        })
    }

    fn eval(&self, _context: &CounterContext) -> i32 {
        unimplemented!()
    }
}

#[derive(Debug)]
pub enum DefaultsCounter {
    //EnterMenu,
    //EscapeMenu,
    //HelpMenu,
    //F2Menu,
    //LoadMenu,
    //BiMesg,
    //SpaceLock,
}

impl DefaultsCounter {
    #[allow(unreachable_code)]
    fn from(name: &ByteString) -> Option<Self> {
        Some(match &**name {
            _ => return None,
        })
    }

    fn eval(&self, _context: &CounterContext) -> i32 {
        unimplemented!()
    }
}

#[derive(Debug)]
pub enum VLayerCounter {
    //VLayerWidth,
    //VLayerHeight,
    //VLayerSize,
    //Vch(i32, i32),
    //Vco(i32, i32),
}

impl VLayerCounter {
    #[allow(unreachable_code)]
    fn from(name: &ByteString) -> Option<Self> {
        Some(match &**name {
            _ => return None,
        })
    }

    fn eval(&self, _context: &CounterContext) -> i32 {
        unimplemented!()
    }
}

#[derive(Debug)]
pub enum MiscCounter {
    //MzxSpeed,
    //CommandsStop,
    //ExitGame,
    //PlayGame,
    //CurrentColor,
    //RedValue,
    //GreenValue,
    //BlueValue,
    //Input,
    //InputSize,
    //DateDay,
    //DateMonth,
    //DateYear,
    //TimeHours,
    //TimeMinutes,
    //TimeSeconds,
    //TimeMillis,
    //Int,
    //BitPlace,
    //Int2Bin,
    KeyPressed,
    //Key(Option<i32>),
    //KeyCode,
    //KeyRelease,
    //RandomSeed0,
    //RandomSeed1,
    //MaxSamples,
    //Xpos,
    //Ypos,
    //FirstXPos,
    //FirstYPos,
    //LastXPos,
    //LastYPos,
}

impl MiscCounter {
    fn from(name: &ByteString) -> Option<Self> {
        Some(match &**name {
            b"keypressed" => Self::KeyPressed,
            _ => return None,
        })
    }

    fn eval(&self, context: &CounterContext) -> i32 {
        match self {
            MiscCounter::KeyPressed => context.state.key_pressed,
        }
    }
}

#[derive(Debug)]
pub enum LocalCounter {
    Loopcount,
    Local(u8),
    Lavawalk,
    HorizPld,
    VertPld,
    PlayerDist,
    ThisX,
    ThisY,
    BulletType,
    ThisColor,
    ThisChar,
    //RobotId,
    //GoopWalk,
}

impl LocalCounter {
    fn from(name: &ByteString) -> Option<LocalCounter> {
        let name = ByteString(
            name.as_bytes()
                .iter()
                .map(|c| c.to_ascii_lowercase())
                .collect(),
        );
        Some(match &*name {
            b"loopcount" => LocalCounter::Loopcount,
            b"local" => LocalCounter::Local(0),
            b"lava_walk" => LocalCounter::Lavawalk,
            b"horizpld" => LocalCounter::HorizPld,
            b"vertpld" => LocalCounter::VertPld,
            b"playerdist" => LocalCounter::PlayerDist,
            b"thisx" => LocalCounter::ThisX,
            b"thisy" => LocalCounter::ThisY,
            b"bullettype" => LocalCounter::BulletType,
            b"thiscolor" => LocalCounter::ThisColor,
            b"thischar" => LocalCounter::ThisChar,
            _ if name.len() > 5 && name[0..5] == b"local"[..] => {
                let suffix = str::from_utf8(&name[5..])
                    .ok()
                    .and_then(|s| s.parse::<u16>().ok());
                match suffix {
                    Some(n) => LocalCounter::Local((n % 32) as u8),
                    _ => return None,
                }
            }
            _ => return None,
        })
    }
}

impl Robot {}

#[derive(Debug, PartialEq)]
pub enum RunStatus {
    NotRun,
    FinishedRunning,
    FinishedWithoutRunning,
    MustRunOnReverse,
}

impl Default for RunStatus {
    fn default() -> RunStatus {
        RunStatus::NotRun
    }
}

#[derive(Debug)]
pub enum WorldError {
    Protected,
    TooNewVersion,
    UnrecognizedVersion([u8; 3]),
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

impl From<BoardError> for WorldError {
    fn from(err: BoardError) -> WorldError {
        WorldError::Board(err)
    }
}

fn get_objects<T, F>(buffer: &[u8], loader: F) -> (Vec<T>, &[u8])
where
    F: Fn(&[u8]) -> (T, &[u8]),
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
    /*if byte != 0 && byte != 1 {
        assert_eq!(byte, 0);
    }*/
    (byte != 0, buffer)
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
    (
        if board == 255 {
            None
        } else {
            Some(BoardId(board))
        },
        buffer,
    )
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
    let (name, buffer) = get_null_terminated_string(buffer, LEGACY_ROBOT_NAME_SIZE);
    debug!("loading robot {:?}", name);
    let (ch, buffer) = get_byte(buffer);
    let (current_line, buffer) = get_word(buffer);
    let (current_loc, buffer) = get_byte(buffer);
    let (cycle, buffer) = get_byte(buffer);
    let (cycle_count, buffer) = get_byte(buffer);
    let (bullet_type, buffer) = get_byte(buffer);
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
        bullet_type: bullet_type as i32,
        locked: locked,
        lavawalking: if lavawalking { 1 } else { 0 },
        walk: walk,
        last_touched: last_touched,
        last_shot: last_shot,
        position: Coordinate(x_pos, y_pos),
        reserved: [0, 0, 0],
        onscreen: onscreen,
        loop_count: loop_count as i32,
        program: program.into(),
        alive: true,
        status: RunStatus::NotRun,
        local: [0; 32],
        stack: vec![],
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
        Scroll {
            num_lines,
            text,
            used,
        },
        buffer,
    )
}

fn load_sensor(buffer: &[u8]) -> (Sensor, &[u8]) {
    let (name, buffer) = get_null_terminated_string(buffer, 15);
    let (ch, buffer) = get_byte(buffer);
    let (target, buffer) = get_null_terminated_string(buffer, 15);
    let (used, buffer) = get_bool(buffer);
    (
        Sensor {
            name,
            ch,
            target,
            used,
        },
        buffer,
    )
}

fn load_board(
    title: ByteString,
    version: u32,
    buffer: &[u8],
) -> Result<(Board, Vec<Robot>), BoardError> {
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
        let overlay_mode = OverlayMode::from_byte(overlay_mode)
            .map_err(|_| BoardError::UnknownOverlayMode(overlay_mode))?;
        let (chars, new_buffer, w, h) = decode_runs(new_buffer);
        let (colors, new_buffer, w2, h2) = decode_runs(new_buffer);
        assert_eq!(w, w2);
        assert_eq!(h, h2);
        buffer = new_buffer;
        Some((
            overlay_mode,
            Zip::new((chars.into_iter(), colors.into_iter())).collect(),
        ))
    } else {
        None
    };

    let (ids, buffer, width, height) = decode_runs(buffer);
    debug!(
        "board {:?} has dimensions {}x{}",
        title.to_string(),
        width,
        height
    );
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

    let (scroll_x, scroll_y, current_message, cycles_until_disappear, message_row, message_col) =
        if version < 0x253 {
            let (_last_key, new_buffer) = get_byte(buffer);
            let (_last_input, new_buffer) = get_word(new_buffer);
            let (_last_input_length, new_buffer) = get_byte(new_buffer);
            let (_last_input_string, new_buffer) = get_null_terminated_string(new_buffer, 81);
            let (_last_player_dir, new_buffer) = get_byte(new_buffer);
            let (current_message, new_buffer) = get_null_terminated_string(new_buffer, 81);
            let (cycles_until_disappear, new_buffer) = get_byte(new_buffer);
            let (_lazer_wall_timer, new_buffer) = get_byte(new_buffer);
            let (message_row, new_buffer) = get_byte(new_buffer);
            let (message_col, new_buffer) = get_byte(new_buffer);
            let (scroll_x, new_buffer) = get_word(new_buffer);
            let (scroll_y, new_buffer) = get_word(new_buffer);
            let (_x_screen_pos, new_buffer) = get_word(new_buffer);
            let (_y_screen_pos, new_buffer) = get_word(new_buffer);
            buffer = new_buffer;
            (
                scroll_x,
                scroll_y,
                current_message,
                cycles_until_disappear,
                message_row,
                message_col,
            )
        } else {
            (0, 0, ByteString(vec![]), 0, 24, 0)
        };

    let (player_locked_ns, buffer) = get_bool(buffer);
    let (player_locked_ew, buffer) = get_bool(buffer);
    let (player_locked_attack, mut buffer) = get_bool(buffer);
    if version < 0x253 {
        let (_mod_volume, new_buffer) = get_byte(buffer);
        let (_mod_volume_change, new_buffer) = get_byte(new_buffer);
        let (_mod_volume_target, new_buffer) = get_byte(new_buffer);
        buffer = new_buffer;
    }

    let (mut robots, buffer) = get_objects(buffer, load_robot);

    for (pos, (id, param)) in ids.iter().zip(params.iter()).enumerate() {
        if *id == Thing::Robot as u8 || *id == Thing::RobotPushable as u8 {
            let x = (pos % width) as u16;
            let y = (pos / width) as u16;
            let robot = &mut robots[*param as usize - 1];
            if robot.position != Coordinate(x, y) {
                warn!(
                    "resetting invalid robot position: {:?} to {},{}",
                    robot.position, x, y
                );
                robot.position = Coordinate(x, y);
            }
        }
    }

    debug!("loading scrolls");
    let (scrolls, buffer) = get_objects(buffer, load_scroll);
    debug!("loading sensors");
    let (sensors, _buffer) = get_objects(buffer, load_sensor);
    assert_eq!(_buffer.len(), 0);

    Ok((
        Board {
            title: title,
            width: width,
            height: height,
            overlay: overlay,
            level: Zip::new((ids.into_iter(), colors.into_iter(), params.into_iter())).collect(),
            under: Zip::new((
                under_ids.into_iter(),
                under_colors.into_iter(),
                under_params.into_iter(),
            ))
            .collect(),
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
            message_line: current_message,
            message_row,
            message_col: if message_col == 0xFF {
                None
            } else {
                Some(message_col)
            },
            remaining_message_cycles: cycles_until_disappear,
            player_locked_ns,
            player_locked_ew,
            player_locked_attack,
            num_robots: robots.len(),
        },
        robots,
    ))
}

const MAX_PASSWORD_LENGTH: usize = 15;
static MAGIC_CODE: &[u8; MAX_PASSWORD_LENGTH] =
    b"\xE6\x52\xEB\xF2\x6D\x4D\x4A\xB7\x87\xB2\x92\x88\xDE\x91\x24";
const WORLD_BLOCK_1_SIZE: usize = 4129;
const WORLD_BLOCK_2_SIZE: usize = 72;
const LEGACY_BOARD_NAME_SIZE: usize = 25;
const LEGACY_ROBOT_NAME_SIZE: usize = 15;

fn decrypt_block(buffer: &[u8], block_len: usize, xor_val: u8) -> (Vec<u8>, &[u8]) {
    let (buffer, rest) = buffer.split_at(block_len);
    assert_eq!(buffer.len(), block_len);
    let decrypted = buffer.iter().map(|b| b ^ xor_val).take(block_len).collect();
    (decrypted, rest)
}

fn decrypt_and_fix_offset(buffer: &[u8], xor_d: i32) -> (Vec<u8>, &[u8]) {
    let (offset, buffer) = get_dword(buffer);
    let mut offset = offset as i32;
    assert!(offset != -1);
    offset ^= xor_d;
    offset -= MAX_PASSWORD_LENGTH as i32;
    let mut bytes = vec![0; 4];
    LittleEndian::write_i32(&mut bytes[..], offset);
    (bytes, buffer)
}

fn get_pw_xor_code(password: &mut [u8], protection: u8) -> i32 {
    let mut work = 85;
    let start = password.iter().position(|&b| b == b'\0');
    if let Some(start) = start {
        for b in password.iter_mut().skip(start) {
            *b = 0;
        }
    }
    for (i, b) in password.iter().enumerate() {
        work <<= 1;
        if work > 255 {
            work ^= 257;
        }
        if i & 1 != 0 {
            work += *b as i8 as i32;
            if work > 255 {
                work ^= 257;
            }
        } else {
            work ^= *b as i8 as i32;
        }
    }
    work += protection as i8 as i32;
    if work > 255 {
        work ^= 257;
    }

    work <<= 1;
    if work > 255 {
        work ^= 257;
    }

    if work == 0 {
        work = 85;
    }
    work
}

pub fn load_world(buffer: &[u8]) -> Result<World, WorldError> {
    let mut original_buffer = buffer;

    let (title, buffer) = get_null_terminated_string(buffer, LEGACY_BOARD_NAME_SIZE);
    debug!("loading world {:?}", title);

    let mut decrypted_buffer;
    let (protection, mut buffer) = get_byte(buffer);
    if protection != 0 {
        let (password, tmp_buffer) = buffer.split_at(MAX_PASSWORD_LENGTH);
        let mut password: Vec<_> = password
            .iter()
            .zip(&MAGIC_CODE[..])
            .map(|(byte, magic)| {
                let a = byte ^ magic;
                let b = a as i8 - (0x12 + protection) as i8;
                b as u8 ^ 0x8D
            })
            .collect();
        let xor_val = get_pw_xor_code(&mut *password, protection) as u8;
        let xor_w: i16 = xor_val as i16 | (xor_val as i16) << 8;
        let xor_d: i32 = xor_val as i32
            | (xor_val as i32) << 8
            | (xor_val as i32) << 16
            | (xor_val as i32) << 24;
        decrypted_buffer = vec![];
        // null terminated title
        let mut extended_title = [0; LEGACY_BOARD_NAME_SIZE];
        extended_title[..title.len()].copy_from_slice(&*title);
        decrypted_buffer.extend(&extended_title[..]);

        // protection method
        decrypted_buffer.push(0);

        let skip_start = decrypted_buffer.len();

        // signature
        decrypted_buffer.extend(&[b'M', b'\x02', b'\x11']);
        let (_, tmp_buffer) = get_byte(tmp_buffer);
        let (_, tmp_buffer) = get_byte(tmp_buffer);
        let (_, tmp_buffer) = get_byte(tmp_buffer);
        let (decrypted, tmp_buffer) =
            decrypt_block(tmp_buffer, WORLD_BLOCK_1_SIZE + WORLD_BLOCK_2_SIZE, xor_val);
        decrypted_buffer.extend(&*decrypted);

        let (offset, tmp_buffer) = decrypt_and_fix_offset(tmp_buffer, xor_d);
        decrypted_buffer.extend(&*offset);

        let (mut num_boards, mut tmp_buffer) = get_byte(tmp_buffer);
        num_boards ^= xor_val;
        decrypted_buffer.push(num_boards);

        if num_boards == 0 {
            let (mut sfx_length, tmp_buffer2) = get_word(tmp_buffer);
            sfx_length ^= xor_w as u16;
            let mut bytes = [0; 2];
            LittleEndian::write_u16(&mut bytes[..], sfx_length);
            decrypted_buffer.extend(&bytes[..]);
            let (sfx_data, tmp_buffer2) = decrypt_block(tmp_buffer2, sfx_length as usize, xor_val);
            decrypted_buffer.extend(&*sfx_data);
            let (new_num_boards, tmp_buffer2) = get_byte(tmp_buffer2);
            num_boards = new_num_boards ^ xor_val;
            decrypted_buffer.push(num_boards);
            tmp_buffer = tmp_buffer2;
        }

        let (board_titles, mut tmp_buffer) = decrypt_block(
            tmp_buffer,
            LEGACY_BOARD_NAME_SIZE * num_boards as usize,
            xor_val,
        );
        decrypted_buffer.extend(&*board_titles);
        for _ in 0..num_boards {
            let (board_length, tmp_buffer2) = get_dword(tmp_buffer);
            let mut bytes = [0; 4];
            LittleEndian::write_u32(&mut bytes[..], board_length ^ xor_d as u32);
            decrypted_buffer.extend(&bytes[..]);
            let (offset, tmp_buffer2) = decrypt_and_fix_offset(tmp_buffer2, xor_d);
            decrypted_buffer.extend(&*offset);
            tmp_buffer = tmp_buffer2;
        }
        let (decrypted, tmp_buffer) = decrypt_block(tmp_buffer, tmp_buffer.len(), xor_val);
        assert_eq!(tmp_buffer.len(), 0);
        decrypted_buffer.extend(&*decrypted);
        buffer = &decrypted_buffer[skip_start..];
        original_buffer = &decrypted_buffer;
    }

    let (signature, buffer) = buffer.split_at(3);
    let version = match (signature[0], signature[1], signature[2]) {
        (b'M', b'Z', b'X') => 0x0100,
        (b'M', b'Z', b'2') => 0x0205,
        (b'M', b'Z', b'A') => 0x0208,
        (b'M', a, b) if a > 1 && a < 10 => ((a as u32) << 8) + (b as u32),
        (a, b, c) => return Err(WorldError::UnrecognizedVersion([a, b, c])),
    };

    if version > LEGACY_WORLD_VERSION {
        //return Err(WorldError::TooNewVersion);
        return world::load_zip_world(original_buffer);
    }

    let (charset_data, buffer) = buffer.split_at(14 * 256);
    if charset_data.len() < CHARSET_BUFFER_SIZE {
        return Err(WorldError::CharsetTooSmall);
    }
    let mut charset = Charset {
        data: [0; CHARSET_BUFFER_SIZE],
    };
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
    let palette = load_palette(palette_color_data);

    let (global_robot_pos, buffer) = get_dword(buffer);
    let (global_robot, _) = load_robot(&original_buffer[global_robot_pos as usize..]);
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
        let (title, new_buffer) = get_null_terminated_string(buffer, LEGACY_BOARD_NAME_SIZE);
        buffer = new_buffer;

        titles.push(title);
    }

    for title in titles {
        let (byte_length, new_buffer) = get_dword(buffer);
        let byte_length = byte_length as usize;

        let (board_pos, new_buffer) = get_dword(new_buffer);
        let board_pos = board_pos as usize;

        if byte_length == 0 {
            buffer = new_buffer;
            boards.push((Board::default(), vec![]));
            continue;
        }

        let end_board_pos = board_pos + byte_length;
        let (mut board, mut robots) = load_board(
            title,
            version,
            &original_buffer[board_pos..end_board_pos],
        )?;
        board.init(&mut robots);
        boards.push((board, robots));
        buffer = new_buffer;
    }

    Ok(World {
        version: version,
        title: title,
        state: WorldState {
            charset: charset.clone(),
            initial_charset: charset,
            palette: palette.clone(),
            initial_palette: palette,
            idchars: idchars.to_vec().into_boxed_slice(),
            saved_positions: [(0, Coordinate(0, 0)); 10],
            scroll_locked: false,
            message_edge: true,
            message_color: 0x01,
            player_face_dir: 1,
            key_pressed: 0,
            lives: starting_lives as i32,
            health: starting_health as i32,
            ammo: 0,
            keys: 0,
            score: 0,
            coins: 0,
            gems: 0,
            hibombs: 0,
            lobombs: 0,
            time: 0,
            limit_lives: limit_lives as i32,
            limit_health: limit_health as i32,
            update_done: vec![],
        },
        boards: boards,
        edge_border: ColorValue(edge_border),
        starting_board_number: BoardId(starting_board_number),
        end_game_board: BoardId(end_game_board),
        death_board: BoardId(death_board),
        end_game_pos: Coordinate(end_game_x, end_game_y),
        game_over_sfx: game_over_sfx,
        death_pos: Coordinate(death_x, death_y),
        starting_lives: starting_lives,
        starting_health: starting_health,
        enemies_hurt_enemies: enemies_hurt_enemies,
        clear_messages_and_projectiles: clear_messages_and_projectiles,
        only_play_via_swap_world: only_play_via_swap_world,
        global_robot,
    })
}

#[cfg(test)]
mod tests {
    use crate::expression::test::TestLocalCounters;
    use crate::{ByteString, Counters};

    #[test]
    fn it_works() {
        use super::load_world;
        use std::fs::File;
        use std::io::Read;

        let mut f = File::open("BERNARD.MZX").unwrap();
        let mut v = vec![];
        f.read_to_end(&mut v).unwrap();
        let world = load_world(&v).unwrap();
        assert_eq!(world.title, "~d@8Bernard the Bard");
    }

    #[test]
    fn string_counter() {
        let mut c = Counters::new();
        let name: ByteString = "$a".into();
        let bytes: ByteString = "test string".into();
        c.set_string(name.clone(), &mut TestLocalCounters, bytes.clone());
        assert_eq!(c.get_string(&name, &TestLocalCounters), bytes);
    }

    #[test]
    fn string_counter_interpolate() {
        let mut c = Counters::new();
        c.set("bar".into(), &mut TestLocalCounters, 5);
        let name: ByteString = "$a&bar&".into();
        let bytes: ByteString = "test string".into();
        c.set_string(name.clone(), &mut TestLocalCounters, bytes.clone());
        assert_eq!(c.get_string(&name, &TestLocalCounters), bytes);
    }

    #[test]
    fn eval_with_string() {
        let mut c = Counters::new();
        let bytes: ByteString = "test string".into();
        c.set_string("$a".into(), &mut TestLocalCounters, bytes.clone());
        let s: ByteString = "&$a&".into();
        assert_eq!(s.evaluate(&c, &TestLocalCounters), bytes);
    }

    #[test]
    fn bytestring_cmp() {
        let s: ByteString = "a".into();
        let s2: ByteString = "a".into();
        assert!(s <= s2 && s >= s2);

        let s: ByteString = "a".into();
        let s2: ByteString = "b".into();
        assert!(s < s2 && s2 > s);

        let s: ByteString = "aa".into();
        let s2: ByteString = "a".into();
        assert!(s > s2 && s2 < s);

        let s: ByteString = "A".into();
        let s2: ByteString = "a".into();
        assert!(!s.case_sensitive_eq(&s2));

        let s: ByteString = "A".into();
        let s2: ByteString = "a".into();
        assert_eq!(s, s2);
    }
}
