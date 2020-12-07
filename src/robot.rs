use crate::audio::AudioEngine;
use crate::board::{move_level_to, put_at, put_thing, reset_view, GameStateChange};
use crate::robotic::Resolve;
use crate::{
    adjust_coordinate, bullet_param, dir_to_cardinal_dir, dir_to_cardinal_dir_rel, Board, BoardId,
    BulletType, ByteString, CardinalDirection, CharId, Color as MzxColor, ColorValue, Command,
    Coordinate, CounterContext, CounterContextMut, Counters, Direction, Explosion,
    ExtendedColorValue, ExtendedParam, KeyPress, MessageBoxLine, MessageBoxLineType, Operator,
    ParamValue, RelativeDirBasis, RelativePart, Robot, RunStatus, SignedNumeric, Size, Thing,
    WorldState,
};
use num_traits::{FromPrimitive, ToPrimitive};
use std::fs::File;
use std::io::Read;
use std::mem;
use std::ops::Deref;
use std::path::Path;

enum MoveResult {
    Move(Coordinate<u16>),
    Edge,
}

#[derive(PartialEq)]
enum Move {
    Moved,
    Blocked,
}

fn move_robot(
    robots: &mut Robots,
    robot_id: RobotId,
    board: &mut Board,
    dir: CardinalDirection,
    update_done: &mut [bool],
    ignore_thud_and_edge: bool,
) -> Move {
    let robot = robots.get_mut(robot_id);
    let result = match dir {
        CardinalDirection::North => {
            if robot.position.1 == 0 {
                MoveResult::Edge
            } else {
                MoveResult::Move(Coordinate(robot.position.0, robot.position.1 - 1))
            }
        }
        CardinalDirection::South => {
            if robot.position.1 as usize == board.height - 1 {
                MoveResult::Edge
            } else {
                MoveResult::Move(Coordinate(robot.position.0, robot.position.1 + 1))
            }
        }
        CardinalDirection::East => {
            if robot.position.0 as usize == board.width - 1 {
                MoveResult::Edge
            } else {
                MoveResult::Move(Coordinate(robot.position.0 + 1, robot.position.1))
            }
        }
        CardinalDirection::West => {
            if robot.position.0 == 0 {
                MoveResult::Edge
            } else {
                MoveResult::Move(Coordinate(robot.position.0 - 1, robot.position.1))
            }
        }
    };
    match result {
        MoveResult::Edge => {
            if !ignore_thud_and_edge {
                if !jump_robot_to_label(robot, BuiltInLabel::Edge) {
                    jump_robot_to_label(robot, BuiltInLabel::Thud);
                }
            }
            Move::Blocked
        }
        MoveResult::Move(new_pos) => {
            let thing = board.thing_at(&new_pos);
            let robot_pos = robot.position;
            if thing.is_pushable() {
                let adjusted = adjust_coordinate(new_pos, board, dir);
                if let Some(pushed_pos) = adjusted {
                    move_level_to(board, robots.robots, &new_pos, &pushed_pos, update_done);
                } else {
                    return Move::Blocked;
                }
            } else if thing.is_solid() {
                if !ignore_thud_and_edge {
                    jump_robot_to_label(robot, BuiltInLabel::Thud);
                }
                return Move::Blocked;
            }

            move_level_to(board, robots.robots, &robot_pos, &new_pos, update_done);
            let robot = robots.get_mut(robot_id);
            robot.position = new_pos;
            Move::Moved
        }
    }
}

fn move_robot_to(
    robots: &mut Robots,
    robot_id: RobotId,
    board: &mut Board,
    pos: Coordinate<u16>,
    update_done: &mut [bool],
) {
    let thing = board.thing_at(&pos);
    let robot_pos = robots.get(robot_id).position;
    if thing == Thing::Player || robot_pos == pos {
        return;
    }
    move_level_to(board, robots.robots, &robot_pos, &pos, update_done);
    assert_eq!(robots.get(robot_id).position, pos);
}

enum CoordinatePart {
    X,
    Y,
}

trait CoordinateExtractor {
    type CoordinateType;
    fn extract(&self, part: CoordinatePart) -> Self::CoordinateType;
}

impl<T: Copy> CoordinateExtractor for Coordinate<T> {
    type CoordinateType = T;
    fn extract(&self, part: CoordinatePart) -> T {
        match part {
            CoordinatePart::X => self.0,
            CoordinatePart::Y => self.1,
        }
    }
}

enum Relative {
    None,
    Coordinate(Option<RelativePart>, Coordinate<u16>),
}

impl Relative {
    fn resolve_xy<'a>(
        &self,
        x_value: &SignedNumeric,
        y_value: &SignedNumeric,
        counters: &Counters,
        context: CounterContext<'a>,
        part: RelativePart,
    ) -> Coordinate<u16> {
        let x = self.resolve(x_value, counters, context, part, CoordinatePart::X);
        let y = self.resolve(y_value, counters, context, part, CoordinatePart::Y);
        Coordinate(x.max(0) as u16, y.max(0) as u16)
    }

    fn resolve<'a>(
        &self,
        value: &SignedNumeric,
        counters: &Counters,
        context: CounterContext<'a>,
        part: RelativePart,
        coord_part: CoordinatePart,
    ) -> i16 {
        let v = value.resolve(counters, context) as i16;
        match *self {
            Relative::None => v,
            Relative::Coordinate(ref value_part, ref coord) => {
                if value_part.map_or(true, |p| p == part) {
                    v + coord.extract(coord_part) as i16
                } else {
                    v
                }
            }
        }
    }
}

pub enum BuiltInLabel {
    Thud,
    Edge,
    Bombed,
    JustEntered,
    Touch,
    Shot,
}

impl Into<EvaluatedByteString> for BuiltInLabel {
    fn into(self) -> EvaluatedByteString {
        EvaluatedByteString(ByteString::from(match self {
            BuiltInLabel::Thud => "thud",
            BuiltInLabel::Edge => "edge",
            BuiltInLabel::Bombed => "bombed",
            BuiltInLabel::JustEntered => "justentered",
            BuiltInLabel::Touch => "touch",
            BuiltInLabel::Shot => "shot",
        }))
    }
}

#[derive(Clone, Debug)]
pub struct EvaluatedByteString(ByteString);

impl EvaluatedByteString {
    pub fn no_eval_needed(s: ByteString) -> EvaluatedByteString {
        assert!(!s.contains(&b'&'));
        EvaluatedByteString(s)
    }
}

impl Deref for EvaluatedByteString {
    type Target = ByteString;
    fn deref(&self) -> &ByteString {
        &self.0
    }
}

pub(crate) trait Evaluator {
    fn eval<'a>(&self, counters: &Counters, context: CounterContext<'a>) -> EvaluatedByteString;
}

impl Evaluator for ByteString {
    fn eval<'a>(&self, counters: &Counters, context: CounterContext<'a>) -> EvaluatedByteString {
        EvaluatedByteString(self.evaluate_for_name(counters, &context))
    }
}

enum BuiltInCounter {
    Xpos,
    Ypos,
}

impl Into<ByteString> for BuiltInCounter {
    fn into(self) -> ByteString {
        ByteString::from(match self {
            BuiltInCounter::Xpos => "xpos",
            BuiltInCounter::Ypos => "ypos",
        })
    }
}

impl Into<EvaluatedByteString> for BuiltInCounter {
    fn into(self) -> EvaluatedByteString {
        EvaluatedByteString(self.into())
    }
}

fn copy_robot(source_id: RobotId, robot_id: RobotId, robots: &mut Robots, board: &mut Board) {
    let dest_position = robots.get(robot_id).position;
    let source_robot = robots.get_mut(source_id);
    let color = board.level_at(&source_robot.position).1;
    board.level_at_mut(&dest_position).1 = color;
    *robots.get_mut(robot_id) = Robot::copy_from(source_robot, dest_position);
}

#[derive(Copy, Clone, Debug, PartialEq)]
pub enum RobotId {
    Board(usize),
    Global,
}

impl RobotId {
    pub fn from(param: u8) -> RobotId {
        if param == 0 {
            RobotId::Global
        } else {
            RobotId::Board(param as usize - 1)
        }
    }

    fn is_global(&self) -> bool {
        *self == RobotId::Global
    }

    fn to_param(&self) -> Result<u8, ()> {
        match self {
            RobotId::Board(p) => Ok((p + 1) as u8),
            RobotId::Global => Err(())
        }
    }
}

pub struct Robots<'a> {
    global_robot: &'a mut Robot,
    robots: &'a mut Vec<Robot>,
}

impl<'a> Robots<'a> {
    pub fn new(robots: &'a mut Vec<Robot>, global: &'a mut Robot) -> Robots<'a> {
        Robots {
            robots: robots,
            global_robot: global,
        }
    }

    fn duplicate_self(&mut self, source: RobotId, pos: Coordinate<u16>) -> RobotId {
        let robot = Robot::copy_from(self.get(source), pos);
        let id = if let Some(index) = self.robots.iter_mut().position(|r| !r.alive) {
            self.robots[index] = robot;
            index
        } else {
            self.robots.push(robot);
            self.robots.len() - 1
        };
        RobotId::Board(id)
    }

    pub fn foreach<F: FnMut(&mut Robot, RobotId)>(&mut self, mut f: F) {
        f(&mut self.global_robot, RobotId::Global);
        for (idx, robot) in self.robots.iter_mut().enumerate() {
            f(robot, RobotId::Board(idx));
        }
    }

    pub fn get(&self, id: RobotId) -> &Robot {
        match id {
            RobotId::Board(id) => &self.robots[id],
            RobotId::Global => &self.global_robot,
        }
    }

    pub fn get_mut(&mut self, id: RobotId) -> &mut Robot {
        match id {
            RobotId::Board(id) => &mut self.robots[id],
            RobotId::Global => &mut self.global_robot,
        }
    }

    pub fn find(&self, name: &ByteString) -> Option<RobotId> {
        if &self.global_robot.name == name {
            return Some(RobotId::Global);
        }
        self.robots
            .iter()
            .position(|r| &r.name == name)
            .map(RobotId::Board)
    }
}

pub fn update_robot(
    state: &mut WorldState,
    audio: &dyn AudioEngine,
    key: Option<KeyPress>,
    world_path: &Path,
    counters: &mut Counters,
    boards: &[ByteString],
    board: &mut Board,
    board_id: usize,
    mut robots: Robots,
    robot_id: RobotId,
    reverse_scan: bool,
) -> Option<GameStateChange> {
    let robot = robots.get_mut(robot_id);
    if !robot.alive ||
        matches!(robot.status, RunStatus::FinishedRunning | RunStatus::FinishedWithoutRunning) ||
        (robot.status == RunStatus::MustRunOnReverse && !reverse_scan)
    {
        debug!("alive: {}, status: {:?}", robot.alive, robot.status);
        return None;
    }

    if !reverse_scan {
        robot.cycle_count += 1;
        if robot.cycle_count < robot.cycle {
            debug!(
                "delaying {:?} (cycle {}/{})",
                robot.name, robot.cycle_count, robot.cycle
            );
            robot.status = RunStatus::FinishedWithoutRunning;
            return None;
        }

        if let Some(dir) = robot.walk {
            move_robot(
                &mut robots,
                robot_id,
                board,
                dir,
                &mut *state.update_done,
                false,
            );
        }
    }

    let robot = robots.get_mut(robot_id);
    robot.cycle_count = 0;

    debug!("executing {:?} ({:?})", robot.name, robot_id);

    let mut lines_run = 0;
    let mut mode = Relative::None;
    let mut message_box_lines = vec![];
    let mut first_cmd = true;

    const CYCLES: u8 = 40;
    let state_change = loop {
        let robot = robots.get_mut(robot_id);
        if !robot_id.is_global() {
            if !board.thing_at(&robot.position).is_robot() {
                warn!("current robot not present at reported position ({:?}); terminating", robot.position);
                robot.alive = false;
            } else if RobotId::from(board.level_at(&robot.position).2) != robot_id {
                warn!(
                    "current robot ({:?}) does not match robot ID {:?} at reported position ({:?}); terminating",
                    RobotId::from(board.level_at(&robot.position).2),
                    robot_id,
                    robot.position
                );
                robot.alive = false;
            }
        }

        if lines_run >= CYCLES || !robot.alive {
            break None;
        }
        let cmd = robot.program.get(robot.current_line as usize).cloned();
        if !message_box_lines.is_empty() && !cmd.as_ref().map_or(false, Command::is_message_box) {
            // TODO: restore execution from current robot and cycle.
            break Some(GameStateChange::MessageBox(
                message_box_lines,
                robot.name.clone(),
                Some(robot_id),
            ));
        }

        let cmd = match cmd {
            Some(cmd) => cmd,
            None => break None,
        };

        debug!("evaluating {:?} ({})", cmd, robot.current_line);

        lines_run += 1;

        let old_mode = mem::replace(&mut mode, Relative::None);
        match run_one_command(
            state,
            audio,
            key,
            world_path,
            counters,
            boards,
            board,
            board_id,
            &mut robots,
            robot_id,
            old_mode,
            &cmd,
        ) {
            CommandResult::Advance => {
                robots.get_mut(robot_id).current_line += 1;
            }
            CommandResult::AdvanceAndChangeState(state) => {
                robots.get_mut(robot_id).current_line += 1;
                break Some(state);
            }
            CommandResult::IgnoreLine(new_mode) => {
                robots.get_mut(robot_id).current_line += 1;
                lines_run -= 1;
                match new_mode {
                    None if !message_box_lines.is_empty() => {
                        message_box_lines
                            .push(MessageBoxLine::Text("".into(), MessageBoxLineType::Plain));
                    }
                    Some(Update::MessageBox(None)) | None => (),
                    Some(Update::Mode(new_mode)) => mode = new_mode,
                    Some(Update::MessageBox(Some(line))) => {
                        message_box_lines.push(line);
                    }
                }
            }
            CommandResult::NoAdvance => {
                if cmd.is_cycle_ending() {
                    if matches!(cmd, Command::End | Command::Wait(..)) && first_cmd {
                        robots.get_mut(robot_id).status = RunStatus::FinishedWithoutRunning;
                    }
                    break None;
                }
            }
            CommandResult::EndCycle => {
                robots.get_mut(robot_id).current_line += 1;
                break None;
            }
        }

        first_cmd = false;
    };

    let robot = robots.get_mut(robot_id);
    if robot.status == RunStatus::NotRun {
        robot.status = RunStatus::FinishedRunning;
    }

    state_change
}

enum Update {
    Mode(Relative),
    MessageBox(Option<MessageBoxLine>),
}

enum CommandResult {
    NoAdvance,
    Advance,
    AdvanceAndChangeState(GameStateChange),
    IgnoreLine(Option<Update>),
    EndCycle,
}

fn run_one_command(
    state: &mut WorldState,
    audio: &dyn AudioEngine,
    key: Option<KeyPress>,
    world_path: &Path,
    counters: &mut Counters,
    boards: &[ByteString],
    board: &mut Board,
    board_id: usize,
    robots: &mut Robots,
    robot_id: RobotId,
    mode: Relative,
    cmd: &Command,
) -> CommandResult {
    match *cmd {
        Command::End => return CommandResult::NoAdvance,

        Command::Die(as_item) => {
            let robot = robots.get_mut(robot_id);
            if !robot_id.is_global() {
                board.remove_thing_at(&robot.position);
            }
            robot.alive = false;
            if as_item && !robot_id.is_global() {
                let player_pos = board.player_pos;
                let robot_pos = robot.position;
                move_level_to(
                    board,
                    robots.robots,
                    &player_pos,
                    &robot_pos,
                    &mut *state.update_done,
                );
            }
        }

        Command::Wait(ref n) => {
            let robot = robots.get(robot_id);
            let new_loc = if robot.current_loc > 0 {
                robot.current_loc - 1
            } else {
                let context = CounterContext::from(board, robot, state);
                n.resolve(counters, context) as u8
            };
            let robot = robots.get_mut(robot_id);
            robot.current_loc = new_loc;
            if robot.current_loc == 0 {
                return CommandResult::Advance;
            } else {
                return CommandResult::NoAdvance;
            }
        }

        Command::PlayerChar(ref c) => {
            let context = CounterContext::from(board, robots.get(robot_id), state);
            let c = c.resolve(counters, context);
            state.set_char_id(CharId::PlayerNorth, c);
            state.set_char_id(CharId::PlayerSouth, c);
            state.set_char_id(CharId::PlayerEast, c);
            state.set_char_id(CharId::PlayerWest, c);
        }

        Command::PlayerCharDir(ref d, ref c) => {
            let context = CounterContext::from(board, robots.get(robot_id), state);
            let c = c.resolve(counters, context);
            match dir_to_cardinal_dir(robots.get(robot_id), d) {
                Some(CardinalDirection::North) => state.set_char_id(CharId::PlayerNorth, c),
                Some(CardinalDirection::South) => state.set_char_id(CharId::PlayerSouth, c),
                Some(CardinalDirection::East) => state.set_char_id(CharId::PlayerEast, c),
                Some(CardinalDirection::West) => state.set_char_id(CharId::PlayerWest, c),
                None => (),
            }
        }

        Command::PlayerColor(ref c) => {
            let context = CounterContext::from(board, robots.get(robot_id), state);
            let c = c.resolve(counters, context);
            state.set_char_id(CharId::PlayerColor, c.0);
        }

        Command::CharEdit(ref c, ref bytes) => {
            let context = CounterContext::from(board, robots.get(robot_id), state);
            let c = c.resolve(counters, context);
            let bytes: Vec<u8> = bytes.iter().map(|b| b.resolve(counters, context)).collect();
            state.charset.nth_mut(c).copy_from_slice(&bytes);
        }

        Command::ScrollChar(ref c, ref dir) => {
            let context = CounterContext::from(board, robots.get(robot_id), state);
            let c = c.resolve(counters, context);
            let (x, y) = match dir_to_cardinal_dir(robots.get(robot_id), dir) {
                Some(CardinalDirection::North) => (0, -1),
                Some(CardinalDirection::South) => (0, 1),
                Some(CardinalDirection::East) => (1, 0),
                Some(CardinalDirection::West) => (-1, 0),
                None => (0, 0),
            };
            let data = state.charset.nth_mut(c);
            if x < 0 {
                for row in data.iter_mut() {
                    let tmp = (*row & 0b1000_0000) >> 7;
                    *row <<= 1;
                    *row = (*row & 0b1111_1110) | tmp;
                }
            } else if x > 0 {
                for row in data.iter_mut() {
                    let tmp = (*row & 0b0000_0001) << 7;
                    *row >>= 1;
                    *row = (*row & 0b0111_1111) | tmp;
                }
            }

            if y < 0 {
                let tmp = data[0].clone();
                for i in 1..=data.len() {
                    data[i - 1] = if i < data.len() { data[i] } else { tmp };
                }
            } else if y > 0 {
                let tmp = data[data.len() - 1].clone();
                for i in (1..data.len()).rev() {
                    data[i] = data[i - 1];
                }
                data[0] = tmp;
            }
        }

        Command::CopyChar(ref c1, ref c2) => {
            let context = CounterContext::from(board, robots.get(robot_id), state);
            let c1 = c1.resolve(counters, context);
            let c2 = c2.resolve(counters, context);
            let bytes: Vec<u8> = state.charset.nth(c1).to_owned();
            state.charset.nth_mut(c2).copy_from_slice(&bytes);
        }

        Command::LoadCharSet(ref c) => {
            let context = CounterContext::from(board, robots.get(robot_id), state);
            let c = c.evaluate(counters, &context);
            //TODO: handle partial charset offsets correctly.
            let path = world_path.join(c.to_string());
            match File::open(&path) {
                Ok(mut file) => {
                    let mut v = vec![];
                    file.read_to_end(&mut v).unwrap();
                    state.charset.data[0..v.len()].copy_from_slice(&v);
                }
                Err(e) => {
                    info!("Error opening charset {} ({})", path.display(), e);
                }
            }
        }

        Command::ColorFadeOut => {
            //FIXME: Hack?
            for &mut (_, ref mut intensity) in &mut state.palette.colors {
                *intensity = 0.;
            }
        }

        Command::ColorFadeIn => {
            //FIXME: Hack? Probably should respect pre-fade intensity values.
            for &mut (_, ref mut intensity) in &mut state.palette.colors {
                *intensity = 1.;
            }
        }

        Command::SetColor(ref c, ref r, ref g, ref b) => {
            let context = CounterContext::from(board, robots.get(robot_id), state);
            let c = c.resolve(counters, context);
            let r = (r.resolve(counters, context) as u8).min(63);
            let g = (g.resolve(counters, context) as u8).min(63);
            let b = (b.resolve(counters, context) as u8).min(63);
            if (c as usize) < state.palette.colors.len() {
                state.palette.colors[c as usize].0 = MzxColor { r, g, b };
            }
        }

        Command::ColorIntensity(ref c, ref n) => {
            let context = CounterContext::from(board, robots.get(robot_id), state);
            let n = n.resolve(counters, context);
            let intensity = (n as f32 / 100.).min(1.0);
            match *c {
                Some(ref c) => {
                    let c = c.resolve(counters, context);
                    if (c as usize) < state.palette.colors.len() {
                        state.palette.colors[c as usize].1 = intensity;
                    }
                }
                None => {
                    for &mut (_, ref mut i) in state.palette.colors.iter_mut() {
                        *i = intensity;
                    }
                }
            }
        }

        Command::LoadPalette(ref p) => {
            let context = CounterContext::from(board, robots.get(robot_id), state);
            let p = p.evaluate(counters, &context);
            let path = world_path.join(p.to_string());
            match File::open(&path) {
                Ok(mut file) => {
                    let mut v = vec![];
                    file.read_to_end(&mut v).unwrap();
                    for (new, (ref mut old, _)) in v.chunks(3).zip(state.palette.colors.iter_mut())
                    {
                        old.r = new[0];
                        old.g = new[1];
                        old.b = new[2];
                    }
                }
                Err(e) => {
                    info!("Error opening palette {} ({})", path.display(), e);
                }
            }
        }

        Command::ViewportSize(ref w, ref h) => {
            let context = CounterContext::from(board, robots.get(robot_id), state);
            let w = w.resolve(counters, context) as u8;
            let h = h.resolve(counters, context) as u8;
            board.viewport_size = Size(w, h);
        }

        Command::Viewport(ref x, ref y) => {
            let context = CounterContext::from(board, robots.get(robot_id), state);
            let x = x.resolve(counters, context) as u8;
            let y = y.resolve(counters, context) as u8;
            board.upper_left_viewport = Coordinate(x, y);
        }

        Command::ResetView => {
            reset_view(board);
        }

        Command::LockScroll => {
            state.scroll_locked = true;
        }

        Command::UnlockScroll => {
            state.scroll_locked = false;
        }

        Command::ScrollViewXY(ref x, ref y) => {
            let context = CounterContext::from(board, robots.get(robot_id), state);
            let coord = mode.resolve_xy(x, y, counters, context, RelativePart::First);
            debug!("scrolling to {},{}", coord.0, coord.1);
            board.scroll_offset = coord;
        }

        Command::ScrollView(ref dir, ref n) => {
            let context = CounterContext::from(board, robots.get(robot_id), state);
            let n = n.resolve(counters, context);
            let dir = dir_to_cardinal_dir(robots.get(robot_id), dir);
            match dir {
                Some(CardinalDirection::West) => {
                    if (board.scroll_offset.0 as u32) < n {
                        board.scroll_offset.0 = 0;
                    } else {
                        board.scroll_offset.0 -= n as u16;
                    }
                }
                Some(CardinalDirection::East) => {
                    if (board.scroll_offset.0 as u32 + n) as usize
                        > board.width - board.viewport_size.0 as usize
                    {
                        board.scroll_offset.0 = board.width as u16 - board.viewport_size.0 as u16;
                    } else {
                        board.scroll_offset.0 += n as u16;
                    }
                }
                Some(CardinalDirection::North) => {
                    if (board.scroll_offset.1 as u32) < n {
                        board.scroll_offset.1 = 0;
                    } else {
                        board.scroll_offset.1 -= n as u16;
                    }
                }
                Some(CardinalDirection::South) => {
                    if (board.scroll_offset.1 as u32 + n) as usize
                        > board.height - board.viewport_size.1 as usize
                    {
                        board.scroll_offset.1 = board.height as u16 - board.viewport_size.1 as u16;
                    } else {
                        board.scroll_offset.1 += n as u16;
                    }
                }
                None => (),
            };
        }

        Command::LockPlayer => {
            board.player_locked_ns = true;
            board.player_locked_ew = true;
        }

        Command::UnlockPlayer => {
            board.player_locked_ns = false;
            board.player_locked_ew = false;
            board.player_locked_attack = false
        }

        Command::LockPlayerNS => {
            board.player_locked_ns = true;
        }

        Command::LockPlayerEW => {
            board.player_locked_ew = true;
        }

        Command::LockPlayerAttack => {
            board.player_locked_attack = true;
        }

        Command::Set(ref s, ref n, ref n2) => {
            //TODO: handle special counters such as LOAD_ROBOT, SAVE_ROBOT, FREAD_OPEN, etc.
            let robot = robots.get_mut(robot_id);
            let mut context = CounterContextMut::from(board, robot, state);
            let s = s.evaluate_for_name(counters, &context);
            // A random range always treats both values as numeric counters.
            if let Some(ref n2) = *n2 {
                let mut val = n.resolve(counters, context.as_immutable()) as i32;
                let upper = n2.resolve(counters, context.as_immutable());
                let range = (upper - val).abs() as u32;
                val += rand::random::<u32>().checked_rem(range).unwrap_or(0) as i32;
                if s.is_string_name() {
                    counters.set_string(s, &mut context, val.to_string().into());
                } else {
                    counters.set(s, &mut context, val);
                }
            } else if s.is_string_name() {
                let str_val = match n {
                    SignedNumeric::Counter(ref s2) => s2.evaluate(counters, &context),
                    SignedNumeric::Literal(i) => i.to_string().into(),
                };
                counters.set_string(s, &mut context, str_val);
            } else {
                let val = n.resolve(counters, context.as_immutable()) as i32;
                counters.set(s, &mut context, val);
            }
        }

        Command::Dec(ref s, ref n, ref n2) => {
            //TODO: handle string truncation
            let context = CounterContext::from(board, robots.get(robot_id), state);
            let s = s.evaluate_for_name(counters, &context);
            let initial = counters.get(&s, &context);
            let mut val = n.resolve(counters, context) as i32;
            if let Some(ref n2) = *n2 {
                let upper = n2.resolve(counters, context);
                let range = (upper - val).abs() as u32;
                val += rand::random::<u32>().checked_rem(range).unwrap_or(0) as i32;
            }
            let mut context = CounterContextMut::from(board, robots.get_mut(robot_id), state);
            counters.set(s, &mut context, initial.wrapping_sub(val));
        }

        Command::Inc(ref s, ref n, ref n2) => {
            //TODO: handle string appending
            let context = CounterContext::from(board, robots.get(robot_id), state);
            let s = s.evaluate_for_name(counters, &context);
            let initial = counters.get(&s, &context);
            let mut val = n.resolve(counters, context);
            if let Some(ref n2) = *n2 {
                let upper = n2.resolve(counters, context);
                let range = (upper - val).abs() as u32;
                val += rand::random::<u32>().checked_rem(range).unwrap_or(0) as i32;
            }
            let mut context = CounterContextMut::from(board, robots.get_mut(robot_id), state);
            counters.set(s, &mut context, initial.wrapping_add(val));
        }

        Command::Multiply(ref s, ref n) => {
            let context = CounterContext::from(board, robots.get(robot_id), state);
            let s = s.evaluate_for_name(counters, &context);
            let initial = counters.get(&s, &context);
            let val = n.resolve(counters, context);
            let mut context = CounterContextMut::from(board, robots.get_mut(robot_id), state);
            counters.set(s, &mut context, initial.wrapping_mul(val));
        }

        Command::Divide(ref s, ref n) => {
            let context = CounterContext::from(board, robots.get(robot_id), state);
            let s = s.evaluate_for_name(counters, &context);
            let initial = counters.get(&s, &context);
            let val = n.resolve(counters, context);
            let mut context = CounterContextMut::from(board, robots.get_mut(robot_id), state);
            counters.set(s, &mut context, initial.checked_div(val).unwrap_or(initial));
        }

        Command::Modulo(ref s, ref n) => {
            let context = CounterContext::from(board, robots.get(robot_id), state);
            let s = s.evaluate_for_name(counters, &context);
            let initial = counters.get(&s, &context);
            let val = n.resolve(counters, context);
            let mut context = CounterContextMut::from(board, robots.get_mut(robot_id), state);
            counters.set(s, &mut context, initial.checked_rem(val).unwrap_or(initial));
        }

        Command::If(ref s, op, ref n, ref l) => {
            //TODO: handle ===, ?=, ?==
            let robot = robots.get_mut(robot_id);
            let context = CounterContext::from(board, robot, state);
            let val = s.evaluate(counters, &context);
            let l = l.eval(counters, context);
            let result = if s.is_string_name() {
                let cmp = match n {
                    SignedNumeric::Counter(ref b) => b.evaluate(counters, &context),
                    SignedNumeric::Literal(i) => i.to_string().into(),
                };
                match op {
                    Operator::Equals => val.case_sensitive_eq(&cmp),
                    Operator::NotEquals => !val.case_sensitive_eq(&cmp),
                    Operator::LessThan => val < cmp,
                    Operator::GreaterThan => val > cmp,
                    Operator::LessThanEquals => val <= cmp,
                    Operator::GreaterThanEquals => val >= cmp,
                }
            } else {
                let val = counters.get(&val, &context);
                let cmp = n.resolve(counters, context);
                match op {
                    Operator::Equals => val == cmp,
                    Operator::NotEquals => val != cmp,
                    Operator::LessThan => val < cmp,
                    Operator::GreaterThan => val > cmp,
                    Operator::LessThanEquals => val <= cmp,
                    Operator::GreaterThanEquals => val >= cmp,
                }
            };
            if result {
                if jump_robot_to_label(robot, l) {
                    return CommandResult::NoAdvance;
                }
            }
        }

        Command::IfAny(ref color, ref thing, ref param, ref label, negate) => {
            let robot = robots.get_mut(robot_id);
            let context = CounterContext::from(board, robot, state);
            let label = label.eval(counters, context);
            let color = color.resolve(counters, context);
            let param = param.resolve(counters, context);
            let mut result = board
                .find_extended(thing.to_u8().unwrap(), color, param)
                .is_some();
            if negate {
                result = !result;
            }
            if result {
                if jump_robot_to_label(robot, label) {
                    return CommandResult::NoAdvance;
                }
            }
        }

        Command::IfCondition(ref condition, ref l, invert) => {
            let robot = robots.get_mut(robot_id);
            let mut result = robot.is(condition, board, key);
            debug!("condition {:?}: {}", condition, result);
            if invert {
                result = !result;
            }
            if result {
                let context = CounterContext::from(board, robot, state);
                let l = l.eval(counters, context);
                if jump_robot_to_label(robot, l) {
                    return CommandResult::NoAdvance;
                }
            }
        }

        Command::IfAt(ref x, ref y, ref l) => {
            if !robot_id.is_global() {
                let robot = robots.get_mut(robot_id);
                let context = CounterContext::from(board, robot, state);
                let pos = mode.resolve_xy(x, y, counters, context, RelativePart::First);
                let l = l.eval(counters, context);
                if robot.position == pos {
                    if jump_robot_to_label(robot, l) {
                        return CommandResult::NoAdvance;
                    }
                }
            }
        }

        Command::IfThingXY(ref color, ref thing, ref param, ref x, ref y, ref l) => {
            let robot = robots.get_mut(robot_id);
            let context = CounterContext::from(board, robot, state);
            let color = color.resolve(counters, context);
            let param = param.resolve(counters, context);
            let pos = mode.resolve_xy(x, y, counters, context, RelativePart::First);
            let l = l.eval(counters, context);
            let &(board_thing, board_color, board_param) = board.level_at(&pos);
            if board_thing == thing.to_u8().unwrap()
                && color.matches(ColorValue(board_color))
                && param.matches(ParamValue(board_param))
            {
                if jump_robot_to_label(robot, l) {
                    return CommandResult::NoAdvance;
                }
            }
        }

        Command::IfThingDir(ref color, ref thing, ref param, ref dir, ref label, _)
        | Command::IfDirOfPlayer(ref dir, ref color, ref thing, ref param, ref label) => {
            let robot = robots.get_mut(robot_id);
            let context = CounterContext::from(board, robot, state);
            let color = color.resolve(counters, context);
            let param = param.resolve(counters, context);
            let l = label.eval(counters, context);
            //FIXME: ignore thingdir when global robot
            let (not, base_pos) = if let Command::IfThingDir(_, _, _, _, _, not) = cmd {
                (*not, robot.position)
            } else {
                (false, board.player_pos)
            };
            enum Source {
                Level,
                Under,
            }
            let (source, pos) = match dir_to_cardinal_dir(robot, dir) {
                Some(dir) => (Source::Level, adjust_coordinate(base_pos, board, dir)),
                None => match dir.dir {
                    Direction::Beneath => (Source::Under, Some(base_pos)),
                    _ => (Source::Level, None),
                },
            };
            if let Some(pos) = pos {
                let &(board_thing, board_color, board_param) = match source {
                    Source::Level => board.level_at(&pos),
                    Source::Under => board.under_at(&pos),
                };
                //XXXjdm thing comparisons need to account for whirlpools
                let mut success = board_thing == thing.to_u8().unwrap()
                    && color.matches(ColorValue(board_color))
                    && param.matches(ParamValue(board_param));
                if not {
                    success = !success;
                }
                if success {
                    if jump_robot_to_label(robot, l) {
                        return CommandResult::NoAdvance;
                    }
                }
            }
        }

        Command::IfPlayerXY(ref x, ref y, ref l) => {
            let robot = robots.get_mut(robot_id);
            let context = CounterContext::from(board, robot, state);
            let pos = mode.resolve_xy(x, y, counters, context, RelativePart::First);
            let l = l.eval(counters, context);
            if board.player_pos == pos {
                if jump_robot_to_label(robot, l) {
                    return CommandResult::NoAdvance;
                }
            }
        }

        Command::Change(ref c1, ref t1, ref p1, ref c2, ref t2, ref p2) => {
            let context = CounterContext::from(board, robots.get(robot_id), state);
            let c1 = c1.resolve(counters, context);
            let c2 = c2.resolve(counters, context);
            let p1 = p1.resolve(counters, context);
            let p2 = p2.resolve(counters, context);
            for &mut (ref mut id, ref mut color, ref mut param) in &mut board.level {
                if c1.matches(ColorValue(*color))
                    && p1.matches(ParamValue(*param))
                    && *t1 == Thing::from_u8(*id).unwrap()
                {
                    match c2 {
                        ExtendedColorValue::Known(c) => *color = c.0,
                        ExtendedColorValue::Unknown(Some(bg), None) => {
                            *color = (*color & 0x0F) | (bg.0 << 4)
                        }
                        ExtendedColorValue::Unknown(None, Some(fg)) => {
                            *color = (*color & 0xF0) | fg.0
                        }
                        ExtendedColorValue::Unknown(None, None) => (),
                        ExtendedColorValue::Unknown(Some(_), Some(_)) => unreachable!(),
                    }
                    *id = t2.to_u8().unwrap();
                    match p2 {
                        ExtendedParam::Any => (),
                        ExtendedParam::Specific(p) => *param = p.0,
                    }
                }
            }
        }

        Command::ChangeOverlay(ref c1, ref c2, ref chars) => {
            let context = CounterContext::from(board, robots.get(robot_id), state);
            let c1 = c1.resolve(counters, context);
            let c2 = c2.resolve(counters, context);
            let chars = chars.as_ref().map(|(ch1, ch2)| {
                (
                    ch1.resolve(counters, context),
                    ch2.resolve(counters, context),
                )
            });
            if let Some((_, ref mut overlay)) = board.overlay {
                for &mut (ref mut ch, ref mut color) in overlay.iter_mut() {
                    if c1.matches(ColorValue(*color))
                        && chars.as_ref().map_or(true, |(ch1, _)| ch1 == ch)
                    {
                        match c2 {
                            ExtendedColorValue::Known(c) => *color = c.0,
                            ExtendedColorValue::Unknown(Some(bg), None) => {
                                *color = (*color & 0x0F) | (bg.0 << 4)
                            }
                            ExtendedColorValue::Unknown(None, Some(fg)) => {
                                *color = (*color & 0xF0) | fg.0
                            }
                            ExtendedColorValue::Unknown(None, None) => (),
                            ExtendedColorValue::Unknown(Some(_), Some(_)) => unreachable!(),
                        }
                        if let Some((_, ch2)) = chars {
                            *ch = ch2;
                        }
                    }
                }
            }
        }

        Command::PutOverlay(ref c, ref ch, ref x, ref y) => {
            //TODO: handle @ prefix for MZM files
            //TODO: handle @$ prefix for string counnters
            let context = CounterContext::from(board, robots.get(robot_id), state);
            let c = c.resolve(counters, context);
            let ch = ch.resolve(counters, context);
            let pos = mode.resolve_xy(x, y, counters, context, RelativePart::First);
            if let Some((_, ref mut overlay)) = board.overlay {
                let overlay = &mut overlay[pos.1 as usize * board.width + pos.0 as usize];
                let color = match c {
                    ExtendedColorValue::Known(c) => c.0,
                    ExtendedColorValue::Unknown(Some(bg), None) => (overlay.0 & 0x0F) | (bg.0 << 4),
                    ExtendedColorValue::Unknown(None, Some(fg)) => (overlay.0 & 0xF0) | fg.0,
                    ExtendedColorValue::Unknown(None, None) => 0x07,
                    ExtendedColorValue::Unknown(Some(_), Some(_)) => unreachable!(),
                };

                *overlay = (ch, color);
            }
        }

        Command::Color(ref c) => {
            if !robot_id.is_global() {
                let robot = robots.get(robot_id);
                let context = CounterContext::from(board, robot, state);
                board.level_at_mut(&robot.position).1 = c.resolve(counters, context).0;
            }
        }

        Command::Char(ref c) => {
            let robot = robots.get_mut(robot_id);
            let context = CounterContext::from(board, robot, state);
            robot.ch = c.resolve(counters, context);
        }

        Command::Goto(ref l) => {
            let robot = robots.get_mut(robot_id);
            let context = CounterContext::from(board, robot, state);
            let l = l.eval(counters, context);
            if jump_robot_to_label(robot, l) {
                return CommandResult::NoAdvance;
            }
        }

        Command::Zap(ref l, ref n) => {
            let robot = robots.get_mut(robot_id);
            let context = CounterContext::from(board, robot, state);
            let n = n.resolve(counters, context);
            let l = l.evaluate_for_name(counters, &context);
            for _ in 0..n {
                let label = robot
                    .program
                    .iter_mut()
                    .find(|c| **c == Command::Label(l.clone()));
                if let Some(cmd) = label {
                    *cmd = Command::ZappedLabel(l.clone());
                }
            }
        }

        Command::Restore(ref l, ref n) => {
            let robot = robots.get_mut(robot_id);
            let context = CounterContext::from(board, robot, state);
            let n = n.resolve(counters, context);
            let l = l.evaluate_for_name(counters, &context);
            for _ in 0..n {
                let label = robot
                    .program
                    .iter_mut()
                    .rev()
                    .find(|c| **c == Command::ZappedLabel(l.clone()));
                if let Some(cmd) = label {
                    *cmd = Command::Label(l.clone());
                } else {
                    break;
                }
            }
        }

        Command::Send(ref r, ref l) => {
            let context = CounterContext::from(board, robots.get(robot_id), state);
            let r = r.evaluate_for_name(counters, &context);
            let l = l.eval(counters, context);
            let mut result = CommandResult::Advance;
            robots.foreach(|robot, id| {
                if r == "all" || robot.name == r {
                    let did_send = send_robot_to_label(robot, l.clone());
                    if id == robot_id && did_send {
                        result = CommandResult::NoAdvance;
                    }
                }
            });
            return result;
        }

        Command::SendDir(ref d, ref l, player) => {
            //FIXME: handle when global robot
            let robot = robots.get(robot_id);
            let basis = if player {
                RelativeDirBasis::from_player(board)
            } else {
                RelativeDirBasis::from_robot(robot)
            };
            if let Some(dir) = dir_to_cardinal_dir_rel(basis, d) {
                let source_pos = if player {
                    board.player_pos
                } else {
                    robot.position
                };
                let adjusted = adjust_coordinate(source_pos, board, dir);
                if let Some(coord) = adjusted {
                    let thing = board.thing_at(&coord);
                    if thing.is_robot() {
                        let dest_robot_id = RobotId::from(board.level_at(&coord).2);
                        let context = CounterContext::from(board, robot, state);
                        let l = l.eval(counters, context);
                        send_robot_to_label(robots.get_mut(dest_robot_id), l);
                        //FIXME: might skip a command if sending to own id and advancing
                    }
                }
            }
        }

        Command::SendXY(ref x, ref y, ref l) => {
            let context = CounterContext::from(board, robots.get(robot_id), state);
            let pos = mode.resolve_xy(x, y, counters, context, RelativePart::First);
            let l = l.eval(counters, context);
            if board.thing_at(&pos).is_robot() {
                let dest_robot_id = RobotId::from(board.level_at(&pos).2);
                send_robot_to_label(robots.get_mut(dest_robot_id), l);
                //FIXME: might skip a command if sending to own id and advancing
            }
        }

        Command::Walk(ref d) => {
            let robot = robots.get_mut(robot_id);
            robot.walk = dir_to_cardinal_dir(robot, d);
        }

        Command::Slash(ref s) => {
            let robot = robots.get_mut(robot_id);
            if robot.current_loc as usize == s.as_bytes().len() {
                robot.current_loc = 0;
                return CommandResult::Advance;
            } else {
                let dir = match s.as_bytes()[robot.current_loc as usize] {
                    b'n' => Some(CardinalDirection::North),
                    b's' => Some(CardinalDirection::South),
                    b'e' => Some(CardinalDirection::East),
                    b'w' => Some(CardinalDirection::West),
                    _ => None,
                };
                robot.current_loc += 1;
                if !robot_id.is_global() {
                    if let Some(dir) = dir {
                        move_robot(robots, robot_id, board, dir, &mut *state.update_done, true);
                    }
                }
                return CommandResult::NoAdvance;
            }
        }

        Command::Go(ref d, ref n) => {
            let robot = robots.get_mut(robot_id);
            if robot.current_loc > 0 {
                robot.current_loc -= 1;
            } else {
                robot.current_loc = {
                    let context = CounterContext::from(board, robot, state);
                    n.resolve(counters, context) as u8
                };
            }
            if robot.current_loc != 0 {
                let dir = dir_to_cardinal_dir(&robot, d);
                if !robot_id.is_global() {
                    if let Some(dir) = dir {
                        move_robot(robots, robot_id, board, dir, &mut *state.update_done, true);
                    }
                }
                return CommandResult::NoAdvance;
            } else {
                return CommandResult::Advance;
            }
        }

        Command::TryDir(ref d, ref l) => {
            let robot = robots.get_mut(robot_id);
            let dir = dir_to_cardinal_dir(robot, d);
            if let Some(dir) = dir {
                if !robot_id.is_global()
                    && move_robot(robots, robot_id, board, dir, &mut *state.update_done, true)
                        == Move::Blocked
                {
                    let robot = robots.get_mut(robot_id);
                    let context = CounterContext::from(board, robot, state);
                    let l = l.eval(counters, context);
                    if jump_robot_to_label(robot, l) {
                        return CommandResult::NoAdvance;
                    }
                }
            }
        }

        Command::Cycle(ref n) => {
            let robot = robots.get_mut(robot_id);
            let context = CounterContext::from(board, robot, state);
            robot.cycle = (n.resolve(counters, context) % 256) as u8;
        }

        Command::Explode(ref n) => {
            let robot = robots.get_mut(robot_id);
            robot.alive = false;
            if !robot_id.is_global() {
                let context = CounterContext::from(board, robot, state);
                let n = n.resolve(counters, context) as u8;
                let &mut (ref mut id, ref mut c, ref mut param) =
                    board.level_at_mut(&robot.position);
                *id = Thing::Explosion.to_u8().unwrap();
                *c = state.char_id(CharId::ExplosionStage1);
                *param = Explosion { stage: 0, size: n }.to_param();
            }
        }

        Command::OverlayMode(mode) => {
            if let Some(ref mut overlay) = board.overlay {
                overlay.0 = mode;
            }
        }

        Command::GotoXY(ref x, ref y) => {
            if !robot_id.is_global() {
                let robot = robots.get_mut(robot_id);
                let context = CounterContext::from(board, robot, state);
                let coord = mode.resolve_xy(x, y, counters, context, RelativePart::First);
                move_robot_to(
                    robots,
                    robot_id,
                    board,
                    coord,
                    &mut *state.update_done,
                );
            }
        }

        Command::RelSelf(ref part) => {
            let mode = Relative::Coordinate(*part, robots.get(robot_id).position);
            return CommandResult::IgnoreLine(Some(Update::Mode(mode)));
        }

        Command::RelPlayer(ref part) => {
            let mode = Relative::Coordinate(*part, board.player_pos);
            return CommandResult::IgnoreLine(Some(Update::Mode(mode)));
        }

        Command::RelCounters(ref part) => {
            //FIXME: casting is probably the wrong solution here
            let context = CounterContext::from(board, robots.get(robot_id), state);
            let coord = Coordinate(
                counters.get(&BuiltInCounter::Xpos.into(), &context) as u16,
                counters.get(&BuiltInCounter::Ypos.into(), &context) as u16,
            );
            let mode = Relative::Coordinate(*part, coord);
            return CommandResult::IgnoreLine(Some(Update::Mode(mode)));
        }

        Command::Teleport(ref b, ref x, ref y) => {
            let context = CounterContext::from(board, robots.get(robot_id), state);
            let b = b.evaluate_for_name(counters, &context);
            let coord = Coordinate(
                x.resolve(counters, context) as u16,
                y.resolve(counters, context) as u16,
            );
            return CommandResult::AdvanceAndChangeState(GameStateChange::Teleport(b, coord));
        }

        Command::SavePlayerPosition(ref n) => {
            let context = CounterContext::from(board, robots.get(robot_id), state);
            let n = n.resolve(counters, context) as usize;
            state.saved_positions[n] = (board_id, board.player_pos);
        }

        Command::RestorePlayerPosition(ref n) => {
            let context = CounterContext::from(board, robots.get(robot_id), state);
            let n = n.resolve(counters, context) as usize;
            let (board_id, pos) = state.saved_positions[n];
            return CommandResult::AdvanceAndChangeState(GameStateChange::Restore(board_id, pos));
        }

        Command::LoopStart => {
            robots.get_mut(robot_id).loop_count = 0;
        }

        Command::LoopFor(ref n) => {
            let robot = robots.get_mut(robot_id);
            let context = CounterContext::from(board, robot, state);
            let n = n.resolve(counters, context);
            if (robot.loop_count as u32) < n {
                let start = robot.program[0..robot.current_line as usize]
                    .iter()
                    .rev()
                    .position(|c| *c == Command::LoopStart);
                if let Some(idx) = start {
                    robot.current_line -= idx as u16 + 1;
                }
                robot.loop_count += 1;
            }
        }

        Command::Label(_) | Command::ZappedLabel(_) | Command::BlankLine => {
            return CommandResult::IgnoreLine(None)
        }

        Command::Comment(ref c) => {
            if c.as_bytes().get(0) == Some(&b'@') {
                let robot = robots.get_mut(robot_id);
                robot.name = c.as_bytes()[1..].to_owned().into();
            }
            return CommandResult::IgnoreLine(None);
        }

        Command::CopyBlock(ref src_x, ref src_y, ref w, ref h, ref dst_x, ref dst_y)
        | Command::CopyOverlayBlock(ref src_x, ref src_y, ref w, ref h, ref dst_x, ref dst_y) => {
            //TODO: handle copying to @$string destination
            let overlay = if let Command::CopyOverlayBlock(..) = cmd {
                true
            } else {
                false
            };
            let (src, w, h, dest) = {
                let context = CounterContext::from(board, robots.get(robot_id), state);
                let mut src = mode.resolve_xy(src_x, src_y, counters, context, RelativePart::First);
                if src.0 as usize > board.width {
                    src.0 = (board.width - 1) as u16;
                }
                if src.1 as usize > board.height {
                    src.1 = (board.height - 1) as u16;
                }

                let mut w = w
                    .resolve(counters, context)
                    .max(1)
                    .min(board.width as u32 - src.0 as u32) as u16;
                let mut h = h
                    .resolve(counters, context)
                    .max(1)
                    .max(1)
                    .min(board.height as u32 - src.1 as u32) as u16;

                let dest = mode.resolve_xy(dst_x, dst_y, counters, context, RelativePart::Last);
                if dest.0 + w > board.width as u16 {
                    w = (board.width as i16 - dest.0 as i16).max(0) as u16;
                }
                if dest.1 + h > board.height as u16 {
                    h = (board.height as i16 - dest.1 as i16).max(0) as u16;
                }
                (src, w, h, dest)
            };

            debug!("copying from {:?} for {}x{} to {:?}", src, w, h, dest);
            if overlay {
                board.copy_overlay(src, Size(w, h), dest);
            } else {
                board.copy(src, Size(w, h), dest);
            }
        }

        Command::WriteOverlay(ref c, ref s, ref x, ref y) => {
            let context = CounterContext::from(board, robots.get(robot_id), state);
            let pos = mode.resolve_xy(x, y, counters, context, RelativePart::First);
            let c = c.resolve(counters, context);
            let s = s.evaluate_for_name(counters, &context);
            board.write_overlay(&pos, &s, c.0);
        }

        Command::PutXY(ref color, ref thing, ref param, ref x, ref y) => {
            let context = CounterContext::from(board, robots.get(robot_id), state);
            let color = color.resolve(counters, context);
            let param = param.resolve(counters, context);
            let pos = mode.resolve_xy(x, y, counters, context, RelativePart::First);
            put_thing(board, color, *thing, param, pos, &mut *state.update_done);
        }

        Command::PutDir(ref color, ref thing, ref param, ref dir) => {
            let robot = robots.get(robot_id);
            let context = CounterContext::from(board, robot, state);
            let color = color.resolve(counters, context);
            let param = param.resolve(counters, context);
            let dir = dir_to_cardinal_dir(robot, dir);
            if let Some(dir) = dir {
                let adjusted = adjust_coordinate(robot.position, board, dir);
                if let Some(coord) = adjusted {
                    put_thing(board, color, *thing, param, coord, &mut *state.update_done);
                }
            }
        }

        Command::DuplicateSelfXY(ref x, ref y) => {
            let context = CounterContext::from(board, robots.get(robot_id), state);
            let pos = mode.resolve_xy(x, y, counters, context, RelativePart::First);

            if pos != board.player_pos {
                let new_id = robots.duplicate_self(robot_id, pos).to_param().unwrap();
                let robot = robots.get(robot_id);
                let &(thing, color, _param) = board.level_at(&robot.position);
                put_at(board, &pos, color, Thing::from_u8(thing).unwrap(), new_id, &mut *state.update_done);
            }
        }

        Command::CopyRobotXY(ref x, ref y) => {
            let context = CounterContext::from(board, robots.get(robot_id), state);
            let pos = mode.resolve_xy(x, y, counters, context, RelativePart::First);

            let &(thing, _color, param) = board.level_at(&pos);
            if Thing::from_u8(thing).unwrap().is_robot() {
                copy_robot(RobotId::from(param), robot_id, robots, board);
            }
        }

        Command::CopyRobotDir(ref d) => {
            let robot = robots.get(robot_id);
            let basis = RelativeDirBasis::from_robot(robot);
            if let Some(dir) = dir_to_cardinal_dir_rel(basis, d) {
                let adjusted = adjust_coordinate(robot.position, board, dir);
                if let Some(pos) = adjusted {
                    let &(thing, _color, param) = board.level_at(&pos);
                    if Thing::from_u8(thing).unwrap().is_robot() {
                        copy_robot(RobotId::from(param), robot_id, robots, board);
                    }
                }
            }
        }

        Command::CopyRobotNamed(ref name) => {
            let context = CounterContext::from(board, robots.get(robot_id), state);
            let name = name.evaluate_for_name(counters, &context);
            let source_id = robots.find(&name);
            if let Some(source_id) = source_id {
                copy_robot(source_id, robot_id, robots, board);
                return CommandResult::NoAdvance;
            }
        }

        Command::LockSelf(is_locked) => {
            robots.get_mut(robot_id).locked = is_locked;
        }

        Command::MesgEdge(enabled) => {
            state.message_edge = enabled;
        }

        Command::CenterMessage => {
            board.message_col = None;
        }

        Command::MessageColumn(ref n) => {
            let context = CounterContext::from(board, robots.get(robot_id), state);
            board.message_col = Some(n.resolve(counters, context) as u8);
        }

        Command::MessageRow(ref n) => {
            let context = CounterContext::from(board, robots.get(robot_id), state);
            board.message_row = (n.resolve(counters, context) as u8).min(24);
        }

        Command::MessageLine(ref s) => {
            let context = CounterContext::from(board, robots.get(robot_id), state);
            board.message_line = s.evaluate_for_name(counters, &context);
            board.remaining_message_cycles = 80;
        }

        Command::ClearMessage => {
            board.remaining_message_cycles = 0;
        }

        Command::TakeKey(ref c, ref l) => {
            let c = {
                let context = CounterContext::from(board, robots.get(robot_id), state);
                c.resolve(counters, context)
            };

            let took_key = state.take_key(c.0);
            match (l, took_key) {
                (&Some(ref l), Err(())) => {
                    let context = CounterContext::from(board, robots.get(robot_id), state);
                    let l = l.eval(counters, context);
                    let did_send = send_robot_to_label(robots.get_mut(robot_id), l);
                    if !did_send {
                        return CommandResult::NoAdvance;
                    }
                }
                _ => (),
            }
        }

        Command::GiveKey(ref c, ref l) => {
            let c = {
                let context = CounterContext::from(board, robots.get(robot_id), state);
                c.resolve(counters, context)
            };

            let has_key = state.give_key(c.0);
            match (l, has_key) {
                (&Some(ref l), Err(())) => {
                    let context = CounterContext::from(board, robots.get(robot_id), state);
                    let l = l.eval(counters, context);
                    let did_send = send_robot_to_label(robots.get_mut(robot_id), l);
                    if !did_send {
                        return CommandResult::NoAdvance;
                    }
                }
                _ => (),
            }
        }

        Command::PutPlayerXY(ref x, ref y) => {
            let context = CounterContext::from(board, robots.get(robot_id), state);
            let pos = mode.resolve_xy(x, y, counters, context, RelativePart::First);
            let old_player_pos = board.player_pos;
            move_level_to(
                board,
                robots.robots,
                &old_player_pos,
                &pos,
                &mut *state.update_done,
            );
            board.player_pos = pos;
        }

        Command::MovePlayerDir(ref dir, ref blocked) => {
            //FIXME: figure out what to do with globl robot
            let robot = robots.get_mut(robot_id);
            let context = CounterContext::from(board, robot, state);
            let blocked = blocked.as_ref().map(|b| b.eval(counters, context));
            let new_pos = dir_to_cardinal_dir(robot, dir)
                .and_then(|dir| adjust_coordinate(board.player_pos, board, dir));
            let mut is_blocked = new_pos.is_none();
            if let Some(new_pos) = new_pos {
                // FIXME: handle pushable by pushing
                let thing = board.thing_at(&new_pos);
                if thing.is_solid() {
                    is_blocked = true;
                } else {
                    let player_pos = board.player_pos;
                    move_level_to(
                        board,
                        robots.robots,
                        &player_pos,
                        &new_pos,
                        &mut *state.update_done,
                    );
                    board.player_pos = new_pos;
                }
            }
            if is_blocked {
                if let Some(blocked) = blocked {
                    let robot = robots.get_mut(robot_id);
                    if jump_robot_to_label(robot, blocked) {
                        return CommandResult::NoAdvance;
                    }
                }
            }
        }

        Command::MessageBoxLine(ref s, line_type) => {
            let robot = robots.get_mut(robot_id);
            let context = CounterContext::from(board, robot, state);
            return CommandResult::IgnoreLine(Some(Update::MessageBox(Some(
                MessageBoxLine::Text(s.evaluate_for_name(counters, &context), line_type),
            ))));
        }

        Command::MessageBoxOption(ref counter, ref label, ref text) => {
            let robot = robots.get_mut(robot_id);
            let context = CounterContext::from(board, robot, state);
            let should_display = counter.as_ref().map_or(true, |counter| {
                counters.get(&counter.evaluate_for_name(counters, &context), &context) != 0
            });
            if should_display {
                return CommandResult::IgnoreLine(Some(Update::MessageBox(Some(
                    MessageBoxLine::Option {
                        label: label.evaluate_for_name(counters, &context),
                        text: text.evaluate_for_name(counters, &context),
                    },
                ))));
            } else {
                return CommandResult::IgnoreLine(Some(Update::MessageBox(None)));
            }
        }

        Command::Mod(ref m) => {
            let robot = robots.get_mut(robot_id);
            let context = CounterContext::from(board, robot, state);
            let m = m.evaluate_for_name(counters, &context).into_string();
            audio.load_module(&m);
            board.mod_file = m;
        }

        Command::ModFadeIn(ref m) => {
            let robot = robots.get_mut(robot_id);
            let context = CounterContext::from(board, robot, state);
            let m = m.evaluate_for_name(counters, &context).into_string();
            audio.mod_fade_in(&m);
            board.mod_file = m;
        }

        Command::EndMod => {
            audio.end_module();
        }

        Command::ModFadeOut => {
            audio.mod_fade_out();
        }

        Command::JumpModOrder(ref o) => {
            let context = CounterContext::from(board, robots.get(robot_id), state);
            let o = o.resolve(counters, context);
            audio.set_mod_order(o as i32);
        }

        Command::BulletColor(ref c) => {
            let context = CounterContext::from(board, robots.get(robot_id), state);
            let c = c.resolve(counters, context);
            state.set_char_id(CharId::PlayerBulletColor, c.0);
            state.set_char_id(CharId::EnemyBulletColor, c.0);
            state.set_char_id(CharId::NeutralBulletColor, c.0);
        }

        Command::PlayerBulletColor(ref c) => {
            let context = CounterContext::from(board, robots.get(robot_id), state);
            let c = c.resolve(counters, context);
            state.set_char_id(CharId::PlayerBulletColor, c.0);
        }

        Command::EnemyBulletColor(ref c) => {
            let context = CounterContext::from(board, robots.get(robot_id), state);
            let c = c.resolve(counters, context);
            state.set_char_id(CharId::EnemyBulletColor, c.0);
        }

        Command::NeutralBulletColor(ref c) => {
            let context = CounterContext::from(board, robots.get(robot_id), state);
            let c = c.resolve(counters, context);
            state.set_char_id(CharId::NeutralBulletColor, c.0);
        }

        Command::Bullet(ref c, ref dir) => {
            let context = CounterContext::from(board, robots.get(robot_id), state);
            let c = c.resolve(counters, context);
            let (player_id, neutral_id, enemy_id) = match dir {
                CardinalDirection::North => (
                    CharId::NPlayerBullet,
                    CharId::NNeutralBullet,
                    CharId::NEnemyBullet,
                ),
                CardinalDirection::South => (
                    CharId::SPlayerBullet,
                    CharId::SNeutralBullet,
                    CharId::SEnemyBullet,
                ),
                CardinalDirection::East => (
                    CharId::EPlayerBullet,
                    CharId::ENeutralBullet,
                    CharId::EEnemyBullet,
                ),
                CardinalDirection::West => (
                    CharId::WPlayerBullet,
                    CharId::WNeutralBullet,
                    CharId::WEnemyBullet,
                ),
            };
            state.set_char_id(player_id, c);
            state.set_char_id(neutral_id, c);
            state.set_char_id(enemy_id, c);
        }

        Command::PlayerBullet(ref c, ref dir) => {
            let context = CounterContext::from(board, robots.get(robot_id), state);
            let c = c.resolve(counters, context);
            let id = match dir {
                CardinalDirection::North => CharId::NPlayerBullet,
                CardinalDirection::South => CharId::SPlayerBullet,
                CardinalDirection::East => CharId::EPlayerBullet,
                CardinalDirection::West => CharId::WPlayerBullet,
            };
            state.set_char_id(id, c);
        }

        Command::NeutralBullet(ref c, ref dir) => {
            let context = CounterContext::from(board, robots.get(robot_id), state);
            let c = c.resolve(counters, context);
            let id = match dir {
                CardinalDirection::North => CharId::NNeutralBullet,
                CardinalDirection::South => CharId::SNeutralBullet,
                CardinalDirection::East => CharId::ENeutralBullet,
                CardinalDirection::West => CharId::WNeutralBullet,
            };
            state.set_char_id(id, c);
        }

        Command::EnemyBullet(ref c, ref dir) => {
            let context = CounterContext::from(board, robots.get(robot_id), state);
            let c = c.resolve(counters, context);
            let id = match dir {
                CardinalDirection::North => CharId::NEnemyBullet,
                CardinalDirection::South => CharId::SEnemyBullet,
                CardinalDirection::East => CharId::EEnemyBullet,
                CardinalDirection::West => CharId::WEnemyBullet,
            };
            state.set_char_id(id, c);
        }

        Command::Shoot(ref dir) => {
            //FIXME: figure out what to do with global robot
            let robot = robots.get(robot_id);
            let dir = dir_to_cardinal_dir(robot, dir);
            if let Some(dir) = dir {
                let bullet_pos = adjust_coordinate(robot.position, board, dir);
                if let Some(ref bullet_pos) = bullet_pos {
                    // FIXME: shoot the solid thing
                    if !board.thing_at(bullet_pos).is_solid() {
                        put_at(
                            board,
                            bullet_pos,
                            0x07,
                            Thing::Bullet,
                            bullet_param(BulletType::Neutral, dir),
                            &mut *state.update_done,
                        );
                    }
                }
            }
        }

        Command::Board(ref dir, ref name) => {
            let robot = robots.get(robot_id);
            let context = CounterContext::from(board, robots.get(robot_id), state);
            let name = name.as_ref().map(|n| n.eval(counters, context));
            let dir = dir_to_cardinal_dir(robot, dir);
            if let Some(dir) = dir {
                let board_id = name.and_then(|n| boards.iter().position(|title| title == &*n));
                let exit = match dir {
                    CardinalDirection::North => &mut board.exits.0,
                    CardinalDirection::South => &mut board.exits.1,
                    CardinalDirection::East => &mut board.exits.2,
                    CardinalDirection::West => &mut board.exits.3,
                };
                *exit = board_id.map(|id| BoardId(id as u8));
            }
        }

        Command::Become(ref color, ref thing, ref param) => {
            let robot = robots.get_mut(robot_id);
            let context = CounterContext::from(board, robot, state);
            let color = match color.resolve(counters, context) {
                ExtendedColorValue::Known(c) => c.0,
                ExtendedColorValue::Unknown(_, _) => 0x07, //XXXjdm proper defaults
            };
            let param = match param.resolve(counters, context) {
                ExtendedParam::Specific(p) => p.0,
                ExtendedParam::Any => 0x00, //XXXjdm proper defaults
            };
            if !robot_id.is_global() {
                put_at(
                    board,
                    &robot.position,
                    color,
                    *thing,
                    param,
                    &mut *state.update_done,
                );
            } else {
                robot.alive = false;
            }
        }

        ref cmd => warn!("ignoring {:?}", cmd),
    };

    if cmd.is_cycle_ending() {
        CommandResult::EndCycle
    } else {
        CommandResult::Advance
    }
}

pub fn send_robot_to_label<S: Into<EvaluatedByteString>>(robot: &mut Robot, label: S) -> bool {
    if robot.locked {
        return false;
    }
    jump_robot_to_label(robot, label)
}

/// Returns a boolean indiciating whether the program counter has already advanced to the
/// subsequent line.
// TODO: make this return an enum instead.
pub fn jump_robot_to_label<S: Into<EvaluatedByteString>>(robot: &mut Robot, label: S) -> bool {
    let label = label.into();
    if label.as_bytes() == b"#return" {
        if let Some(line) = robot.stack.pop() {
            debug!("jumping to previous stack position");
            robot.current_loc = 0;
            robot.current_line = line;
            if robot.status == RunStatus::FinishedWithoutRunning {
                robot.status = RunStatus::MustRunOnReverse;
            }
            return true;
        }
    }
    let label_pos = robot
        .program
        .iter()
        .position(|c| c == &Command::Label(label.deref().clone()));
    if let Some(pos) = label_pos {
        debug!("jumping {:?} to {:?}", robot.name, label);
        if label.as_bytes().get(0) == Some(&b'#') {
            debug!("saving stack position");
            robot.stack.push(robot.current_line + 1);
        }
        robot.current_loc = 0;
        robot.current_line = pos as u16 + 1;
        if robot.status == RunStatus::FinishedWithoutRunning {
            robot.status = RunStatus::MustRunOnReverse;
        }
        true
    } else {
        false
    }
}
