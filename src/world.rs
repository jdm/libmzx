use crate::robotic::{parse_program, Command};
use crate::{
    get_bool, get_byte, get_dword, get_null_terminated_string, get_word, load_palette, Board,
    BoardId, ByteString, ColorValue, OverlayMode, Robot, World, WorldError, CHARSET_BUFFER_SIZE,
    LEGACY_BOARD_NAME_SIZE, LEGACY_ROBOT_NAME_SIZE, Scroll, Sensor,
};
use itertools::Zip;
use std::io::{Cursor, Read, Seek};
use zip::ZipArchive;

enum PropFile {
    World,
    Board(u8),
    BoardRobot(u8, u8),
    BoardScroll(u8, u8),
    BoardSensor(u8, u8),
}

impl PropFile {
    fn to_string(&self) -> String {
        match self {
            PropFile::World => "world".to_owned(),
            PropFile::Board(bid) => format!("b{:02X}", bid),
            PropFile::BoardRobot(bid, rid) => format!("b{:02X}r{:02X}", bid, rid),
            PropFile::BoardScroll(bid, sid) => format!("b{:02x}sc{:02X}", bid, sid),
            PropFile::BoardSensor(bid, sid) => format!("b{:02x}se{:02X}", bid, sid),
        }
    }
}

enum Plane {
    Id,
    Param,
    Color,
}

impl Plane {
    fn as_str(&self) -> &'static str {
        match self {
            Plane::Id => "id",
            Plane::Param => "pr",
            Plane::Color => "co",
        }
    }
}

enum WorldFile {
    GlobalRobot,
    //CustomSfx,
    Properties(PropFile),
    CharSets,
    Palette,
    BoardLevel(u8, Plane),
    BoardUnder(u8, Plane),
    BoardOverlayChar(u8),
    BoardOverlayColor(u8),
}

impl WorldFile {
    fn to_string(&self) -> String {
        match self {
            WorldFile::GlobalRobot => "gr".to_owned(),
            /*WorldFile::CustomSfx => "sfx".to_owned(),*/
            WorldFile::CharSets => "chars".to_owned(),
            WorldFile::Palette => "pal".to_owned(),
            WorldFile::BoardLevel(bid, plane) => format!("b{:02X}b{}", bid, plane.as_str()),
            WorldFile::BoardUnder(bid, plane) => format!("b{:02X}u{}", bid, plane.as_str()),
            WorldFile::BoardOverlayChar(bid) => format!("b{:02X}och", bid),
            WorldFile::BoardOverlayColor(bid) => format!("b{:02X}oco", bid),
            WorldFile::Properties(file) => file.to_string(),
        }
    }
}

trait ReadFile {
    fn read_known_file(&mut self, f: WorldFile) -> Result<Vec<u8>, ()>;
}

impl<R: Read + Seek> ReadFile for ZipArchive<R> {
    fn read_known_file(&mut self, f: WorldFile) -> Result<Vec<u8>, ()> {
        println!("opening {:?}", f.to_string());
        let mut file = self.by_name(&f.to_string()).map_err(|_| ())?;
        let mut contents = vec![];
        file.read_to_end(&mut contents).unwrap();
        Ok(contents)
    }
}

pub(crate) fn load_zip_world(buffer: &[u8]) -> Result<World, WorldError> {
    let buffer = Cursor::new(buffer);
    let mut zip = ZipArchive::new(buffer).unwrap();

    for name in zip.file_names() {
        println!("file: {:?}", name);
    }

    let world_props = zip
        .read_known_file(WorldFile::Properties(PropFile::World))
        .unwrap();
    let mut world = load_world_info(&world_props).unwrap();

    let global_robot_props = zip.read_known_file(WorldFile::GlobalRobot).unwrap();
    world.global_robot = load_robot(&global_robot_props).unwrap();

    let charset = zip.read_known_file(WorldFile::CharSets).unwrap();
    //XXXjdm support all the reserved charsets
    world
        .state
        .charset
        .data
        .copy_from_slice(&charset[0..CHARSET_BUFFER_SIZE]);
    world
        .state
        .initial_charset
        .data
        .copy_from_slice(&charset[0..CHARSET_BUFFER_SIZE]);
    println!("{:?}", world.state.charset);

    let palette = zip.read_known_file(WorldFile::Palette).unwrap();
    let palette = load_palette(&palette[0..16 * 3]); //XXXjdm support larger palettes
    world.state.palette = palette.clone();
    world.state.initial_palette = palette;

    for i in 0..world.boards.len() as u8 {
        let board_props = match zip.read_known_file(WorldFile::Properties(PropFile::Board(i))) {
            Ok(props) => props,
            Err(_) => continue,
        };
        let mut board = load_board(&board_props).unwrap();

        let level_id = zip
            .read_known_file(WorldFile::BoardLevel(i, Plane::Id))
            .unwrap();
        let level_param = zip
            .read_known_file(WorldFile::BoardLevel(i, Plane::Param))
            .unwrap();
        let level_color = zip
            .read_known_file(WorldFile::BoardLevel(i, Plane::Color))
            .unwrap();
        board.level = Zip::new((level_id, level_color, level_param)).collect();

        let under_id = zip
            .read_known_file(WorldFile::BoardUnder(i, Plane::Id))
            .unwrap();
        let under_param = zip
            .read_known_file(WorldFile::BoardUnder(i, Plane::Param))
            .unwrap();
        let under_color = zip
            .read_known_file(WorldFile::BoardUnder(i, Plane::Color))
            .unwrap();
        board.under = Zip::new((under_id, under_color, under_param)).collect();

        if let Some((_, ref mut overlay)) = board.overlay {
            let overlay_char = zip.read_known_file(WorldFile::BoardOverlayChar(i)).unwrap();
            let overlay_color = zip
                .read_known_file(WorldFile::BoardOverlayColor(i))
                .unwrap();
            *overlay = overlay_char.into_iter().zip(overlay_color).collect();
        }

        let mut robots = vec![];
        for r in 0..board.num_robots {
            let robot_props = match zip
                .read_known_file(WorldFile::Properties(PropFile::BoardRobot(i, r as u8 + 1)))
            {
                Ok(r) => r,
                Err(_) => {
                    robots.push(Robot::default());
                    continue;
                }
            };
            let robot = load_robot(&robot_props).unwrap();
            robots.push(robot);
        }

        let mut scrolls = vec![];
        for r in 0..board.num_scrolls {
            let scroll_props = match zip
                .read_known_file(WorldFile::Properties(PropFile::BoardScroll(i, r as u8 + 1)))
            {
                Ok(r) => r,
                Err(_) => {
                    scrolls.push(Scroll::default());
                    continue;
                }
            };
            let robot = load_scroll(&scroll_props).unwrap();
            scrolls.push(robot);
        }
        board.scrolls = scrolls;

        let mut sensors = vec![];
        for r in 0..board.num_sensors {
            let sensor_props = match zip
                .read_known_file(WorldFile::Properties(PropFile::BoardSensor(i, r as u8 + 1)))
            {
                Ok(r) => r,
                Err(_) => {
                    sensors.push(Sensor::default());
                    continue;
                }
            };
            let sensor = load_sensor(&sensor_props).unwrap();
            sensors.push(sensor);
        }
        board.sensors = sensors;

        board.init(&mut robots);
        world.boards[i as usize] = (board, robots);
    }

    Ok(world)
}

#[derive(Debug)]
enum WorldProp {
    Name(ByteString),
    WorldVersion(u16),
    FileVersion(u16),
    //SaveStartBoard(u8),
    //SaveHasTemp(u8),
    NumberOfBoards(u8),
    IdBlock([u8; 323]),
    /*IdMissileColor(u8),
    IdBulletColors([u8; 3]),*/
    IdBlock2([u8; 128]),
    /*StatusCounters([ByteString; 6]),*/
    EdgeColor(u8),
    FirstBoard(u8),
    /*EndgameBoard(u8),
    DeathBoard(u8),
    EndgameTeleportX(u16),
    EndgameTeleportY(u16),
    GameOverSfx(bool),
    DeathTeleportX(u16),
    DeathTeleportY(u16),
    StartingLives(u16),
    LivesLimit(u16),
    StartingHealth(u16),
    HealthLimit(u16),
    EnemyBulletsHurtOthers(bool),
    ClearMessagesOnExit(bool),
    PlayFromSwapWorld(bool),*/
}

impl WorldProp {
    //const END: u16 = 0x0000;
    const WORLD_NAME: u16 = 0x0001;
    const WORLD_VERSION: u16 = 0x0002;
    const FILE_VERSION: u16 = 0x0003;
    //const SAVE_START: u16 = 0x0004;
    //const SAVE_HAS_TEMPORARY: u16 = 0x0005;
    const NUM_BOARDS: u16 = 0x0008;
    const CHAR_ID_BLOCK_1: u16 = 0x0010;
    /*const CHAR_ID_MISSILE: u16 = 0x0011;
    const CHAR_ID_BULLETS: u16 = 0x0012;*/
    const CHAR_ID_BLOCK_3: u16 = 0x0013;
    /*const STATUS_COUNTERS: u16 = 0x0018;*/
    const EDGE_COLOR: u16 = 0x0020;
    const FIRST_BOARD: u16 = 0x0021;
    /*const ENDGAME_BOARD: u16 = 0x0022;
    const DEATH_BOARD: u16 = 0x0023;*/
}

const END_PROP: u16 = 0x0000;

fn next_prop<T>(
    mut buffer: &[u8],
    read: fn(u16, &[u8]) -> Result<T, ()>,
) -> Result<(Option<T>, &[u8]), ()> {
    loop {
        let (id, tmp_buffer) = get_word(buffer);
        if id == END_PROP {
            return Ok((None, tmp_buffer));
        }
        if tmp_buffer.len() < 4 {
            return Ok((None, tmp_buffer));
        }
        let (size, tmp_buffer) = get_dword(tmp_buffer);
        let (data, tmp_buffer) = tmp_buffer.split_at(size as usize);
        if let Ok(prop) = read(id, data) {
            return Ok((Some(prop), tmp_buffer));
        }
        buffer = tmp_buffer;
    }
}

impl WorldProp {
    fn read(id: u16, buffer: &[u8]) -> Result<WorldProp, ()> {
        Ok(match id {
            WorldProp::WORLD_NAME => {
                WorldProp::Name(get_null_terminated_string(buffer, LEGACY_BOARD_NAME_SIZE).0)
            }
            WorldProp::WORLD_VERSION => WorldProp::WorldVersion(get_word(buffer).0),
            WorldProp::FILE_VERSION => WorldProp::FileVersion(get_word(buffer).0),
            WorldProp::NUM_BOARDS => WorldProp::NumberOfBoards(get_byte(buffer).0),
            WorldProp::CHAR_ID_BLOCK_1 => {
                let mut block = [0; 323];
                block.copy_from_slice(buffer);
                WorldProp::IdBlock(block)
            }
            WorldProp::CHAR_ID_BLOCK_3 => {
                let mut block = [0; 128];
                block.copy_from_slice(buffer);
                WorldProp::IdBlock2(block)
            }
            WorldProp::EDGE_COLOR => WorldProp::EdgeColor(get_byte(buffer).0),
            WorldProp::FIRST_BOARD => WorldProp::FirstBoard(get_byte(buffer).0),
            _ => return Err(()),
        })
    }

    fn apply(self, world: &mut World) {
        match self {
            WorldProp::Name(name) => world.title = name,
            WorldProp::WorldVersion(v) => world.version = v as u32, //XXXjdm
            WorldProp::FileVersion(_v) => {}
            WorldProp::NumberOfBoards(b) => world.boards.resize_with(b as usize, Default::default),
            WorldProp::IdBlock(block) => world.state.idchars[0..323].copy_from_slice(&block),
            WorldProp::IdBlock2(block) => {
                let start = world.state.idchars.len() - 128;
                let end = world.state.idchars.len();
                world.state.idchars[start..end].copy_from_slice(&block)
            }
            WorldProp::EdgeColor(c) => world.edge_border = ColorValue(c),
            WorldProp::FirstBoard(b) => world.starting_board_number = BoardId(b),
            //_ => unimplemented!(),
        }
    }
}

fn load_world_info(mut buffer: &[u8]) -> Result<World, ()> {
    let mut world = World::default();
    loop {
        let (prop, tmp_buffer) = next_prop(buffer, WorldProp::read).unwrap();
        match prop {
            Some(prop) => {
                println!("{:?}", prop);
                prop.apply(&mut world);
            }
            None => break,
        }
        buffer = tmp_buffer;
    }

    Ok(world)
}

#[derive(Debug)]
enum ScrollProp {
    Lines(u16),
    Text(ByteString),
}

impl ScrollProp {
    const LINES: u16 = 0x0001;
    const TEXT: u16 = 0x0002;

    fn read(id: u16, buffer: &[u8]) -> Result<ScrollProp, ()> {
        Ok(match id {
            ScrollProp::LINES => ScrollProp::Lines(get_word(buffer).0),
            ScrollProp::TEXT => {
                ScrollProp::Text(get_null_terminated_string(buffer, usize::MAX).0)
            }
            _ => return Err(()),
        })
    }

    fn apply(self, scroll: &mut Scroll) {
        scroll.used = true;
        match self {
            ScrollProp::Lines(lines) => scroll.num_lines = lines,
            ScrollProp::Text(bytes) => scroll.text = bytes,
        }
    }
}

#[derive(Debug)]
enum SensorProp {
    Name(ByteString),
    Char(u8),
    Robot(ByteString),
}

impl SensorProp {
    const NAME: u16 = 0x0001;
    const CHAR: u16 = 0x0002;
    const ROBOT: u16 = 0x0003;

    fn read(id: u16, buffer: &[u8]) -> Result<SensorProp, ()> {
        Ok(match id {
            SensorProp::NAME => {
                SensorProp::Name(get_null_terminated_string(buffer, LEGACY_ROBOT_NAME_SIZE).0)
            }
            SensorProp::CHAR => SensorProp::Char(get_byte(buffer).0),
            SensorProp::ROBOT => {
                SensorProp::Robot(get_null_terminated_string(buffer, LEGACY_ROBOT_NAME_SIZE).0)
            }
            _ => return Err(()),
        })
    }

    fn apply(self, sensor: &mut Sensor) {
        sensor.used = true;
        match self {
            SensorProp::Name(name) => sensor.name = name,
            SensorProp::Char(ch) => sensor.ch = ch,
            SensorProp::Robot(robot) => sensor.target = robot,
        }
    }
}

#[derive(Debug)]
enum RobotProp {
    Name(ByteString),
    Char(u8),
    X(i16),
    Y(i16),
    Program(Vec<Command>),
}

impl RobotProp {
    const NAME: u16 = 0x0001;
    const CHAR: u16 = 0x0002;
    const X_ID: u16 = 0x0003;
    const Y_ID: u16 = 0x0004;
    const PROGRAM: u16 = 0x00FF;

    fn read(id: u16, buffer: &[u8]) -> Result<RobotProp, ()> {
        Ok(match id {
            RobotProp::NAME => {
                RobotProp::Name(get_null_terminated_string(buffer, LEGACY_ROBOT_NAME_SIZE).0)
            }
            RobotProp::CHAR => RobotProp::Char(get_byte(buffer).0),
            RobotProp::X_ID => RobotProp::X(get_word(buffer).0 as i16),
            RobotProp::Y_ID => RobotProp::Y(get_word(buffer).0 as i16),
            RobotProp::PROGRAM => RobotProp::Program(parse_program(buffer)),
            _ => return Err(()),
        })
    }

    fn apply(self, robot: &mut Robot) {
        match self {
            RobotProp::Name(name) => robot.name = name,
            RobotProp::Char(ch) => robot.ch = ch,
            RobotProp::X(x) => robot.position.0 = x as u16, //XXXjdm
            RobotProp::Y(y) => robot.position.1 = y as u16, //XXXjdm
            RobotProp::Program(program) => robot.program = program,
        }
    }
}

fn load_scroll(mut buffer: &[u8]) -> Result<Scroll, ()> {
    let mut scroll = Scroll::default();
    loop {
        let (prop, tmp_buffer) = next_prop(buffer, ScrollProp::read).unwrap();
        match prop {
            Some(prop) => {
                println!("{:?}", prop);
                prop.apply(&mut scroll);
            }
            None => break,
        }
        buffer = tmp_buffer;
    }

    Ok(scroll)
}

fn load_sensor(mut buffer: &[u8]) -> Result<Sensor, ()> {
    let mut sensor = Sensor::default();
    loop {
        let (prop, tmp_buffer) = next_prop(buffer, SensorProp::read).unwrap();
        match prop {
            Some(prop) => {
                println!("{:?}", prop);
                prop.apply(&mut sensor);
            }
            None => break,
        }
        buffer = tmp_buffer;
    }

    Ok(sensor)
}

fn load_robot(mut buffer: &[u8]) -> Result<Robot, ()> {
    let mut robot = Robot::default();
    loop {
        let (prop, tmp_buffer) = next_prop(buffer, RobotProp::read).unwrap();
        match prop {
            Some(prop) => {
                println!("{:?}", prop);
                prop.apply(&mut robot);
            }
            None => break,
        }
        buffer = tmp_buffer;
    }

    Ok(robot)
}

#[derive(Debug)]
enum BoardProp {
    Name(ByteString),
    Width(u16),
    Height(u16),
    Overlay(u8),
    Robots(u8),
    Scrolls(u8),
    Sensors(u8),
    //FileVersion(u16),
    Mod(ByteString),
    ViewX(u8),
    ViewY(u8),
    ViewW(u8),
    ViewH(u8),
    Shoot(bool),
    /*Bomb(bool),
    BurnBrown(bool),
    BurnSpace(bool),
    BurnFake(bool),
    BurnTree(bool),
    ExplosionsLeave(u8),
    SaveMode(u8),
    ForestFloor(bool),
    CollectBombs(bool),
    BurnForever(bool),*/
    North(BoardId),
    South(BoardId),
    East(BoardId),
    West(BoardId),
    /*RestardOnZap(bool),
    TimeLimit(u16),
    LockedNS(bool),
    LockedEW(bool),
    AttackLocked(bool),
    ResetOnEntry(bool),
    Charset(ByteString),
    Palette(ByteString),*/
}

impl BoardProp {
    const NAME: u16 = 0x0001;
    const WIDTH: u16 = 0x0002;
    const HEIGHT: u16 = 0x0003;
    const OVERLAY: u16 = 0x0004;
    const ROBOT_COUNT: u16 = 0x0005;
    const SCROLL_COUNT: u16 = 0x006;
    const SENSOR_COUNT: u16 = 0x007;
    const MOD: u16 = 0x0010;
    const VIEWPORT_X: u16 = 0x0011;
    const VIEWPORT_Y: u16 = 0x0012;
    const VIEWPORT_W: u16 = 0x0013;
    const VIEWPORT_H: u16 = 0x0014;
    const CAN_SHOOT: u16 = 0x0015;
    const NORTH: u16 = 0x0020;
    const SOUTH: u16 = 0x0021;
    const EAST: u16 = 0x0022;
    const WEST: u16 = 0x0023;

    fn read(id: u16, buffer: &[u8]) -> Result<BoardProp, ()> {
        Ok(match id {
            BoardProp::NAME => {
                BoardProp::Name(get_null_terminated_string(buffer, LEGACY_BOARD_NAME_SIZE).0)
            }
            BoardProp::WIDTH => BoardProp::Width(get_word(buffer).0),
            BoardProp::HEIGHT => BoardProp::Height(get_word(buffer).0),
            BoardProp::OVERLAY => BoardProp::Overlay(get_byte(buffer).0),
            BoardProp::ROBOT_COUNT => BoardProp::Robots(get_byte(buffer).0),
            BoardProp::SCROLL_COUNT => BoardProp::Scrolls(get_byte(buffer).0),
            BoardProp::SENSOR_COUNT => BoardProp::Sensors(get_byte(buffer).0),
            BoardProp::MOD => BoardProp::Mod(ByteString::from(buffer)),
            BoardProp::VIEWPORT_X => BoardProp::ViewX(get_byte(buffer).0),
            BoardProp::VIEWPORT_Y => BoardProp::ViewY(get_byte(buffer).0),
            BoardProp::VIEWPORT_W => BoardProp::ViewW(get_byte(buffer).0),
            BoardProp::VIEWPORT_H => BoardProp::ViewH(get_byte(buffer).0),
            BoardProp::CAN_SHOOT => BoardProp::Shoot(get_bool(buffer).0),
            BoardProp::NORTH => BoardProp::North(BoardId(get_byte(buffer).0)),
            BoardProp::SOUTH => BoardProp::South(BoardId(get_byte(buffer).0)),
            BoardProp::EAST => BoardProp::East(BoardId(get_byte(buffer).0)),
            BoardProp::WEST => BoardProp::West(BoardId(get_byte(buffer).0)),
            _ => return Err(()),
        })
    }

    fn apply(self, board: &mut Board) {
        match self {
            BoardProp::Name(name) => board.title = name,
            BoardProp::Width(w) => board.width = w as usize,
            BoardProp::Height(h) => board.height = h as usize,
            BoardProp::Overlay(mode) => {
                if mode != 0 {
                    board.overlay = Some((OverlayMode::from_byte(mode).unwrap(), vec![]));
                }
            }
            BoardProp::Robots(count) => {
                board.num_robots = count as usize;
            }
            BoardProp::Scrolls(count) => {
                board.num_scrolls = count as usize;
            }
            BoardProp::Sensors(count) => {
                board.num_sensors = count as usize;
            }
            BoardProp::Mod(file) => board.mod_file = file.into_string(),
            BoardProp::ViewX(x) => board.upper_left_viewport.0 = x,
            BoardProp::ViewY(y) => board.upper_left_viewport.1 = y,
            BoardProp::ViewW(w) => board.viewport_size.0 = w,
            BoardProp::ViewH(h) => board.viewport_size.1 = h,
            BoardProp::Shoot(b) => board.can_shoot = b,
            BoardProp::North(b) => board.exits.0 = Some(b),
            BoardProp::South(b) => board.exits.1 = Some(b),
            BoardProp::East(b) => board.exits.2 = Some(b),
            BoardProp::West(b) => board.exits.3 = Some(b),
        }
    }
}

fn load_board(mut buffer: &[u8]) -> Result<Board, ()> {
    let mut board = Board::default();
    loop {
        let (prop, tmp_buffer) = next_prop(buffer, BoardProp::read).unwrap();
        match prop {
            Some(prop) => {
                println!("{:?}", prop);
                prop.apply(&mut board);
            }
            None => break,
        }
        buffer = tmp_buffer;
    }

    Ok(board)
}
