use crate::{Board, ByteString, CardinalDirection, Coordinate, Counters, Explosion, ExplosionResult, ExtendedColorValue, ExtendedParam, KeyPress, MessageBoxLine, Robot, RunStatus, Thing, WorldState, adjust_coordinate, bullet_from_param};
use crate::audio::AudioEngine;
use crate::robot::{BuiltInLabel, RobotId, Robots, send_robot_to_label, update_robot};
use num_traits::ToPrimitive;
use std::iter;
use std::path::Path;

pub const NORTH: (i8, i8) = (0, -1);
pub const SOUTH: (i8, i8) = (0, 1);
pub const EAST: (i8, i8) = (1, 0);
pub const WEST: (i8, i8) = (-1, 0);
pub const IDLE: (i8, i8) = (0, 0);

pub enum GameStateChange {
    Teleport(ByteString, Coordinate<u16>),
    Restore(usize, Coordinate<u16>),
    MessageBox(Vec<MessageBoxLine>, ByteString, Option<RobotId>),
}

pub fn enter_board(
    state: &mut WorldState,
    audio: &dyn AudioEngine,
    board: &mut Board,
    player_pos: Coordinate<u16>,
    robots: &mut [Robot],
) {
    reset_update_done(board, &mut state.update_done);

    if board.mod_file != "*" {
        audio.load_module(&board.mod_file);
    }
    let old_pos = board.player_pos;
    if old_pos != player_pos {
        move_level_to(board, robots, &old_pos, &player_pos, &mut *state.update_done);
    }
    board.player_pos = player_pos;
    reset_view(board);
    state.scroll_locked = false;

    Robots::new(board, robots).foreach(|robot, _id| {
        send_robot_to_label(robot, BuiltInLabel::JustEntered);
    })
}

pub fn reset_view(board: &mut Board) {
    let vwidth = board.viewport_size.0 as u16;
    let vheight = board.viewport_size.1 as u16;

    let xpos = (board.player_pos.0.checked_sub(vwidth / 2))
        .unwrap_or(0)
        .min(board.width as u16 - vwidth);

    let ypos = (board.player_pos.1.checked_sub(vheight / 2))
        .unwrap_or(0)
        .min(board.height as u16 - vheight);

    board.scroll_offset = Coordinate(xpos, ypos);
}

pub fn reset_update_done(board: &Board, update_done: &mut Vec<bool>) {
    update_done.clear();
    let total_size = (board.width * board.height) as usize;
    update_done.reserve(total_size);
    update_done.extend(iter::repeat(false).take(total_size));
}

pub fn put_thing(
    board: &mut Board,
    color: ExtendedColorValue,
    thing: Thing,
    param: ExtendedParam,
    pos: Coordinate<u16>,
    update_done: &mut [bool],
) {
    let color = match color {
        ExtendedColorValue::Known(c) =>
            c.0,
        // TODO: have a table of default foreground colors for things,
        //       get the current background color at destination.
        ExtendedColorValue::Unknown(Some(_), None) |
        ExtendedColorValue::Unknown(None, Some(_)) |
        ExtendedColorValue::Unknown(None, None) |
        ExtendedColorValue::Unknown(Some(_), Some(_)) =>
            0x07, //HACK
    };

    // TODO: have a table of default parameters for things.
    let param = match param {
        ExtendedParam::Specific(p) => p.0,
        ExtendedParam::Any => 0x00, //HACK
    };

    put_at(board, &pos, color, thing, param, update_done);
}

pub fn put_at(
    board: &mut Board,
    pos: &Coordinate<u16>,
    color: u8,
    thing: Thing,
    param: u8,
    update_done: &mut [bool],
) {
    board.put_at(&pos, thing.to_u8().unwrap(), color, param);
    update_done[pos.1 as usize * board.width + pos.0 as usize] = true;
}

pub fn move_level_to(
    board: &mut Board,
    robots: &mut [Robot],
    from: &Coordinate<u16>,
    to: &Coordinate<u16>,
    update_done: &mut [bool],
) {
    board.move_level_to(robots, from, to);
    update_done[to.1 as usize * board.width + to.0 as usize] = true;
}

pub fn move_level(
    board: &mut Board,
    robots: &mut [Robot],
    pos: &Coordinate<u16>,
    xdiff: i8,
    ydiff: i8,
    update_done: &mut [bool],
) {
    board.move_level(robots, pos, xdiff, ydiff);
    let x = (pos.0 as i16 + xdiff as i16) as usize;
    let y = (pos.1 as i16 + ydiff as i16) as usize;
    update_done[y * board.width + x] = true;
}

pub fn update_board(
    state: &mut WorldState,
    audio: &dyn AudioEngine,
    key: Option<KeyPress>,
    world_path: &Path,
    counters: &mut Counters,
    boards: &[ByteString],
    board: &mut Board,
    board_id: usize,
    all_robots: &mut Vec<Robot>,
) -> Option<GameStateChange> {
    debug!("running global robot");
    let change = update_robot(
        state,
        audio,
        key,
        world_path,
        counters,
        boards,
        board,
        board_id,
        Robots::new(board, all_robots),
        RobotId::Global,
    );
    if change.is_some() {
        return change;
    }

    debug!("updating board: {},{}", board.width, board.height);
    for y in 0..board.height {
        for x in 0..board.width {
            if state.update_done[y * board.width + x] {
                debug!("already updated {},{}", x,y);
                continue;
            }

            let coord = Coordinate(x as u16, y as u16);
            if let (0, 0) = (x, y) {
                debug!("thing at 0,0: {:?}", board.thing_at(&coord));
            }
            match board.thing_at(&coord) {
                Thing::Robot | Thing::RobotPushable => {
                    let robots = Robots::new(board, all_robots);

                    debug!("running robot at {},{}", x, y);
                    let change = update_robot(
                        state,
                        audio,
                        key,
                        world_path,
                        counters,
                        boards,
                        board,
                        board_id,
                        robots,
                        RobotId::from(board.level_at(&coord).2)
                    );
                    if change.is_some() {
                        return change;
                    }
                }

                Thing::Explosion => {
                    let mut explosion = Explosion::from_param(board.level_at(&coord).2);
                    if explosion.stage == 0 {
                        if explosion.size > 0 {
                            explosion.size -= 1;
                            board.level_at_mut(&coord).2 = explosion.to_param();

                            let dirs = [
                                CardinalDirection::North,
                                CardinalDirection::South,
                                CardinalDirection::East,
                                CardinalDirection::West,
                            ];
                            for dir in &dirs {
                                let adjusted = adjust_coordinate(
                                    coord,
                                    board,
                                    *dir,
                                );
                                let coord = match adjusted {
                                    Some(coord) => coord,
                                    None => continue,
                                };
                                let thing = board.thing_at(&coord);
                                if !thing.is_solid() && thing != Thing::Explosion {
                                    put_at(
                                        board,
                                        &coord,
                                        0x00,
                                        Thing::Explosion,
                                        explosion.to_param(),
                                        &mut *state.update_done
                                    );
                                } else if thing.is_robot() {
                                    let robot_id = RobotId::from(board.level_at(&coord).2);

                                    let mut robots = Robots::new(board, all_robots);
                                    let robot = robots.get_mut(robot_id);
                                    send_robot_to_label(robot, BuiltInLabel::Bombed);
                                }
                                // TODO: hurt player.
                            }
                        }
                    }

                    if explosion.stage == 3 {
                        let (thing, color) = match board.explosion_result {
                            ExplosionResult::Nothing => (
                                Thing::Space,
                                0x07,
                            ),
                            ExplosionResult::Ash => (
                                Thing::Floor,
                                0x08,
                            ),
                            ExplosionResult::Fire => (
                                Thing::Fire,
                                0x0C,
                            ),
                        };
                        put_at(board, &coord, color, thing, 0x00, &mut *state.update_done);
                    } else {
                        explosion.stage += 1;
                        board.level_at_mut(&coord).2 = explosion.to_param();
                    }
                }

                Thing::Fire => {
                    if rand::random::<u8>() >= 20 {
                        let cur_param = board.level_at(&coord).2;
                        if cur_param < 5 {
                            board.level_at_mut(&coord).2 += 1;
                        } else {
                            board.level_at_mut(&coord).2 = 0;
                        }
                    }

                    let rval = rand::random::<u8>();
                    if rval < 8 {
                        if rval == 1 && !board.fire_burns_forever {
                            put_at(
                                board,
                                &coord,
                                0x08,
                                Thing::Floor,
                                0x00,
                                &mut *state.update_done,
                            );
                        }

                        let dirs = [
                            CardinalDirection::North,
                            CardinalDirection::South,
                            CardinalDirection::East,
                            CardinalDirection::West,
                        ];
                        for dir in &dirs {
                            let adjusted = adjust_coordinate(
                                coord,
                                board,
                                *dir,
                            );
                            let coord = match adjusted {
                                Some(coord) => coord,
                                None => continue,
                            };

                            let thing = board.thing_at(&coord);
                            let level = board.level_at(&coord);
                            let thing_id = level.0;

                            let spread =
                                (thing == Thing::Space && board.fire_burns_space) ||
                                (thing_id >= Thing::Fake.to_u8().unwrap() &&
                                 thing_id <= Thing::ThickWeb.to_u8().unwrap() &&
                                 board.fire_burns_fakes) ||
                                (thing == Thing::Tree && board.fire_burns_trees) ||
                                (level.1 == 0x06 &&
                                 board.fire_burns_brown &&
                                 thing_id < Thing::Sensor.to_u8().unwrap());

                            if spread {
                                put_at(
                                    board,
                                    &coord,
                                    0x0C,
                                    Thing::Fire,
                                    0x00,
                                    &mut *state.update_done,
                                );
                            }
                        }
                    }
                }

                Thing::OpenGate => {
                    let param = board.level_at(&coord).2;
                    if param == 0 {
                        board.level_at_mut(&coord).0 = Thing::Gate.to_u8().unwrap();
                    } else {
                        board.level_at_mut(&coord).2 -= 1;
                    }
                }

                Thing::OpenDoor => {
                    let param = board.level_at(&coord).2;
                    let cur_wait = param & 0xE0;
                    let stage = param & 0x1F;
                    const OPEN_DOOR_MOVE: &[(i8, i8)] = &[
                        WEST, NORTH, EAST, NORTH,
                        WEST, SOUTH, EAST, SOUTH,
                        IDLE, IDLE, IDLE, IDLE,
                        IDLE, IDLE, IDLE, IDLE,
                        EAST, SOUTH, WEST, SOUTH,
                        EAST, NORTH, WEST, NORTH,
                        SOUTH, EAST, SOUTH, WEST,
                        NORTH, EAST, NORTH, WEST,
                    ];
                    const OPEN_DOOR_WAIT: &[u8] = &[
                        32 , 32 , 32 , 32 , 32 , 32 , 32 , 32 ,
                        224, 224, 224, 224, 224, 224, 224, 224,
                        224, 224, 224, 224, 224, 224, 224, 224,
                        32 , 32 , 32 , 32 , 32 , 32 , 32 , 32
                    ];
                    let door_wait = OPEN_DOOR_WAIT[stage as usize];
                    let door_move = OPEN_DOOR_MOVE[stage as usize];

                    // TODO: less magic numbers.
                    if cur_wait == door_wait {
                        if param & 0x18 == 0x18 {
                            let (ref mut id, _, ref mut param) = board.level_at_mut(&coord);
                            *param &= 0x07;
                            *id = Thing::Door.to_u8().unwrap();
                        } else {
                            board.level_at_mut(&coord).2 = stage + 8;
                        }

                        if door_move != IDLE {
                            // FIXME: support pushing
                            // FIXME: check for blocked, act appropriately.
                            move_level(board, all_robots, &coord, door_move.0, door_move.1, &mut *state.update_done);
                        }
                    } else {
                        board.level_at_mut(&coord).2 = param + 0x20;
                    }
                }

                Thing::Bullet => {
                    let param = board.level_at(&coord).2;
                    let (_type_, dir) = bullet_from_param(param);
                    let new_pos = adjust_coordinate(coord, board, dir);
                    if let Some(ref new_pos) = new_pos {
                        // TODO: shot behaviour
                        let dest_thing = board.thing_at(new_pos);
                        if dest_thing.is_solid() {
                            board.remove_thing_at(&coord);
                            match dest_thing {
                                Thing::Bullet => board.remove_thing_at(&new_pos),
                                Thing::Robot | Thing::RobotPushable => {
                                    let robot_id = RobotId::from(board.level_at(&new_pos).2);
                                    let mut robots = Robots::new(board, all_robots);
                                    let robot = robots.get_mut(robot_id);
                                    send_robot_to_label(robot, BuiltInLabel::Shot);
                                }
                                // TODO: player, bombs, mines, etc.
                                _ => (),
                            }
                        } else {
                            move_level_to(board, all_robots, &coord, &new_pos, &mut *state.update_done);
                        }
                    } else {
                        board.remove_thing_at(&coord);
                    }
                }

                _ => (),
            }
        }
    }

    state.message_color += 1;
    if state.message_color > 0x0F {
        state.message_color = 0x01;
    }
    if board.remaining_message_cycles > 0 {
        board.remaining_message_cycles -= 1;
    }

    reset_update_done(board, &mut state.update_done);

    let mut robots = Robots::new(board, all_robots);
    robots.foreach(|robot, _| {
        robot.status = RunStatus::NotRun;
    });

    None
}
