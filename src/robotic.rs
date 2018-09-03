use super::{get_word, get_byte, ByteString, Direction, ColorValue, ParamValue, Thing};

#[derive(Debug)]
pub enum Operator {
    Equals,
    NotEquals,
    LessThan,
    LessThanEquals,
    GreaterThan,
    GreaterThanEquals,
}

impl From<Parameter> for Operator {
    fn from(val: Parameter) -> Operator {
        match val.as_word() {
            0 | 6 => Operator::Equals,
            5 => Operator::NotEquals,
            1 => Operator::LessThan,
            2 => Operator::GreaterThan,
            3 => Operator::GreaterThanEquals,
            4 => Operator::LessThanEquals,
            n => panic!("unexpected operator value {}", n),
        }
    }
}

#[derive(Debug)]
pub struct ModifiedDirection {
    dir: Direction,
    randp: bool,
    cw: bool,
    opp: bool,
    randnot: bool,
}

#[derive(Debug)]
pub enum Condition {
    Walking,
    Swimming,
    Firewalking,
    Touching(ModifiedDirection),
    Blocked(ModifiedDirection),
    Aligned,
    AlignedNS,
    AlignedEW,
    LastShot(ModifiedDirection),
    LastTouch(ModifiedDirection),
    RightPressed,
    LeftPressed,
    UpPressed,
    DownPressed,
    SpacePressed,
    DelPressed,
    MusicOn,
    PcSfxOn,
}

impl From<Parameter> for Condition {
    fn from(val: Parameter) -> Condition {
        let (first, second) = val.as_bytes();
        match first {
            0 => Condition::Walking,
            1 => Condition::Swimming,
            2 => Condition::Firewalking,
            3 => Condition::Touching(second.into()),
            4 => Condition::Blocked(second.into()),
            5 => Condition::Aligned,
            6 => Condition::AlignedNS,
            7 => Condition::AlignedEW,
            8 => Condition::LastShot(second.into()),
            9 => Condition::RightPressed,
            10 => Condition::LeftPressed,
            11 => Condition::UpPressed,
            12 => Condition::DownPressed,
            13 => Condition::SpacePressed,
            14 => Condition::DelPressed,
            15 => Condition::MusicOn,
            16 => Condition::PcSfxOn,
            n => panic!("unexpected condition: {}", n),
        }
    }
}

create_ordinalized_enum!(
    pub CommandOp,
    u8,
    End,
    Die,
    Wait,
    Cycle,
    Go,
    Walk,
    Become,
    Char,
    Color,
    GotoXY,
    Set,
    Inc,
    Dec,
    Unused13,
    Unused14,
    Unused15,
    If,
    Unused17,
    IfCondition,
    IfNotCondition,
    IfAny,
    IfNo,
    IfThingDir,
    IfNotThingDir,
    IfAt,
    IfDirOfPlayer,
    Double,
    Half,
    Goto,
    Send,
    Explode,
    PutDir,
    Give,
    Take,
    TakeOr,
    EndGame,
    EndLife,
    Mod,
    Sam,
    Volume,
    EndMod,
    EndSam,
    Play,
    EndPlay,
    WaitThenPlay,
    WaitPlay,
    BlankLine,
    Sfx,
    PlayIfSilent,
    Open,
    LockSelf,
    UnlockSelf,
    SendDir,
    Zap,
    Restore,
    LockPlayer,
    UnlockPlayer,
    LockPlayerNS,
    LockPlayerEW,
    LockPlayerAttak,
    MovePlayerDir,
    MovePlayerDirOr,
    PutPlayerXY,
    ObsoleteIfPlayerDir,
    ObsoleteIfNotPlayerDir,
    IfPlayerXY,
    PutPlayerDir,
    TryDir,
    RotateCW,
    RotateCCW,
    Switch,
    Shoot,
    LayBomb,
    LayBombHigh,
    ShootMissile,
    ShootSeeker,
    SpitFire,
    LazerWall,
    PutXY,
    DieItem,
    SendXY,
    CopyRobotNamed,
    CopyRobotXY,
    CopyRobotDir,
    DuplicateSelfDir,
    DuplicateSelfXY,
    BulletN,
    BulletS,
    BulletE,
    BulletW,
    GiveKey,
    GiveKeyOr,
    TakeKey,
    TakeKeyOr,
    IncRandom,
    DecRandom,
    SetRandom,
    Trade,
    SendDirPlayer,
    PutDirPlayer,
    Slash,
    MessageLine,
    MessageBoxLine,
    MessageBoxOption,
    MessageBoxMaybeOption,
    Label,
    Comment,
    ZappedLabel,
    Teleport,
    ScrollView,
    Input,
    IfInput,
    IfInputNot,
    IfInputMatches,
    PlayerChar,
    MessageBoxColorLine,
    MessageBoxCenterLine,
    MoveAll,
    Copy,
    SetEdgeColor,
    Board,
    BoardIsNone,
    CharEdit,
    BecomePushable,
    BecomeNonpushable,
    Blind,
    FireWalker,
    FreezeTime,
    SlowTime,
    Wind,
    Avalanche,
    CopyDir,
    BecomeLavaWalker,
    BecomeNonLavaWalker,
    Change,
    PlayerColor,
    BulletColor,
    MissileColor,
    MessageRow,
    RelSelf,
    RelPlayer,
    RelCounters,
    SetIdChar,
    JumpModOrder,
    Ask,
    FillHealth,
    ThickArrow,
    ThinArrow,
    SetMaxHealth,
    SavePlayerPosition,
    RestorePlayerPosition,
    ExchangePlayerPosition,
    MessageColumn,
    CenterMessage,
    ClearMessage,
    ResetView,
    ModSam,
    Volume2,
    ScrollBase,
    ScrollCorner,
    ScrollTitle,
    ScrollPointer,
    ScrollArrow,
    Viewport,
    ViewportWidth,
    Unused166,
    Unused167,
    SavePlayerPositionN,
    RestorePlayerPositionN,
    ExchangePlayerPositionN,
    RestorePlayerPositionNDuplicateSelf,
    ExchangePlayerPositionNDuplicateSelf,
    PlayerBulletN,
    PlayerBulletS,
    PlayerBulletE,
    PlayerBulletW,
    NeutralBulletN,
    NeutralBulletS,
    NeutralBulletE,
    NeutralBulletW,
    EnemyBulletN,
    EnemyBulletS,
    EnemyBulletE,
    EnemyBulletW,
    PlayerBulletColor,
    NeutralBulletColor,
    EnemyBulletColor,
    Unused188,
    Unused189,
    Unused190,
    Unused191,
    Unused192,
    RelSelfFirst,
    RelSelfLast,
    RelPlayerFirst,
    RelPlayerLast,
    RelCountersFirst,
    RelCountersLast,
    ModFadeOut,
    ModFadeIn,
    CopyBlock,
    ClipInput,
    Push,
    ScrollChar,
    FlipChar,
    CopyChar,
    Unused207,
    Unused208,
    Unused209,
    ChangeSfx,
    ColorIntensityAll,
    ColorIntensityN,
    ColorFadeOut,
    ColorFadeIn,
    SetColor,
    LoadCharSet,
    Multiply,
    Divide,
    Modulo,
    PlayerCharDir,
    Unused221,
    LoadPalette,
    Unused223,
    ModFadeTo,
    ScrollViewXY,
    SwapWorld,
    IfAlignedRobot,
    Unused228,
    LockScroll,
    UnlockScroll,
    IfFirstInput,
    PersistentGo,
    WaitModFade,
    Unused234,
    EnableSaving,
    DisableSaving,
    EnableSensorOnlySaving,
    StatusCounter,
    OverlayOn,
    OverlayStatic,
    OverlayTransparent,
    OverlayPutOverlay,
    CopyOverlayBlock,
    Unused244,
    ChangeOverlay,
    ChangeOverlayColor,
    WriteOverlay,
    Unused248,
    Unused249,
    Unused250,
    LoopStart,
    LoopFor,
    AbortLoop,
    DisableMesgEdge,
    EnableMesgEdge,
);

create_ordinalized_enum!(
    pub Item,
    u8,
    Gems,
    Ammo,
    Time,
    Score,
    Health,
    Lives,
    LoBombs,
    HiBombs,
    Coins,
);

#[derive(Debug)]
pub enum CardinalDirection {
    North,
    South,
    East,
    West,
}

#[derive(Debug)]
pub enum Byte {
    Counter(ByteString),
    Literal(u8),
}

#[derive(Debug)]
pub enum Numeric {
    Counter(ByteString),
    Literal(u16),
}

#[derive(Debug)]
pub enum SignedNumeric {
    Counter(ByteString),
    Literal(i16),
}

#[derive(Debug)]
pub enum Character {
    Counter(ByteString),
    Literal(u8),
}

#[derive(Debug)]
pub enum Color {
    Counter(ByteString),
    Literal(ColorValue),
}

#[derive(Debug)]
pub enum Command {
    End,
    Die,
    Wait(Numeric),
    Cycle(Numeric),
    Go(ModifiedDirection, Numeric),
    Walk(ModifiedDirection),
    Become(Color, Thing, ParamValue),
    Char(Character),
    Color(Color),
    GotoXY(SignedNumeric, SignedNumeric),
    Set(ByteString, Numeric),
    Inc(ByteString, Numeric),
    Dec(ByteString, Numeric),
    If(ByteString, Operator, Numeric, ByteString),
    IfCondition(Condition, ByteString, bool),
    IfAny(Color, Thing, ParamValue, ByteString, bool),
    IfThingDir(Color, Thing, ParamValue, ModifiedDirection, ByteString, bool),
    IfThingXY(Color, Thing, SignedNumeric, SignedNumeric, ByteString),
    IfAt(SignedNumeric, SignedNumeric, ByteString),
    IfDirOfPlayer(ModifiedDirection, Color, Thing, ByteString),
    Double(ByteString),
    Half(ByteString),
    Goto(ByteString),
    Send(ByteString, ByteString),
    Explode(Numeric),
    PutDir(Color, Thing, ParamValue, ModifiedDirection),
    Give(Numeric, Item),
    Take(Numeric, Item, Option<ByteString>),
    EndGame,
    EndLife,
    Mod(ByteString),
    Sam(Numeric, ByteString),
    Volume(Numeric),
    EndMod,
    EndSam,
    Play(ByteString),
    EndPlay,
    WaitThenPlay(ByteString),
    WaitPlay,
    BlankLine,
    Sfx(Numeric),
    PlayIfSilent(ByteString),
    Open(ModifiedDirection),
    LockSelf,
    UnlockSelf,
    SendDir(ModifiedDirection, ByteString),
    Zap(ByteString, Numeric),
    Restore(ByteString, Numeric),
    LockPlayer,
    UnlockPlayer,
    LockPlayerNS,
    LockPlayerEW,
    LockPlayerAttak,
    MovePlayerDir(ModifiedDirection, Option<ByteString>),
    PutPlayerXY(SignedNumeric),
    ObsoleteIfPlayerDir(ModifiedDirection, ByteString, bool),
    IfPlayerXY(SignedNumeric, SignedNumeric, ByteString),
    PutPlayerDir(ModifiedDirection),
    TryDir(ModifiedDirection, ByteString),
    RotateCW,
    RotateCCW,
    Switch(ModifiedDirection, ModifiedDirection),
    Shoot(ModifiedDirection),
    LayBomb(ModifiedDirection, bool),
    ShootMissile(ModifiedDirection),
    ShootSeeker(ModifiedDirection),
    SpitFire(ModifiedDirection),
    LazerWall(ModifiedDirection, Numeric),
    PutXY(Color, Thing, ParamValue, SignedNumeric, SignedNumeric),
    DieItem,
    SendXY(SignedNumeric, SignedNumeric, ByteString),
    CopyRobotNamed(ByteString),
    CopyRobotXY(SignedNumeric, SignedNumeric),
    CopyRobotDir(ModifiedDirection),
    DuplicateSelfDir(ModifiedDirection),
    DuplicateSelfXY(SignedNumeric, SignedNumeric),
    BulletN(Character),
    BulletS(Character),
    BulletE(Character),
    BulletW(Character),
    GiveKey(Color, Option<ByteString>),
    TakeKey(Color, Option<ByteString>),
    IncRandom(ByteString, Numeric, Numeric),
    DecRandom(ByteString, Numeric, Numeric),
    SetRandom(ByteString, Numeric, Numeric),
    Trade(Numeric, Item, Numeric, Item, ByteString),
    SendDirPlayer(ModifiedDirection, ByteString),
    PutDirPlayer(Color, Thing, ParamValue, ModifiedDirection),
    Slash(ByteString),
    MessageLine(ByteString),
    MessageBoxLine(ByteString),
    MessageBoxOption(ByteString),
    MessageBoxMaybeOption(ByteString),
    Label(ByteString),
    Comment(ByteString),
    ZappedLabel(ByteString),
    Teleport(ByteString, SignedNumeric, SignedNumeric),
    ScrollView(ModifiedDirection, Numeric),
    Input(ByteString),
    IfInput(ByteString, ByteString, bool),
    IfInputMatches(ByteString, ByteString),
    PlayerChar(Character),
    MessageBoxColorLine(ByteString),
    MessageBoxCenterLine(ByteString),
    MoveAll(Color, Thing, ModifiedDirection),
    Copy(SignedNumeric, SignedNumeric, SignedNumeric, SignedNumeric),
    SetEdgeColor(Color),
    Board(ModifiedDirection, Option<ByteString>),
    CharEdit(Character, [Byte; 14]),
    BecomePushable,
    BecomeNonpushable,
    Blind(Numeric),
    FireWalker(Numeric),
    FreezeTime(Numeric),
    SlowTime(Numeric),
    Wind(Numeric),
    Avalanche,
    CopyDir(ModifiedDirection, ModifiedDirection),
    BecomeLavaWalker,
    BecomeNonLavaWalker,
    Change(Color, Thing, Color, Thing, ParamValue),
    PlayerColor(Color),
    BulletColor(Color),
    MissileColor(Color),
    MessageRow(Numeric),
    RelSelf,
    RelPlayer,
    RelCounters,
    SetIdChar(Numeric, Character),
    JumpModOrder(Numeric),
    Ask(ByteString),
    FillHealth,
    ChangeArrowChar(ModifiedDirection, Character, bool),
    SetMaxHealth(Numeric),
    MessageColumn(Numeric),
    CenterMessage,
    ClearMessage,
    ResetView,
    ModSam(Numeric, Numeric),
    Volume2(ByteString),
    ScrollBase(Color),
    ScrollCorner(Color),
    ScrollTitle(Color),
    ScrollPointer(Color),
    ScrollArrow(Color),
    Viewport(Numeric, Numeric),
    ViewportWidth(Numeric, Numeric),
    SavePlayerPosition(Option<Numeric>),
    RestorePlayerPosition(Option<Numeric>),
    ExchangePlayerPosition(Option<Numeric>),
    RestorePlayerPositionDupSelf(Numeric),
    ExchangePlayerPositionDupSelf(Numeric),
    PlayerBullet(Character, CardinalDirection),
    NeutralBullet(Character, CardinalDirection),
    EnemyBullet(Character, CardinalDirection),
    PlayerBulletColor(Color),
    NeutralBulletColor(Color),
    EnemyBulletColor(Color),
    RelSelfFirst,
    RelSelfLast,
    RelPlayerFirst,
    RelPlayerLast,
    RelCountersFirst,
    RelCountersLast,
    ModFadeOut,
    ModFadeIn,
    CopyBlock(SignedNumeric, SignedNumeric, Numeric, Numeric, SignedNumeric, SignedNumeric),
    ClipInput,
    Push(ModifiedDirection),
    ScrollChar(Character, ModifiedDirection),
    FlipChar(Character, ModifiedDirection),
    CopyChar(Character, Character),
    ChangeSfx(Numeric, ByteString),
    ColorIntensity(Option<Numeric>, Numeric),
    ColorFadeOut,
    ColorFadeIn,
    SetColor(Numeric, Numeric, Numeric, Numeric),
    LoadCharSet(ByteString),
    Multiply(ByteString, Numeric),
    Divide(ByteString, Numeric),
    Modulo(ByteString, Numeric),
    PlayerCharDir(ModifiedDirection, Character),
    LoadPalette(ByteString),
    ModFadeTo(Numeric, Numeric),
    ScrollViewXY(SignedNumeric, SignedNumeric),
    SwapWorld(ByteString),
    IfAlignedRobot(ByteString, ByteString),
    LockScroll,
    UnlockScroll,
    IfFirstInput(ByteString, ByteString),
    PersistentGo(ByteString),
    WaitModFade,
    EnableSaving,
    DisableSaving,
    EnableSensorOnlySaving,
    StatusCounter(Numeric, ByteString),
    OverlayOn,
    OverlayStatic,
    OverlayTransparent,
    OverlayPutOverlay(Color, Character, SignedNumeric, SignedNumeric),
    CopyOverlayBlock(SignedNumeric, SignedNumeric, Numeric, Numeric, SignedNumeric, SignedNumeric),
    ChangeOverlay(Color, Color, Option<(Character, Character)>),
    WriteOverlay(Color, ByteString, SignedNumeric, SignedNumeric),
    LoopStart,
    LoopFor(Numeric),
    AbortLoop,
    DisableMesgEdge,
    EnableMesgEdge,
}

enum Parameter {
    Word(u16),
    String(ByteString),
}

impl Parameter {
    fn as_word(&self) -> u16 {
        match *self {
            Parameter::Word(ref u) => *u,
            Parameter::String(_) => panic!("unexpected string"),
        }
    }

    fn as_bytes(&self) -> (u8, u8) {
        let w = self.as_word();
        (
            (w & 0x00FF) as u8,
            ((w & 0xFF00) >> 8) as u8,
        )
    }

    fn as_string(&self) -> &ByteString {
        match *self {
            Parameter::Word(_) => panic!("unexpected word literal"),
            Parameter::String(ref s) => s,
        }
    }

    fn into_string(self) -> ByteString {
        match self {
            Parameter::Word(_) => panic!("unexpected word literal"),
            Parameter::String(s) => s,
        }
    }
}

impl From<Parameter> for Numeric {
    fn from(val: Parameter) -> Numeric {
        match val {
            Parameter::Word(u) => Numeric::Literal(u),
            Parameter::String(s) => Numeric::Counter(s),
        }
    }
}

impl From<Parameter> for Character {
    fn from(val: Parameter) -> Character {
        match val {
            Parameter::Word(u) => Character::Literal(u as u8),
            Parameter::String(s) => Character::Counter(s),
        }
    }
}

impl From<u8> for ModifiedDirection {
    fn from(val: u8) -> ModifiedDirection {
        let dir = Direction::from_ordinal(val & 0x0F).expect("unexpected direction value");
        ModifiedDirection {
            dir: dir,
            randp: val & 0x10 != 0,
            cw: val & 0x20 != 0,
            opp: val & 0x40 != 0,
            randnot: val & 0x80 != 0,
        }
    }
}

impl From<Parameter> for ModifiedDirection {
    fn from(val: Parameter) -> ModifiedDirection {
        debug!("converting {:?} to direction", val.as_word());
        (val.as_word() as u8).into()
    }
}

impl From<Parameter> for Thing {
    fn from(val: Parameter) -> Thing {
        Thing::from_ordinal(val.as_word() as u8).expect("unexpected thing value")
    }
}

impl From<Parameter> for Color {
    fn from(val: Parameter) -> Color {
        match val {
            Parameter::Word(u) => Color::Literal(ColorValue(u as u8)),
            Parameter::String(s) => Color::Counter(s),
        }
    }
}

impl From<Parameter> for ParamValue {
    fn from(val: Parameter) -> ParamValue {
        ParamValue(val.as_word() as u8)
    }
}

impl From<Parameter> for SignedNumeric {
    fn from(val: Parameter) -> SignedNumeric {
        match val {
            Parameter::Word(u) => SignedNumeric::Literal(u as i16),
            Parameter::String(s) => SignedNumeric::Counter(s),
        }
    }
}

impl From<Parameter> for ByteString {
    fn from(val: Parameter) -> ByteString {
        val.into_string()
    }
}

fn get_robotic_parameter(buffer: &[u8]) -> (Parameter, &[u8]) {
    let (type_, buffer) = get_byte(buffer);
    debug!("parameter type {}", type_);
    match type_ {
        0 => {
            let (word, buffer) = get_word(buffer);
            (Parameter::Word(word), buffer)
        }
        1 => {
            let (_, buffer) = get_word(buffer);
            (Parameter::String(Default::default()), buffer)
        }
        2 => {
            let (first, buffer) = get_byte(buffer);
            let (_second, buffer) = get_byte(buffer);
            (Parameter::String(ByteString(vec![first])), buffer)
        }
        n => {
            let (contents, remainder) = buffer.split_at(n as usize);
            (Parameter::String(ByteString(contents.to_vec())), remainder)
        }
    }
}

fn one_arg<F, T>(buffer: &[u8], cmd: F) -> Command
    where F: Fn(T) -> Command,
          T: From<Parameter>,
{
    cmd(get_robotic_parameter(buffer).0.into())
}

fn two_args<F, T, U>(buffer: &[u8], cmd: F) -> Command
    where F: Fn(T, U) -> Command,
          T: From<Parameter>,
          U: From<Parameter>,
{
    let (param1, buffer) = get_robotic_parameter(buffer);
    let (param2, _buffer) = get_robotic_parameter(buffer);
    cmd(param1.into(), param2.into())
}

fn three_args<F, T, U, V>(buffer: &[u8], cmd: F) -> Command
    where F: Fn(T, U, V) -> Command,
          T: From<Parameter>,
          U: From<Parameter>,
          V: From<Parameter>,
{
    let (param1, buffer) = get_robotic_parameter(buffer);
    let (param2, buffer) = get_robotic_parameter(buffer);
    let (param3, _buffer) = get_robotic_parameter(buffer);
    cmd(param1.into(), param2.into(), param3.into())
}

fn four_args<F, T, U, V, W>(buffer: &[u8], cmd: F) -> Command
    where F: Fn(T, U, V, W) -> Command,
          T: From<Parameter>,
          U: From<Parameter>,
          V: From<Parameter>,
          W: From<Parameter>,
{
    let (param1, buffer) = get_robotic_parameter(buffer);
    let (param2, buffer) = get_robotic_parameter(buffer);
    let (param3, buffer) = get_robotic_parameter(buffer);
    let (param4, _buffer) = get_robotic_parameter(buffer);
    cmd(param1.into(), param2.into(), param3.into(), param4.into())
}

fn parse_opcode(buffer: &[u8], op: CommandOp) -> Option<Command> {
    debug!("parsing {:?}", op);
    let cmd = match op {
        CommandOp::End => Command::End,
        CommandOp::Die => Command::Die,
        CommandOp::Wait => one_arg(buffer, Command::Wait),
        CommandOp::Cycle => one_arg(buffer, Command::Cycle),
        CommandOp::Go => two_args(buffer, Command::Go),
        CommandOp::Walk => one_arg(buffer, Command::Walk),
        CommandOp::Become => three_args(buffer, Command::Become),
        CommandOp::Char => one_arg(buffer, Command::Char),
        CommandOp::Color => one_arg(buffer, Command::Color),
        CommandOp::GotoXY => two_args(buffer, Command::GotoXY),
        CommandOp::Set => two_args(buffer, Command::Set),
        CommandOp::Inc => two_args(buffer, Command::Inc),
        CommandOp::Dec => two_args(buffer, Command::Dec),
        CommandOp::Unused13 => two_args(buffer, Command::Set),
        CommandOp::Unused14 => two_args(buffer, Command::Inc),
        CommandOp::Unused15 => two_args(buffer, Command::Dec),
        CommandOp::If | CommandOp::Unused17 => four_args(buffer, Command::If),
        CommandOp::IfCondition | CommandOp::IfNotCondition => {
            let (param1, buffer) = get_robotic_parameter(buffer);
            let (param2, _buffer) = get_robotic_parameter(buffer);
            Command::IfCondition(
                param1.into(),
                param2.into(),
                op == CommandOp::IfNotCondition,
            )
        }
        CommandOp::IfAny | CommandOp::IfNo => {
            let (param1, buffer) = get_robotic_parameter(buffer);
            let (param2, buffer) = get_robotic_parameter(buffer);
            let (param3, buffer) = get_robotic_parameter(buffer);
            let (param4, _buffer) = get_robotic_parameter(buffer);
            Command::IfAny(
                param1.into(),
                param2.into(),
                param3.into(),
                param4.into(),
                op == CommandOp::IfNo,
            )
        }
        CommandOp::IfThingDir | CommandOp::IfNotThingDir => {
            let (param1, buffer) = get_robotic_parameter(buffer);
            let (param2, buffer) = get_robotic_parameter(buffer);
            let (param3, buffer) = get_robotic_parameter(buffer);
            let (param4, buffer) = get_robotic_parameter(buffer);
            let (param5, _buffer) = get_robotic_parameter(buffer);
            Command::IfThingDir(
                param1.into(),
                param2.into(),
                param3.into(),
                param4.into(),
                param5.into(),
                op == CommandOp::IfNotThingDir,
            )
        }
        _ => return None,
    };
    Some(cmd)
}

pub(crate) fn parse_program(buffer: &[u8]) -> Vec<Command> {
    let mut commands = vec![];
    if buffer.is_empty() {
        return commands;
    }

    let (initial, mut buffer) = get_byte(buffer);
    assert_eq!(initial as i8, -1);
    loop {
        let (total, new_buffer) = get_byte(buffer);
        if total == 0 {
            break;
        }
        let total = total as usize;
        let (op, new_buffer) = get_byte(new_buffer);
        let cmd = CommandOp::from_ordinal(op).expect("unknown opcode");
        if let Some(cmd) = parse_opcode(&new_buffer[0..total-1], cmd) {
            commands.push(cmd)
        }
        let (total_dup, new_buffer) = get_byte(&new_buffer[total-1..]);
        assert_eq!(total, total_dup as usize);
        buffer = new_buffer;
    }
    commands
}
