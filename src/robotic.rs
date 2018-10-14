use num_traits::FromPrimitive;
use std::mem;
use super::{
    get_word, get_byte, ByteString, Direction, ColorValue, ParamValue, Thing, Counters,
    CardinalDirection, OverlayMode, CHAR_BYTES, Robot
};

#[derive(Copy, Clone, Debug, PartialEq)]
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

#[derive(Clone, Debug, PartialEq)]
pub enum Param {
    Counter(ByteString),
    Literal(u16),
}


#[derive(Copy, Clone, Debug, PartialEq)]
pub enum ExtendedParam {
    Specific(ParamValue),
    Any,
}

impl ExtendedParam {
    pub fn matches(&self, other: ParamValue) -> bool {
        match *self {
            ExtendedParam::Specific(p) => p == other,
            ExtendedParam::Any => true,
        }
    }
}

impl Resolve for Param {
    type Output = ExtendedParam;
    fn resolve(&self, counters: &Counters, context: &Robot) -> Self::Output {
        let v = match *self {
            Param::Counter(ref s) => counters.get(s, context) as u16,
            Param::Literal(p) => p,
        };
        if v == 256 {
            ExtendedParam::Any
        } else {
            ExtendedParam::Specific(ParamValue(v as u8))
        }
    }
}

#[derive(Copy, Clone, Debug, PartialEq)]
pub struct ModifiedDirection {
    pub dir: Direction,
    pub randp: bool,
    pub cw: bool,
    pub opp: bool,
    pub randnot: bool,
}

#[derive(Copy, Clone, Debug, PartialEq)]
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
    LastTouched(ModifiedDirection),
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
            9 => Condition::LastTouched(second.into()),
            10 => Condition::RightPressed,
            11 => Condition::LeftPressed,
            12 => Condition::UpPressed,
            13 => Condition::DownPressed,
            14 => Condition::SpacePressed,
            15 => Condition::DelPressed,
            16 => Condition::MusicOn,
            17 => Condition::PcSfxOn,
            n => panic!("unexpected condition: {}", n),
        }
    }
}

enum_from_primitive! {
    #[derive(Debug, PartialEq)]
    pub enum CommandOp {
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
        IfThingXY,
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
        LockPlayerAttack,
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
        ViewportSize,
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
        PutOverlay,
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
    }
}

#[derive(Copy, Clone, Debug, Primitive, PartialEq)]
pub enum Item {
    Gems = 0,
    Ammo = 1,
    Time = 2,
    Score = 3,
    Health = 4,
    Lives = 5,
    LoBombs = 6,
    HiBombs = 7,
    Coins = 8,
}

impl From<Parameter> for Item {
    fn from(val: Parameter) -> Item {
        Item::from_u16(val.as_word()).expect("unexpected item value")
    }
}

pub trait Resolve {
    type Output;
    fn resolve(&self, counters: &Counters, context: &Robot) -> Self::Output;
}

#[derive(Clone, Debug, PartialEq)]
pub enum Byte {
    Counter(ByteString),
    Literal(u8),
}

impl Resolve for Byte {
    type Output = u8;
    fn resolve(&self, counters: &Counters, context: &Robot) -> Self::Output {
        match *self {
            Byte::Counter(ref s) => counters.get(s, context) as u8,
            Byte::Literal(b) => b,
        }
    }
}

#[derive(Clone, Debug, PartialEq)]
pub enum Numeric {
    Counter(ByteString),
    Literal(u16),
}

impl Resolve for Numeric {
    type Output = u16;
    fn resolve(&self, counters: &Counters, context: &Robot) -> Self::Output {
        match *self {
            Numeric::Counter(ref s) => counters.get(s, context) as u16,
            Numeric::Literal(u) => u,
        }
    }
}

#[derive(Clone, Debug, PartialEq)]
pub enum SignedNumeric {
    Counter(ByteString),
    Literal(i16),
}

impl Resolve for SignedNumeric {
    type Output = i16;
    fn resolve(&self, counters: &Counters, context: &Robot) -> Self::Output {
        match *self {
            SignedNumeric::Counter(ref s) => counters.get(s, context),
            SignedNumeric::Literal(u) => u,
        }
    }
}

#[derive(Clone, Debug, PartialEq)]
pub enum Character {
    Counter(ByteString),
    Literal(u8),
}

impl Resolve for Character {
    type Output = u8;
    fn resolve(&self, counters: &Counters, context: &Robot) -> Self::Output {
        match *self {
            Character::Counter(ref s) => counters.get(s, context) as u8,
            Character::Literal(u) => u,
        }
    }
}

#[derive(Clone, Debug, PartialEq)]
pub enum ExtendedColor {
    Counter(ByteString),
    Literal(u16),
}

pub enum ExtendedColorValue {
    Known(ColorValue),
    Unknown(Option<ColorValue>, Option<ColorValue>),
}

impl ExtendedColorValue {
    fn new(c: u16) -> ExtendedColorValue {
        if c & 0xFF00 == 0 {
            ExtendedColorValue::Known(ColorValue(c as u8))
        } else {
            match c & 0xFF {
                v @ 0...15 => ExtendedColorValue::Unknown(None, Some(ColorValue(v as u8))),
                v @ 16...31 => ExtendedColorValue::Unknown(Some(ColorValue(v as u8)), None),
                32 => ExtendedColorValue::Unknown(None, None),
                _ => unreachable!(),
            }
        }
    }

    pub fn matches(&self, other: ColorValue) -> bool {
        match *self {
            ExtendedColorValue::Known(c) =>
                c == other,
            ExtendedColorValue::Unknown(Some(bg), None) =>
                (other.0 & 0xF0) >> 4 == bg.0,
            ExtendedColorValue::Unknown(None, Some(fg)) =>
                (other.0 & 0x0F) == fg.0,
            ExtendedColorValue::Unknown(None, None) =>
                true,
            ExtendedColorValue::Unknown(Some(_), Some(_)) =>
                unreachable!(),
        }
    }
}

impl Resolve for ExtendedColor {
    type Output = ExtendedColorValue;
    fn resolve(&self, counters: &Counters, context: &Robot) -> Self::Output {
        match *self {
            ExtendedColor::Counter(ref s) => ExtendedColorValue::new(counters.get(s, context) as u16),
            ExtendedColor::Literal(c) => ExtendedColorValue::new(c),
        }
    }
}

impl From<Parameter> for ExtendedColor {
    fn from(val: Parameter) -> ExtendedColor {
        match val {
            Parameter::Word(u) => ExtendedColor::Literal(u),
            Parameter::String(s) => ExtendedColor::Counter(s),
        }
    }
}

#[derive(Clone, Debug, PartialEq)]
pub enum Color {
    Counter(ByteString),
    Literal(ColorValue),
}

impl Resolve for Color {
    type Output = ColorValue;
    fn resolve(&self, counters: &Counters, context: &Robot) -> Self::Output {
        match *self {
            Color::Counter(ref s) => ColorValue(counters.get(s, context) as u8),
            Color::Literal(c) => c,
        }
    }
}

#[derive(Copy, Clone, Debug, PartialEq)]
pub enum RelativePart {
    First,
    Last,
}

#[derive(Clone, Debug, PartialEq)]
pub enum Command {
    End,
    Die,
    Wait(Numeric),
    Cycle(Numeric),
    Go(ModifiedDirection, Numeric),
    Walk(ModifiedDirection),
    Become(ExtendedColor, Thing, Param),
    Char(Character),
    Color(Color),
    GotoXY(SignedNumeric, SignedNumeric),
    Set(ByteString, Numeric, Option<Numeric>),
    Inc(ByteString, Numeric, Option<Numeric>),
    Dec(ByteString, Numeric, Option<Numeric>),
    If(ByteString, Operator, Numeric, ByteString),
    IfCondition(Condition, ByteString, bool),
    IfAny(ExtendedColor, Thing, Param, ByteString, bool),
    IfThingDir(ExtendedColor, Thing, Param, ModifiedDirection, ByteString, bool),
    IfThingXY(ExtendedColor, Thing, Param, SignedNumeric, SignedNumeric, ByteString),
    IfAt(SignedNumeric, SignedNumeric, ByteString),
    IfDirOfPlayer(ModifiedDirection, ExtendedColor, Thing, Param, ByteString),
    Double(ByteString),
    Half(ByteString),
    Goto(ByteString),
    Send(ByteString, ByteString),
    Explode(Numeric),
    PutDir(ExtendedColor, Thing, Param, ModifiedDirection),
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
    LockPlayerAttack,
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
    PutXY(ExtendedColor, Thing, Param, SignedNumeric, SignedNumeric),
    DieItem,
    SendXY(SignedNumeric, SignedNumeric, ByteString),
    CopyRobotNamed(ByteString),
    CopyRobotXY(SignedNumeric, SignedNumeric),
    CopyRobotDir(ModifiedDirection),
    DuplicateSelfDir(ModifiedDirection),
    DuplicateSelfXY(SignedNumeric, SignedNumeric),
    Bullet(Character, CardinalDirection),
    GiveKey(Color, Option<ByteString>),
    TakeKey(Color, Option<ByteString>),
    Trade(Numeric, Item, Numeric, Item, ByteString),
    SendDirPlayer(ModifiedDirection, ByteString),
    PutDirPlayer(ExtendedColor, Thing, Param, ModifiedDirection),
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
    MoveAll(ExtendedColor, Thing, ModifiedDirection),
    Copy(SignedNumeric, SignedNumeric, SignedNumeric, SignedNumeric),
    SetEdgeColor(Color),
    Board(ModifiedDirection, Option<ByteString>),
    CharEdit(Character, [Byte; CHAR_BYTES]),
    BecomePushable(bool),
    Blind(Numeric),
    FireWalker(Numeric),
    FreezeTime(Numeric),
    SlowTime(Numeric),
    Wind(Numeric),
    Avalanche,
    CopyDir(ModifiedDirection, ModifiedDirection),
    BecomeLavaWalker(bool),
    Change(ExtendedColor, Thing, Param, ExtendedColor, Thing, Param),
    PlayerColor(Color),
    BulletColor(Color),
    MissileColor(Color),
    MessageRow(Numeric),
    RelSelf(Option<RelativePart>),
    RelPlayer(Option<RelativePart>),
    RelCounters(Option<RelativePart>),
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
    ViewportSize(Numeric, Numeric),
    SavePlayerPosition(Numeric),
    RestorePlayerPosition(Numeric),
    ExchangePlayerPosition(Numeric),
    RestorePlayerPositionDupSelf(Numeric),
    ExchangePlayerPositionDupSelf(Numeric),
    PlayerBullet(Character, CardinalDirection),
    NeutralBullet(Character, CardinalDirection),
    EnemyBullet(Character, CardinalDirection),
    PlayerBulletColor(Color),
    NeutralBulletColor(Color),
    EnemyBulletColor(Color),
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
    OverlayMode(OverlayMode),
    PutOverlay(ExtendedColor, Character, SignedNumeric, SignedNumeric),
    CopyOverlayBlock(SignedNumeric, SignedNumeric, Numeric, Numeric, SignedNumeric, SignedNumeric),
    ChangeOverlay(ExtendedColor, ExtendedColor, Option<(Character, Character)>),
    WriteOverlay(Color, ByteString, SignedNumeric, SignedNumeric),
    LoopStart,
    LoopFor(Numeric),
    AbortLoop,
    MesgEdge(bool),
}

impl Command {
    pub fn is_cycle_ending(&self) -> bool {
        match *self {
            Command::End |
            Command::Die |
            Command::Wait(..) |
            Command::Go(..) |
            Command::Become(..) |
            Command::GotoXY(..) |
            Command::Explode(..) |
            Command::MovePlayerDir(..) |
            Command::TryDir(..) |
            Command::DieItem |
            Command::CopyRobotNamed(..) |
            Command::CopyRobotXY(..) |
            Command::CopyRobotDir(..) |
            Command::Slash(..) |
            Command::Teleport(..) |
            Command::MoveAll(..) |
            Command::Copy(..) |
            Command::CopyDir(..) |
            Command::CopyBlock(..) |
            Command::ColorFadeIn |
            Command::ColorFadeOut |
            Command::SwapWorld(..) |
            Command::CopyOverlayBlock(..) => true,
            _ => false,
        }
    }
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

    fn into_string(self) -> ByteString {
        match self {
            Parameter::Word(_) => panic!("unexpected word literal"),
            Parameter::String(s) => s,
        }
    }
}

impl From<Parameter> for Byte {
    fn from(val: Parameter) -> Byte {
        match val {
            Parameter::Word(u) => Byte::Literal(u as u8),
            Parameter::String(s) => Byte::Counter(s),
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
        let dir = Direction::from_u8(val & 0x0F).expect("unexpected direction value");
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
        Thing::from_u16(val.as_word()).expect("unexpected thing value")
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

impl From<Parameter> for Param {
    fn from(val: Parameter) -> Param {
        match val {
            Parameter::String(s) => Param::Counter(s),
            Parameter::Word(w) => Param::Literal(w),
        }
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
            let (first, buffer) = get_byte(buffer);
            assert_eq!(first, 0);
            (Parameter::String(Default::default()), buffer)
        }
        2 => {
            let (first, buffer) = get_byte(buffer);
            let (second, buffer) = get_byte(buffer);
            assert_eq!(second, 0);
            (Parameter::String(ByteString(vec![first])), buffer)
        }
        n => {
            // Don't include the null byte.
            let (contents, remainder) = buffer.split_at(n as usize - 1);
            assert_ne!(contents.last(), Some(&b'\0'));
            // Skip the ignored null byte.
            (Parameter::String(ByteString(contents.to_vec())), &remainder[1..])
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

fn five_args<F, T, U, V, W, X>(buffer: &[u8], cmd: F) -> Command
    where F: Fn(T, U, V, W, X) -> Command,
          T: From<Parameter>,
          U: From<Parameter>,
          V: From<Parameter>,
          W: From<Parameter>,
          X: From<Parameter>,
{
    let (param1, buffer) = get_robotic_parameter(buffer);
    let (param2, buffer) = get_robotic_parameter(buffer);
    let (param3, buffer) = get_robotic_parameter(buffer);
    let (param4, buffer) = get_robotic_parameter(buffer);
    let (param5, _buffer) = get_robotic_parameter(buffer);
    cmd(param1.into(), param2.into(), param3.into(), param4.into(), param5.into())
}

fn six_args<F, T, U, V, W, X, Y>(buffer: &[u8], cmd: F) -> Command
    where F: Fn(T, U, V, W, X, Y) -> Command,
          T: From<Parameter>,
          U: From<Parameter>,
          V: From<Parameter>,
          W: From<Parameter>,
          X: From<Parameter>,
          Y: From<Parameter>,
{
    let (param1, buffer) = get_robotic_parameter(buffer);
    let (param2, buffer) = get_robotic_parameter(buffer);
    let (param3, buffer) = get_robotic_parameter(buffer);
    let (param4, buffer) = get_robotic_parameter(buffer);
    let (param5, buffer) = get_robotic_parameter(buffer);
    let (param6, _buffer) = get_robotic_parameter(buffer);
    cmd(param1.into(), param2.into(), param3.into(), param4.into(), param5.into(), param6.into())
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
        CommandOp::Set | CommandOp::SetRandom => {
            let (param1, buffer) = get_robotic_parameter(buffer);
            let (param2, buffer) = get_robotic_parameter(buffer);
            let param3 = if op == CommandOp::SetRandom {
                let (param, _buffer) = get_robotic_parameter(buffer);
                Some(param)
            } else {
                None
            };
            Command::Set(
                param1.into(),
                param2.into(),
                param3.map(|p| p.into()),
            )
        }
        CommandOp::Inc | CommandOp::IncRandom => {
            let (param1, buffer) = get_robotic_parameter(buffer);
            let (param2, buffer) = get_robotic_parameter(buffer);
            let param3 = if op == CommandOp::IncRandom {
                let (param, _buffer) = get_robotic_parameter(buffer);
                Some(param)
            } else {
                None
            };
            Command::Inc(
                param1.into(),
                param2.into(),
                param3.map(|p| p.into()),
            )
        }
        CommandOp::Dec | CommandOp::DecRandom => {
            let (param1, buffer) = get_robotic_parameter(buffer);
            let (param2, buffer) = get_robotic_parameter(buffer);
            let param3 = if op == CommandOp::DecRandom {
                let (param, _buffer) = get_robotic_parameter(buffer);
                Some(param)
            } else {
                None
            };
            Command::Dec(
                param1.into(),
                param2.into(),
                param3.map(|p| p.into()),
            )
        }
        CommandOp::Unused13 => {
            let (param1, buffer) = get_robotic_parameter(buffer);
            let (param2, _buffer) = get_robotic_parameter(buffer);
            Command::Set(
                param1.into(),
                param2.into(),
                None,
            )
        }
        CommandOp::Unused14 => {
            let (param1, buffer) = get_robotic_parameter(buffer);
            let (param2, _buffer) = get_robotic_parameter(buffer);
            Command::Inc(
                param1.into(),
                param2.into(),
                None,
            )
        }
        CommandOp::Unused15 => {
            let (param1, buffer) = get_robotic_parameter(buffer);
            let (param2, _buffer) = get_robotic_parameter(buffer);
            Command::Dec(
                param1.into(),
                param2.into(),
                None,
            )
        }
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
        CommandOp::IfThingXY => six_args(buffer, Command::IfThingXY),
        CommandOp::IfAt => three_args(buffer, Command::IfAt),
        CommandOp::IfDirOfPlayer => five_args(buffer, Command::IfDirOfPlayer),
        CommandOp::Double => one_arg(buffer, Command::Double),
        CommandOp::Half => one_arg(buffer, Command::Half),
        CommandOp::Goto => one_arg(buffer, Command::Goto),
        CommandOp::Send => two_args(buffer, Command::Send),
        CommandOp::Explode => one_arg(buffer, Command::Explode),
        CommandOp::PutDir => four_args(buffer, Command::PutDir),
        CommandOp::Give => two_args(buffer, Command::Give),
        CommandOp::Take | CommandOp::TakeOr => {
            let (param1, buffer) = get_robotic_parameter(buffer);
            let (param2, buffer) = get_robotic_parameter(buffer);
            let param3 = if op == CommandOp::TakeOr {
                let (param, _buffer) = get_robotic_parameter(buffer);
                Some(param)
            } else {
                None
            };
            Command::Take(
                param1.into(),
                param2.into(),
                param3.map(|p| p.into()),
            )
        }
        CommandOp::EndGame => Command::EndGame,
        CommandOp::EndLife => Command::EndLife,
        CommandOp::Mod => one_arg(buffer, Command::Mod),
        CommandOp::Sam => two_args(buffer, Command::Sam),
        CommandOp::Volume => one_arg(buffer, Command::Volume),
        CommandOp::EndMod => Command::EndMod,
        CommandOp::EndSam => Command::EndSam,
        CommandOp::Play => one_arg(buffer, Command::Play),
        CommandOp::EndPlay => Command::EndPlay,
        CommandOp::WaitThenPlay => one_arg(buffer, Command::WaitThenPlay),
        CommandOp::WaitPlay => Command::WaitPlay,
        CommandOp::BlankLine => Command::BlankLine,
        CommandOp::Sfx => one_arg(buffer, Command::Sfx),
        CommandOp::PlayIfSilent => one_arg(buffer, Command::PlayIfSilent),
        CommandOp::Open => one_arg(buffer, Command::Open),
        CommandOp::LockSelf => Command::LockSelf,
        CommandOp::UnlockSelf => Command::UnlockSelf,
        CommandOp::SendDir => two_args(buffer, Command::SendDir),
        CommandOp::Zap => two_args(buffer, Command::Zap),
        CommandOp::Restore => two_args(buffer, Command::Restore),
        CommandOp::LockPlayer => Command::LockPlayer,
        CommandOp::UnlockPlayer => Command::UnlockPlayer,
        CommandOp::LockPlayerNS => Command::LockPlayerNS,
        CommandOp::LockPlayerEW => Command::LockPlayerEW,
        CommandOp::LockPlayerAttack => Command::LockPlayerAttack,
        CommandOp::MovePlayerDir | CommandOp::MovePlayerDirOr => {
            let (param1, buffer) = get_robotic_parameter(buffer);
            let param2 = if op == CommandOp::MovePlayerDirOr {
                let (param, _buffer) = get_robotic_parameter(buffer);
                Some(param)
            } else {
                None
            };
            Command::MovePlayerDir(
                param1.into(),
                param2.map(|p| p.into()),
            )
        }
        CommandOp::PutPlayerXY => one_arg(buffer, Command::PutPlayerXY),
        CommandOp::ObsoleteIfPlayerDir | CommandOp::ObsoleteIfNotPlayerDir => {
            let (param1, buffer) = get_robotic_parameter(buffer);
            let (param2, _buffer) = get_robotic_parameter(buffer);
            Command::ObsoleteIfPlayerDir(
                param1.into(),
                param2.into(),
                op == CommandOp::ObsoleteIfNotPlayerDir,
            )
        }
        CommandOp::IfPlayerXY => three_args(buffer, Command::IfPlayerXY),
        CommandOp::PutPlayerDir => one_arg(buffer, Command::PutPlayerDir),
        CommandOp::TryDir => two_args(buffer, Command::TryDir),
        CommandOp::RotateCW => Command::RotateCW,
        CommandOp::RotateCCW => Command::RotateCCW,
        CommandOp::Switch => two_args(buffer, Command::Switch),
        CommandOp::Shoot => one_arg(buffer, Command::Shoot),
        CommandOp::LayBomb | CommandOp::LayBombHigh => {
            let (param1, _buffer) = get_robotic_parameter(buffer);
            Command::LayBomb(
                param1.into(),
                op == CommandOp::LayBombHigh,
            )
        }
        CommandOp::ShootMissile => one_arg(buffer, Command::ShootMissile),
        CommandOp::ShootSeeker => one_arg(buffer, Command::ShootSeeker),
        CommandOp::SpitFire => one_arg(buffer, Command::SpitFire),
        CommandOp::LazerWall => two_args(buffer, Command::LazerWall),
        CommandOp::PutXY => five_args(buffer, Command::PutXY),
        CommandOp::DieItem => Command::DieItem,
        CommandOp::SendXY => three_args(buffer, Command::SendXY),
        CommandOp::CopyRobotNamed => one_arg(buffer, Command::CopyRobotNamed),
        CommandOp::CopyRobotXY => two_args(buffer, Command::CopyRobotXY),
        CommandOp::CopyRobotDir => one_arg(buffer, Command::CopyRobotDir),
        CommandOp::DuplicateSelfDir => one_arg(buffer, Command::DuplicateSelfDir),
        CommandOp::DuplicateSelfXY => two_args(buffer, Command::DuplicateSelfXY),
        CommandOp::BulletN | CommandOp::BulletS | CommandOp::BulletE | CommandOp::BulletW => {
            let (param1, _buffer) = get_robotic_parameter(buffer);
            Command::Bullet(
                param1.into(),
                match op {
                    CommandOp::BulletN => CardinalDirection::North,
                    CommandOp::BulletS => CardinalDirection::South,
                    CommandOp::BulletE => CardinalDirection::East,
                    CommandOp::BulletW => CardinalDirection::West,
                    _ => unreachable!(),
                },
            )
        }
        CommandOp::GiveKey | CommandOp::GiveKeyOr => {
            let (param1, buffer) = get_robotic_parameter(buffer);
            let param2 = if op == CommandOp::GiveKeyOr {
                let (param, _buffer) = get_robotic_parameter(buffer);
                Some(param)
            } else {
                None
            };
            Command::GiveKey(
                param1.into(),
                param2.map(|p| p.into()),
            )
        }
        CommandOp::TakeKey | CommandOp::TakeKeyOr => {
            let (param1, buffer) = get_robotic_parameter(buffer);
            let param2 = if op == CommandOp::TakeKeyOr {
                let (param, _buffer) = get_robotic_parameter(buffer);
                Some(param)
            } else {
                None
            };
            Command::TakeKey(
                param1.into(),
                param2.map(|p| p.into()),
            )
        }
        CommandOp::Trade => five_args(buffer, Command::Trade),
        CommandOp::SendDirPlayer => two_args(buffer, Command::SendDirPlayer),
        CommandOp::PutDirPlayer => four_args(buffer, Command::PutDirPlayer),
        CommandOp::Slash => one_arg(buffer, Command::Slash),
        CommandOp::MessageLine => one_arg(buffer, Command::MessageLine),
        CommandOp::MessageBoxLine => one_arg(buffer, Command::MessageBoxLine),
        CommandOp::MessageBoxOption => one_arg(buffer, Command::MessageBoxOption),
        CommandOp::MessageBoxMaybeOption => one_arg(buffer, Command::MessageBoxMaybeOption),
        CommandOp::Label => one_arg(buffer, Command::Label),
        CommandOp::Comment => one_arg(buffer, Command::Comment),
        CommandOp::ZappedLabel => one_arg(buffer, Command::ZappedLabel),
        CommandOp::Teleport => three_args(buffer, Command::Teleport),
        CommandOp::ScrollView => two_args(buffer, Command::ScrollView),
        CommandOp::Input => one_arg(buffer, Command::Input),
        CommandOp::IfInput | CommandOp::IfInputNot => {
            let (param1, buffer) = get_robotic_parameter(buffer);
            let (param2, _buffer) = get_robotic_parameter(buffer);
            Command::IfInput(
                param1.into(),
                param2.into(),
                op == CommandOp::IfInputNot,
            )
        }
        CommandOp::IfInputMatches => two_args(buffer, Command::IfInputMatches),
        CommandOp::PlayerChar => one_arg(buffer, Command::PlayerChar),
        CommandOp::MessageBoxColorLine => one_arg(buffer, Command::MessageBoxColorLine),
        CommandOp::MessageBoxCenterLine => one_arg(buffer, Command::MessageBoxCenterLine),
        CommandOp::MoveAll => three_args(buffer, Command::MoveAll),
        CommandOp::Copy => four_args(buffer, Command::Copy),
        CommandOp::SetEdgeColor => one_arg(buffer, Command::SetEdgeColor),
        CommandOp::Board | CommandOp::BoardIsNone => {
            let (param1, buffer) = get_robotic_parameter(buffer);
            let param2 = if op == CommandOp::Board {
                let (param, _buffer) = get_robotic_parameter(buffer);
                Some(param)
            } else {
                None
            };
            Command::Board(
                param1.into(),
                param2.map(|p| p.into()),
            )
        }
        CommandOp::CharEdit => {
            let (param1, mut buffer) = get_robotic_parameter(buffer);
            let mut bytes = [
                Byte::Literal(0),
                Byte::Literal(0),
                Byte::Literal(0),
                Byte::Literal(0),
                Byte::Literal(0),
                Byte::Literal(0),
                Byte::Literal(0),
                Byte::Literal(0),
                Byte::Literal(0),
                Byte::Literal(0),
                Byte::Literal(0),
                Byte::Literal(0),
                Byte::Literal(0),
                Byte::Literal(0),
            ];
            for byte in bytes.iter_mut() {
                let (param, new_buffer) = get_robotic_parameter(buffer);
                *byte = param.into();
                buffer = new_buffer;
            }
            Command::CharEdit(param1.into(), bytes)
        }
        CommandOp::BecomePushable | CommandOp::BecomeNonpushable =>
            Command::BecomePushable(op == CommandOp::BecomePushable),
        CommandOp::Blind => one_arg(buffer, Command::Blind),
        CommandOp::FireWalker => one_arg(buffer, Command::FireWalker),
        CommandOp::FreezeTime => one_arg(buffer, Command::FreezeTime),
        CommandOp::SlowTime => one_arg(buffer, Command::SlowTime),
        CommandOp::Wind => one_arg(buffer, Command::Wind),
        CommandOp::Avalanche => Command::Avalanche,
        CommandOp::CopyDir => two_args(buffer, Command::CopyDir),
        CommandOp::BecomeLavaWalker | CommandOp::BecomeNonLavaWalker =>
            Command::BecomeLavaWalker(op == CommandOp::BecomeLavaWalker),
        CommandOp::Change => six_args(buffer, Command::Change),
        CommandOp::PlayerColor => one_arg(buffer, Command::PlayerColor),
        CommandOp::BulletColor => one_arg(buffer, Command::BulletColor),
        CommandOp::MissileColor => one_arg(buffer, Command::MissileColor),
        CommandOp::MessageRow | CommandOp::Unused167 => one_arg(buffer, Command::MessageRow),
        CommandOp::RelSelf => Command::RelSelf(None),
        CommandOp::RelPlayer => Command::RelPlayer(None),
        CommandOp::RelCounters => Command::RelCounters(None),
        CommandOp::SetIdChar => two_args(buffer, Command::SetIdChar),
        CommandOp::JumpModOrder => one_arg(buffer, Command::JumpModOrder),
        CommandOp::Ask => one_arg(buffer, Command::Ask),
        CommandOp::FillHealth => Command::FillHealth,
        CommandOp::ThickArrow | CommandOp::ThinArrow => {
            let (param1, buffer) = get_robotic_parameter(buffer);
            let (param2, _buffer) = get_robotic_parameter(buffer);
            Command::ChangeArrowChar(
                param1.into(),
                param2.into(),
                op == CommandOp::ThickArrow,
            )
        }
        CommandOp::SetMaxHealth => one_arg(buffer, Command::SetMaxHealth),
        CommandOp::SavePlayerPosition | CommandOp::SavePlayerPositionN => {
            let param1 = if op == CommandOp::SavePlayerPositionN {
                let (param, _buffer) = get_robotic_parameter(buffer);
                param
            } else {
                Parameter::Word(0)
            };
            Command::SavePlayerPosition(param1.into())
        }
        CommandOp::RestorePlayerPosition | CommandOp::RestorePlayerPositionN => {
            let param1 = if op == CommandOp::RestorePlayerPositionN {
                let (param, _buffer) = get_robotic_parameter(buffer);
                param
            } else {
                Parameter::Word(0)
            };
            Command::RestorePlayerPosition(param1.into())
        }
        CommandOp::ExchangePlayerPosition | CommandOp::ExchangePlayerPositionN => {
            let param1 = if op == CommandOp::ExchangePlayerPositionN {
                let (param, _buffer) = get_robotic_parameter(buffer);
                param
            } else {
                Parameter::Word(0)
            };
            Command::ExchangePlayerPosition(param1.into())
        }
        CommandOp::MessageColumn | CommandOp::Unused166 => one_arg(buffer, Command::MessageColumn),
        CommandOp::CenterMessage => Command::CenterMessage,
        CommandOp::ClearMessage => Command::ClearMessage,
        CommandOp::ResetView => Command::ResetView,
        CommandOp::ModSam => two_args(buffer, Command::ModSam),
        CommandOp::Volume2 => one_arg(buffer, Command::Volume2),
        CommandOp::ScrollBase => one_arg(buffer, Command::ScrollBase),
        CommandOp::ScrollCorner => one_arg(buffer, Command::ScrollCorner),
        CommandOp::ScrollTitle => one_arg(buffer, Command::ScrollTitle),
        CommandOp::ScrollPointer => one_arg(buffer, Command::ScrollPointer),
        CommandOp::ScrollArrow => one_arg(buffer, Command::ScrollArrow),
        CommandOp::Viewport => two_args(buffer, Command::Viewport),
        CommandOp::ViewportSize => two_args(buffer, Command::ViewportSize),
        CommandOp::RestorePlayerPositionNDuplicateSelf =>
            one_arg(buffer, Command::RestorePlayerPositionDupSelf),
        CommandOp::ExchangePlayerPositionNDuplicateSelf =>
            one_arg(buffer, Command::ExchangePlayerPositionDupSelf),
        CommandOp::PlayerBulletN |
        CommandOp::PlayerBulletS |
        CommandOp::PlayerBulletW |
        CommandOp::PlayerBulletE => {
            let (param1, _buffer) = get_robotic_parameter(buffer);
            let dir = match op {
                CommandOp::PlayerBulletN => CardinalDirection::North,
                CommandOp::PlayerBulletS => CardinalDirection::South,
                CommandOp::PlayerBulletE => CardinalDirection::East,
                CommandOp::PlayerBulletW => CardinalDirection::South,
                _ => unreachable!(),
            };
            Command::PlayerBullet(param1.into(), dir)
        }
        CommandOp::NeutralBulletN |
        CommandOp::NeutralBulletS |
        CommandOp::NeutralBulletW |
        CommandOp::NeutralBulletE => {
            let (param1, _buffer) = get_robotic_parameter(buffer);
            let dir = match op {
                CommandOp::NeutralBulletN => CardinalDirection::North,
                CommandOp::NeutralBulletS => CardinalDirection::South,
                CommandOp::NeutralBulletE => CardinalDirection::East,
                CommandOp::NeutralBulletW => CardinalDirection::South,
                _ => unreachable!(),
            };
            Command::NeutralBullet(param1.into(), dir)
        }
        CommandOp::EnemyBulletN |
        CommandOp::EnemyBulletS |
        CommandOp::EnemyBulletW |
        CommandOp::EnemyBulletE => {
            let (param1, _buffer) = get_robotic_parameter(buffer);
            let dir = match op {
                CommandOp::EnemyBulletN => CardinalDirection::North,
                CommandOp::EnemyBulletS => CardinalDirection::South,
                CommandOp::EnemyBulletE => CardinalDirection::East,
                CommandOp::EnemyBulletW => CardinalDirection::South,
                _ => unreachable!(),
            };
            Command::EnemyBullet(param1.into(), dir)
        }
        CommandOp::PlayerBulletColor => one_arg(buffer, Command::PlayerBulletColor),
        CommandOp::NeutralBulletColor => one_arg(buffer, Command::NeutralBulletColor),
        CommandOp::EnemyBulletColor => one_arg(buffer, Command::EnemyBulletColor),
        CommandOp::Unused188 |
        CommandOp::Unused189 |
        CommandOp::Unused190 |
        CommandOp::Unused191 |
        CommandOp::Unused192 |
        CommandOp::Unused207 |
        CommandOp::Unused208 |
        CommandOp::Unused209 |
        CommandOp::Unused221 |
        CommandOp::Unused223 |
        CommandOp::Unused228 |
        CommandOp::Unused234 |
        CommandOp::Unused244 |
        CommandOp::Unused248 |
        CommandOp::Unused249 |
        CommandOp::Unused250 => return None,
        CommandOp::RelSelfFirst => Command::RelSelf(Some(RelativePart::First)),
        CommandOp::RelSelfLast => Command::RelSelf(Some(RelativePart::Last)),
        CommandOp::RelPlayerFirst => Command::RelPlayer(Some(RelativePart::First)),
        CommandOp::RelPlayerLast => Command::RelPlayer(Some(RelativePart::Last)),
        CommandOp::RelCountersFirst => Command::RelCounters(Some(RelativePart::First)),
        CommandOp::RelCountersLast => Command::RelCounters(Some(RelativePart::Last)),
        CommandOp::ModFadeOut => Command::ModFadeOut,
        CommandOp::ModFadeIn => Command::ModFadeIn,
        CommandOp::CopyBlock => six_args(buffer, Command::CopyBlock),
        CommandOp::ClipInput => Command::ClipInput,
        CommandOp::Push => one_arg(buffer, Command::Push),
        CommandOp::ScrollChar => two_args(buffer, Command::ScrollChar),
        CommandOp::FlipChar => two_args(buffer, Command::FlipChar),
        CommandOp::CopyChar => two_args(buffer, Command::CopyChar),
        CommandOp::ChangeSfx => two_args(buffer, Command::ChangeSfx),
        CommandOp::ColorIntensityAll |
        CommandOp::ColorIntensityN => {
            let (param1, buffer) = if op == CommandOp::ColorIntensityN {
                let (param, buffer) = get_robotic_parameter(buffer);
                (Some(param), buffer)
            } else {
                (None, buffer)
            };
            let (param2, _buffer) = get_robotic_parameter(buffer);
            Command::ColorIntensity(
                param1.map(|p| p.into()),
                param2.into(),
            )
        }
        CommandOp::ColorFadeOut => Command::ColorFadeOut,
        CommandOp::ColorFadeIn => Command::ColorFadeIn,
        CommandOp::SetColor => four_args(buffer, Command::SetColor),
        CommandOp::LoadCharSet => one_arg(buffer, Command::LoadCharSet),
        CommandOp::Multiply => two_args(buffer, Command::Multiply),
        CommandOp::Divide => two_args(buffer, Command::Divide),
        CommandOp::Modulo => two_args(buffer, Command::Modulo),
        CommandOp::PlayerCharDir => two_args(buffer, Command::PlayerCharDir),
        CommandOp::LoadPalette => one_arg(buffer, Command::LoadPalette),
        CommandOp::ModFadeTo => two_args(buffer, Command::ModFadeTo),
        CommandOp::ScrollViewXY => two_args(buffer, Command::ScrollViewXY),
        CommandOp::SwapWorld => one_arg(buffer, Command::SwapWorld),
        CommandOp::IfAlignedRobot => two_args(buffer, Command::IfAlignedRobot),
        CommandOp::LockScroll => Command::LockScroll,
        CommandOp::UnlockScroll => Command::UnlockScroll,
        CommandOp::IfFirstInput => two_args(buffer, Command::IfFirstInput),
        CommandOp::PersistentGo => one_arg(buffer, Command::PersistentGo),
        CommandOp::WaitModFade => Command::WaitModFade,
        CommandOp::EnableSaving => Command::EnableSaving,
        CommandOp::DisableSaving => Command::DisableSaving,
        CommandOp::EnableSensorOnlySaving => Command::EnableSensorOnlySaving,
        CommandOp::StatusCounter => two_args(buffer, Command::StatusCounter),
        CommandOp::OverlayOn => Command::OverlayMode(OverlayMode::Normal),
        CommandOp::OverlayStatic => Command::OverlayMode(OverlayMode::Static),
        CommandOp::OverlayTransparent => Command::OverlayMode(OverlayMode::Transparent),
        CommandOp::PutOverlay => four_args(buffer, Command::PutOverlay),
        CommandOp::CopyOverlayBlock => six_args(buffer, Command::CopyOverlayBlock),
        CommandOp::ChangeOverlay | CommandOp::ChangeOverlayColor => {
            let (param1, buffer) = get_robotic_parameter(buffer);
            let (mut param2, buffer) = get_robotic_parameter(buffer);
            let opt_params = if op == CommandOp::ChangeOverlay {
                let (mut param_a, buffer) = get_robotic_parameter(buffer);
                let (param_b, _buffer) = get_robotic_parameter(buffer);
                mem::swap(&mut param_a, &mut param2);
                Some((param_a, param_b))
            } else {
                None
            };
            Command::ChangeOverlay(
                param1.into(),
                param2.into(),
                opt_params.map(|(p3, p4)| (p3.into(), p4.into())),
            )
        }
        CommandOp::WriteOverlay => four_args(buffer, Command::WriteOverlay),
        CommandOp::LoopStart => Command::LoopStart,
        CommandOp::LoopFor => one_arg(buffer, Command::LoopFor),
        CommandOp::AbortLoop => Command::AbortLoop,
        CommandOp::DisableMesgEdge | CommandOp::EnableMesgEdge =>
            Command::MesgEdge(op == CommandOp::EnableMesgEdge),
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
        let cmd = CommandOp::from_u8(op).expect("unknown opcode");
        if let Some(cmd) = parse_opcode(&new_buffer[0..total-1], cmd) {
            commands.push(cmd)
        }
        let (total_dup, new_buffer) = get_byte(&new_buffer[total-1..]);
        assert_eq!(total, total_dup as usize);
        buffer = new_buffer;
    }
    commands
}
