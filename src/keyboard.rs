pub enum Key {
    A,
    B,
    C,
    D,
    E,
    F,
    G,
    H,
    I,
    J,
    K,
    L,
    M,
    N,
    O,
    P,
    Q,
    R,
    S,
    T,
    U,
    V,
    W,
    X,
    Y,
    Z,
    Space,
    Return,
    Left,
    Right,
    Up,
    Down,
    Num1,
    Num2,
    Num3,
    Num4,
    Num5,
    Num6,
    Num7,
    Num8,
    Num9,
    Num0,
    LCtrl,
    RCtrl,
}

impl Key {
    pub(crate) fn key_pressed(&self) -> i32 {
        match self {
            Key::A => 97,
            Key::B => 98,
            Key::C => 99,
            Key::D => 100,
            Key::E => 101,
            Key::F => 102,
            Key::G => 103,
            Key::H => 104,
            Key::I => 105,
            Key::J => 106,
            Key::K => 107,
            Key::L => 108,
            Key::M => 109,
            Key::N => 110,
            Key::O => 111,
            Key::P => 112,
            Key::Q => 113,
            Key::R => 114,
            Key::S => 115,
            Key::T => 116,
            Key::U => 117,
            Key::V => 118,
            Key::W => 119,
            Key::X => 120,
            Key::Y => 121,
            Key::Z => 122,
            Key::Space => 32,
            Key::Return => 13,
            Key::Left => 276,
            Key::Right => 275,
            Key::Up => 273,
            Key::Down => 274,
            Key::Num1 => 49,
            Key::Num2 => 50,
            Key::Num3 => 51,
            Key::Num4 => 52,
            Key::Num5 => 53,
            Key::Num6 => 54,
            Key::Num7 => 55,
            Key::Num8 => 56,
            Key::Num9 => 57,
            Key::Num0 => 48,
            Key::LCtrl => 306,
            Key::RCtrl => 305,
        }
    }
}