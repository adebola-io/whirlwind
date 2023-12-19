#[derive(Clone, Copy)]
pub struct Register(pub f64);

#[derive(Default)]
pub enum AccValue {
    #[default]
    None,
    StackPointer(usize),
    ConstIndex(usize),
    Numeric(f64),
    Bool(bool),
}

#[derive(Default, Debug)]
pub struct RegisterList {
    pub acc8: i8,
    pub acc16: i16,
    pub acc32: f32,
    pub acc64: f64,

    pub boolx: bool,

    pub r8: i8,
    pub r16: i16,
    pub r32: f32,
    pub r64: f64,

    pub stackptr: usize,
    pub constptr: usize,

    pub addressptr: u64,
    pub loopcounter: usize,
}

impl RegisterList {
    pub fn new() -> Self {
        Default::default()
    }
}
