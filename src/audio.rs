pub trait AudioEngine {
    fn load_module(&self, file: &str);
    fn mod_fade_in(&self, file: &str);
    fn mod_fade_out(&self);
    fn end_module(&self);
    fn set_mod_order(&self, order: i32);
}
