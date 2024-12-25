#[no_mangle]
pub fn add(lhs: i128, rhs: i128) -> i128 {
    lhs + rhs
}
#[no_mangle]
pub fn sum(a: f32, b: f32, c: f32, d: f32) -> f32 {
    a + b + c + d
}
#[no_mangle]
pub fn deref_ptr(val: &mut u64) -> u64 {
    *val
}

pub fn str_get(_v_al: &str) -> char {
    'u'
}
#[no_mangle]
pub fn slice_get(val: &[u8], idx: usize) -> u8 {
    val[idx]
}
pub trait Odd {
    fn be_funny(&self);
}
#[no_mangle]
pub fn pass_dyn(ptr: *const dyn Odd) -> *const dyn Odd {
    ptr
}
