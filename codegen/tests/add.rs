#[inline(never)]
pub fn add(lhs: i128, rhs: i128) -> i128 {
    lhs + rhs
}
#[inline(never)]
pub fn copy(val: &mut [i32; 8]) -> [i32; 8] {
    *val
}
#[derive(Clone, Copy)]
pub struct Vec3(pub f32, pub f32, pub f32);
#[no_mangle]
pub fn recive_vec3(val: Vec3) -> Vec3 {
    val
}
#[inline(never)]
#[no_mangle]
pub fn recive_box(val: Box<i32>) {}
#[inline(never)]
pub fn recive_vec3_ref(val: &Vec3) {
    recive_vec3(*val);
}
/*
#[inline(never)]
#[no_mangle]
pub  fn pass_btree(
    val: std::collections::BTreeMap<std::ffi::OsString, Option<std::ffi::OsString>>,
) -> std::collections::BTreeMap<std::ffi::OsString, Option<std::ffi::OsString>> {
    val
}*/

#[no_mangle]
pub fn non_null_ptr(
    ptr: std::mem::ManuallyDrop<std::ptr::NonNull<Vec3>>,
) -> std::mem::ManuallyDrop<std::ptr::NonNull<Vec3>> {
    ptr
}
/*
macro_rules! require_type {
    ($uid:ident,$ty:ty) => {
        mod $uid {
            #[used]
            static DROP: std::sync::LazyLock<()> = std::sync::LazyLock::new(|| {
                let _ = |tmp: $ty| drop(tmp);
            });
        }
    };
}
macro_rules! require_fn {
    ($uid:ident,$fn:expr) => {
        mod $uid {
            use super::*;
            #[used]
            static KEEP_FN: std::sync::LazyLock<()> = std::sync::LazyLock::new(|| {
                std::hint::black_box($fn);
            });
        }
    };
}

require_type!(BOXI32, Box<i32>);
require_fn!(RBOXI32, recive_box);
 */
