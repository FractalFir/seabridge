#![feature(
    lang_items,
    adt_const_params,
    associated_type_defaults,
    core_intrinsics,
    unsized_const_params
)]
#![allow(
    internal_features,
    incomplete_features,
    unused_variables,
    dead_code,
    unused_imports
)]

static mut INT32: i32 = 0;
macro_rules! test_numeric_static {
    ($tpe:ty) => {{
        static TMP: $tpe = 64 as $tpe;
        std::hint::black_box(&TMP);
    }};
}
static NANY: f32 = f32::NAN;
static NANY2: f64 = f64::NAN;
#[no_mangle]
pub fn test_main() {
    let zero = unsafe { INT32 };
    assert_eq!(zero, 0);
    unsafe { INT32 += 1 };
    let one = unsafe { INT32 };
    assert_eq!(one, 1);
    unsafe { INT32 += 1 };
    let two = unsafe { INT32 };
    assert_eq!(two, 2);
    unsafe { INT32 *= two };
    let four = unsafe { INT32 };
    assert_eq!(four, 4);
    test_numeric_static!(i8);
    test_numeric_static!(i16);
    test_numeric_static!(i32);
    test_numeric_static!(i64);
    test_numeric_static!(i128);
    test_numeric_static!(u8);
    test_numeric_static!(u16);
    test_numeric_static!(u32);
    test_numeric_static!(u64);
    test_numeric_static!(u128);
    test_numeric_static!(f32);
    test_numeric_static!(f64);
    std::hint::black_box(&NANY);
    std::hint::black_box(&NANY2);
}
