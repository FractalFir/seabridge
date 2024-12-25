pub fn pass<T>(val: T) -> T {
    val
}
#[no_mangle]
pub fn recive_ptr(idx: u8) -> *mut () {
    match idx {
        0 => {
            let tmp: fn(u8) -> u8 = pass;
            tmp as *mut ()
        }
        1 => {
            let tmp: fn(u16) -> u16 = pass;
            tmp as *mut ()
        }
        2 => {
            let tmp: fn(u32) -> u32 = pass;
            tmp as *mut ()
        }
        _ => std::ptr::null_mut::<()>(),
    }
}
