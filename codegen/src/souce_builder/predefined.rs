/// Headers stricitly neccessary for the transalation.
pub const MANDATORY_HEADERS: &[&str] = &["stdint.h", "stdexcept", "span"];
/// Builtin, layout-compatible C++ repr of a Rust array
pub const RUST_ARR: &str = "#ifndef RUST_ARR_DEFINED
template<typename T, uintptr_t SIZE> class RustArr{
    T arr[SIZE];
    T& operator[](uintptr_t index){
        if(index > SIZE) throw std::out_of_range(\"Index out of range in Rust array\");
        return arr[index];
    }
};
#define RUST_ARR_DEFINED 1
#endif\n";
/// Builtin, layout-compatible C++ repr of a Rust *[T].
pub const RUST_SLICE: &str = "#ifndef RUST_SLICE
template<typename T> class RustSlice{
    T* ptr;
    uintptr_t len;
    T& operator[](uintptr_t idx){
        if (idx > len)throw std::out_of_range(\"Index out of range in Rust slice\");
        return (ptr+idx);
    }
};
#define RUST_SLICE 1
#endif\n";
/// Builtin, layout-compatible C++ repr Rust *dyn Trait
pub const RUST_DYN: &str = "#ifndef RUST_DYN
class RustDyn{
    void* ptr;
    void* vtable;
};
#define RUST_DYN 1
#endif\n";
/// Builtin, layout-compatible C++ repr of Rust *str
pub const RUST_STR: &str = "#ifndef RUST_STR
template<bool> struct RustStr{
    char32_t* utrf8_data;
    uintptr_t len;
};
#define RUST_STR 1
#endif
#ifndef RUST_FAT_PTR
template<typename T> struct RustFatPtr{
    T* data;
    void* metadata;
};
#define RUST_FAT_PTR 1
#endif\n";
/// Rust-fn typdef
pub const RUST_FN_DEF: &str = "
#ifndef RUST_FN_DEF
template<uint64_t id> struct RustFnDef{};
#define RUST_FN_DEF 1
#endif\n";
/// Rust isize type-wrapper
pub const ISIZE: &str =
    "#ifndef _RUST_ISIZE\nstruct isize{intptr_t i;};\n#define _RUST_ISIZE 1\n#endif\n";
/// Rust usize type-wrapper
pub const USIZE: &str =
    "#ifndef _RUST_USIZE\nstruct usize{uintptr_t i;};\n#define _RUST_USIZE 1\n#endif\n";
/// Rust char wrapper
pub const CHAR: &str =
    "#ifndef _RUST_CHAR\nstruct RustChar{uint32_t i;};\n#define _RUST_CHAR 1\n#endif\n";
/// Mandatory C tpe definitions
pub const C_TYPEDEFS: &[&str] = &[
    RUST_ARR,
    RUST_SLICE,
    RUST_DYN,
    RUST_STR,
    RUST_FN_DEF,
    ISIZE,
    USIZE,
    CHAR,
    "struct RustFn;\n",
    "#define RUST_IMMUTABLE false\n",
    "#define RUST_MUTABLE true\n",
    "template<typename... Types> struct RustTuple;\n",
    "#undef unix\n",
    "#define restrict __restrict\n",
];
