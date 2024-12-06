# seabridge(*siː brɪʤ*) - next-gen tool for easy Rust / C++ interop

**WARNING: This tool is in early research phase. It paritaly works, but it has some *very* rough edges and is nothing more than a tech demo ATM.**

**Since it is so early in developement, this README may not be up to date. While I try to be clear about what is implemented and planned,**
**it is important to note that many features are still quite buggy.**

Seabridge is an experimental tool, which uses rich type information from the
rust compiler to generate high-quality C++ bindings to Rust code.

Thanks to this unqiue, deep access to the internal state of the compiler,
sebaridge can generate C++ bindings to all Rust types(DONE), functions(WIP, no demangling and shims yet), and statics(WIP, no demangling yet). You
don't need to write any glue code: seabridge can genearate bindings to almost all 
crates out of the box(currently works with most of `std`, `core`, and `alloc`).

## Solving ABI instability

### Solving the quiestion of unstable Layout:

Since sebridge uses the same layout calcualtions as the Rust compiler, it can gurantee that the bindings to Rust types it generates are fully accurate. 

Doing things this way eliminates all the layout issues for in case of static linking: who cares if the layout of a Rust type changes, as long as the layout of its C++ counterpart also changes to match it again?

Sebridge can *guarantee* the memory layout of C++ bindings it generates matches whatever the used version of the Rust compiler.

### Keeping up with the everchanging ABI:

TODO: write about translating Rust ABI to C(implemented, but limited/flawed(*unhandled egde cases*) approach), and about generating C-to-Rust shims(WIP, should be rock-solid in theory).

# High level translation

The ultimate goal of Seabridge is allowing you to forget that the code you are using was originaly written in Rust.

Seabrdige translates Rust generics into C++ templates(with some limitations), allowing you to use generic Rust types form C++:
```rust
#[no_mangle]
fn my_fn(args:(i32,f64,&[u8]))->Box<i32>{
	eprintln!("Recived args:{args:?}");
	Box::new(args.0)
}
```
```cpp
#include <mycrate/includes/mycrate.hpp>
int main() {
	uint8_t* slice_content = (uint8_t*)"Hi Bob";
	// Create Rust slice
	RustSlice<uint8_t> slice;
	slice.ptr = slice_content;
	slice.len = 6;
	// Create Rust tuple
	RustTuple<int32_t,double,RustSlice> args = {8,3.14159,slice};
	// Just call a Rust function
	alloc::boxed::Box<int32_t> rust_box = my_crate::my_fn(args);
	// Box<int32_t> will get dropped, automaticaly
}
```
Seabridge will also translate all `Drop` impls into C++ destructors.

# LICENSE

Parts of Seabrdige's code come from `rustc_codegen_clr`, and, as such, it is licensed under the same license as it and the Rust compiler: MIT or Apache. 