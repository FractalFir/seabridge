# Project Sea Bridge(*siː brɪʤ*) - easy Rust / C++ interop

Seabridge is an experimental tool, which uses rich type information from the
rust compiler to generate high-quality C++ bidnings to Rust code.

Thanks to this unqiue, deep access to the internal state of the compiler,
sebaridge can generate C++ bindings to all Rust types and functions. You
don't need to write any glue code: seabridge can genearate bindings to all 
crates out of the box.

```rust
// In crate `vec`
#[derive(Clone,Copy,Default)]
struct Vec3(pub f32,pub f32,pub f32);
pub fn add(lhs:Vec3, rhs:Vec3)->Vec3{
	Vec3(lhs.x + rhs.x, lhs.y + rhs.y, lhs.x + rhs.y)
}
``` 

```cpp
#include <vec>
int main(){
   	// The default trait causes `sebaridge` to generate the defualt constructor.
	vec::Vec3 zero = Vec3();
	// Since all fields of Vec are accessible in Rust, we can change them in C++ too.
	vec::Vec3 one;
	one.f0 = 1.0;
	one.f1 = 1.0;
	one.f2 = 1.0;
	// Plain old call
	vec::Vec3 sum = vec::add(zero,one);
}
```

Seabridge is also able to map drop implementations into C++ destuctors, and adds copy constructors to all Rust types
which implement `Copy`, ensuring Rust types can be used in convienent and safe ways.

Generics are also partialy handled by seabridge, altough there are some limitations. Due to technical limitations, 
seabridge will only provide function implemenations and type definitions for types monomoprhized(used) by Rust code. 
In practice, this means that all genercis used by Rust will be exported, and ones not used will not.
```rust
// In crate `vec`
#[derive(Clone,Copy,Default)]
struct Vec3<T>(pub T,pub T,pub T);
pub fn add<T:Add>(lhs:Vec3<T>, rhs:Vec3<T>)->Vec3<T>{
        Vec3(lhs.x + rhs.x, lhs.y + rhs.y, lhs.x + rhs.y)
}
pub fn add_vecf32(lhs:Vec<f32>,rhs:Vec<f32>)->Vec<f32>{
	add(lhs,rhs)
}
```
```cpp
// OK to use: since Rust used this type, sebridge was able to expose it to C++.
vec::Vec3<float> val;
// NOT OK! vec::Vec3<int32_t> was never used by Rust, so this generic does not exist. 
```

This can be worked around by explicitly listing out generics used by C++. 

```rust
seabridge_require!(Vec3<i32>);
seabridve_require!(add<f32>);
```
