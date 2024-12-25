#include "core.hpp"
#include "alloc.hpp"
#include <iostream>
#include <cstring>
int main(){
	alloc::ffi::c_str::CString cstring = alloc::ffi::c_str::cstring::from_raw((int8_t*)"Hello :)");
	// Assemble a Rust `&str` in C++
	char32_t* utf8_location = (char32_t*)__FILE__;
	uintptr_t location_length = strlen((char*)utf8_location);
	RustStr<RUST_IMMUTABLE> file = {utf8_location,location_length};
	// Call a function from core to create a panic location - infromation about where a panic is comming from. 
	core::panic::location::Location loc = core::panic::location::location::internal_constructor(file,__LINE__,0);
	// Create the panic message, to print something nice to stdout!
	char32_t* utf8_data = (char32_t*)"This panic message is comming straight from C++ land :). I am panicking to print cause `std` bindings don't work yet.";
	uintptr_t message_length = strlen((char*)utf8_data);
	RustStr<RUST_IMMUTABLE> message = {utf8_data,message_length};
	// Once again, just call a function from core like it is a "normal" C++ function.
	// This will abort, but this is the only way to print something from `core`(`std` bindings don't work yet)
	core::panicking::panic(message,&loc);
}

//	