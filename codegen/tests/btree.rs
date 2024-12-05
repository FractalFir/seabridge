#![feature(allocator_api)]
use std::alloc::Allocator;
use std::alloc::Global;
use std::ffi::OsString;
use std::marker::PhantomData;
use std::mem::ManuallyDrop;
use std::mem::MaybeUninit;
use std::ptr::NonNull;

pub struct NodeRef {
    node: NonNull<LeafNode>,
}
struct LeafNode {
    parent: Option<NonNull<LeafNode>>,
}

type Map = NodeRef;
#[no_mangle]
#[inline(never)]
pub fn rcv_btree(val: Map) -> Map {
    val
}
