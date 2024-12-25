#![feature(allocator_api)]
#![allow(dead_code,non_camel_case_types)]
use std::ptr::NonNull;
#[repr(C)]
pub struct aiocb {
    __next_prio: *mut aiocb,
}
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
#[no_mangle]
#[inline(never)]
pub fn rcv_aiocb(aiocb: &aiocb) -> &aiocb {
    aiocb
}
