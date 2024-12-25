#![allow(dead_code)]
use std::collections::btree_map;
use std::sync::Arc;
#[no_mangle]
pub fn abrev(ab: &mut AbbreviationsCache) {
    ab.abbreviations.insert(0, Err(()));
}
#[derive(Debug, Default)]
pub struct AbbreviationsCache {
    abbreviations: btree_map::BTreeMap<u64, Result<Arc<Abbreviations>, ()>>,
}
#[derive(Debug, Default, Clone)]
pub struct Abbreviations {
    vec: Vec<Abbreviation>,
    map: btree_map::BTreeMap<u64, Abbreviation>,
}
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Abbreviation {
    code: u64,
    tag: DwTag,
    has_children: DwChildren,
    attributes: Attributes,
}
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct DwTag(pub u16);
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct DwChildren(pub u8);
#[derive(Debug, Clone, PartialEq, Eq)]
pub(crate) enum Attributes {
    Inline {
        buf: [AttributeSpecification; MAX_ATTRIBUTES_INLINE],
        len: usize,
    },
    Heap(Vec<AttributeSpecification>),
}
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct AttributeSpecification {
    //name: constants::DwAt,
    //form: constants::DwForm,
    implicit_const_value: i64,
}
const MAX_ATTRIBUTES_INLINE: usize = 5;
