// //use std::ops::Add;

// use std::collections::HashMap;

// use petgraph::{
//     algo::{self}, graph::{DiGraph, NodeIndex}, prelude::{DiGraphMap, GraphMap}, visit::DfsPostOrder, Directed
// };

// use crate::root::resource::ast::{Ast, Expr, Module, Program, SymbolType};

#[derive(Clone, Debug, Default, Hash, Eq, PartialEq, Copy, PartialOrd, Ord)]
pub struct TypeVar(pub usize);

#[derive(Clone, Copy, Debug, Hash, Eq, PartialEq, PartialOrd, Ord)]
pub enum PartialType {
    Root,
    Module,
    Instance,
    TypeDef,
    StandardScope,
    Variable(TypeVar),
    Record,
    Field(TypeVar, &'static Self),
    Enum,
    Variant,
    Int,
    Uint,
    Byte,
    Flt,
    Bool,
    Char,
    Str,
    Naught,
    Fn(&'static [Self], &'static Self),
    Mut(&'static Self),
    Pointer(&'static Self),
    Custom(usize),
}
