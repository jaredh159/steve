#![allow(non_camel_case_types)]
use bilge::prelude::*;

#[derive(Debug)]
pub struct Module {
  pub kind: ModuleKind,
  pub platform: PlatformSpec,
  pub alloc: AllocModel,
  pub std: bool,
}

#[derive(Debug)]
pub struct PlatformSpec {
  // Fields for platform specification
}

#[derive(Debug, PartialEq, Eq)]
pub enum ModuleKind {
  Bin(Vec<Node>),
  Lib, // export map of some sort
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum AllocModel {
  Implicit,
  Explicit,
}

#[derive(Debug, PartialEq, Eq)]
pub struct Node {
  pub kind: NodeKind,
  pub token: u32,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum NodeKind {
  VarDeclStmt(N_VarDecl),
  AsciiLit,
  ExprStmt,
  CallExpr,
  FieldAccess,
  PlatformKeyword,
}

// https://astexplorer.net/#/gist/c4b0ef53aabb43ac2da1a11b02161e5c/7de0332ff8422c486b3be119d86ce980548767ed

#[bitsize(16)]
#[derive(FromBits, DebugBits, Clone, Copy, Eq, PartialEq)]
pub struct N_VarDecl {
  pub has_type: bool,
  reserved: u15,
}

pub struct N_CallExpr {}
