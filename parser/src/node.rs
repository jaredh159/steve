#![allow(non_camel_case_types)]
use bilge::prelude::*;

#[derive(Debug, PartialEq, Eq)]
pub struct DataNode {
  pub kind: DataNodeKind,
  pub token: u32,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum DataNodeKind {
  VarDeclStmt(VarDecl_D),
  AsciiLit,
  IntLit,
  ExprStmt,
  CallExpr(u8),
  FieldAccess,
  PlatformKeyword,
  Ident,
  ReturnStmt,
  ImplicitMemberAccess,
}

#[bitsize(16)]
#[derive(FromBits, DebugBits, Clone, Copy, Eq, PartialEq)]
pub struct VarDecl_D {
  pub has_type: bool,
  reserved: u15,
}

// #[bitsize(16)]
// #[derive(FromBits, DebugBits, Clone, Copy, Eq, PartialEq)]
// pub struct CallExpr_D {
//   pub num_args: u8,
//   reserved: u8,
// }
