#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct DataNode {
  pub kind: DataNodeKind,
  pub token: u32,
}

pub union Node {
  data: DataNode,
  num: u32,
}

#[derive(Debug, PartialEq, Eq)]
pub enum ExpandedNode {
  Data(DataNode),
  Num(u32),
}

// NB: size will grow to 4 when you add a 16 bitstruct, that's OK
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum DataNodeKind {
  VarDeclStmt { has_type: bool },
  AsciiLit,
  IntLit,
  ExprStmt,
  CallExpr { num_args: u8 },
  MemberAccess { implicit: bool },
  PlatformKeyword,
  Ident,
  ReturnStmt,
  ImplicitMemberAccess,
}

// #![allow(non_camel_case_types)]
// use bilge::prelude::*;

// #[bitsize(16)]
// #[derive(FromBits, DebugBits, Clone, Copy, Eq, PartialEq)]
// pub struct VarDecl_D {
//   pub has_type: bool,
//   reserved: u15,
// }
