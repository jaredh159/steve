use crate::internal::*;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct DataNode {
  pub kind: DataNodeKind,
  pub token: u32,
}

impl DataNode {
  pub const fn new(kind: DataNodeKind, token: u32) -> Self {
    DataNode { kind, token }
  }
}

#[derive(Clone, Copy)]
pub union Node {
  ast: DataNode,
  int: u64,
}

impl Node {
  pub const fn ast(kind: DataNodeKind, token: u32) -> Self {
    Node { ast: DataNode::new(kind, token) }
  }
  pub const fn int(int: u64) -> Self {
    Node { int }
  }
  pub const fn as_ast(self) -> DataNode {
    unsafe { self.ast }
  }
  pub const fn as_int(self) -> u64 {
    unsafe { self.int }
  }
}

impl std::fmt::Debug for Node {
  fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    write!(f, "Node(<union>)")
  }
}

impl From<DataNode> for Node {
  fn from(ast: DataNode) -> Self {
    Node { ast }
  }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum DataNodeKind {
  VarDeclStmt { has_type: bool },
  AsciiLit,
  Type { num_tokens: u16 },
  IntLit(IntData),
  FnDecl(FnDeclData),
  ExprStmt,
  CallExpr { num_args: u8 },
  MemberAccess { implicit: bool },
  PlatformKeyword,
  Ident,
  ReturnStmt,
  ImplicitMemberAccess,
}

impl DataNodeKind {
  pub const fn is_stmt(&self) -> bool {
    match self {
      DataNodeKind::VarDeclStmt { .. } => true,
      DataNodeKind::ExprStmt => true,
      DataNodeKind::ReturnStmt => true,
      DataNodeKind::AsciiLit => false,
      DataNodeKind::Type { .. } => false,
      DataNodeKind::IntLit(_) => false,
      DataNodeKind::FnDecl(_) => false,
      DataNodeKind::CallExpr { .. } => false,
      DataNodeKind::MemberAccess { .. } => false,
      DataNodeKind::PlatformKeyword => false,
      DataNodeKind::Ident => false,
      DataNodeKind::ImplicitMemberAccess => false,
    }
  }
  pub const fn is_decl(&self) -> bool {
    match self {
      DataNodeKind::FnDecl(_) => true,
      DataNodeKind::VarDeclStmt { .. } => false,
      DataNodeKind::ExprStmt => false,
      DataNodeKind::ReturnStmt => false,
      DataNodeKind::AsciiLit => false,
      DataNodeKind::Type { .. } => false,
      DataNodeKind::IntLit(_) => false,
      DataNodeKind::CallExpr { .. } => false,
      DataNodeKind::MemberAccess { .. } => false,
      DataNodeKind::PlatformKeyword => false,
      DataNodeKind::Ident => false,
      DataNodeKind::ImplicitMemberAccess => false,
    }
  }
}

#[bitsize(16)]
#[derive(FromBits, DebugBits, Clone, Copy, Eq, PartialEq)]
pub struct IntData {
  pub len: u2,
  pub payload: u14,
}

#[bitsize(16)]
#[derive(FromBits, DebugBits, Clone, Copy, Eq, PartialEq)]
pub struct FnDeclData {
  pub num_args: u8,
  pub is_pure: bool,
  pub discardable: bool,
  pub reserved: u6, // TODO: access
}

#[test]
fn sizes() {
  assert!(std::mem::size_of::<DataNode>() <= 8);
  assert!(std::mem::size_of::<Node>() <= 8);
}
