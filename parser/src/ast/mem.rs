use crate::internal::*;

#[derive(Clone, Copy)]
pub union Element {
  ast: TokenNode,
  int: u64,
  idx: idx::AstNode,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct TokenNode {
  pub token: u32,
  pub node: Mem,
}

impl TokenNode {
  pub const fn new(node: Mem, token: u32) -> Self {
    TokenNode { node, token }
  }
}

impl Element {
  pub const fn ast(node: Mem, token: u32) -> Self {
    Element { ast: TokenNode::new(node, token) }
  }
  pub const fn int(int: u64) -> Self {
    Element { int }
  }
  pub const fn idx(idx: idx::AstNode) -> Self {
    Element { idx }
  }
  pub const fn as_ast(self) -> TokenNode {
    unsafe { self.ast }
  }
  pub const fn as_int(self) -> u64 {
    unsafe { self.int }
  }
  pub const fn as_next(self) -> idx::AstNode {
    unsafe { self.idx }
  }
}

impl std::fmt::Debug for Element {
  fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    write!(f, "Element(<union>)")
  }
}

impl From<TokenNode> for Element {
  fn from(ast: TokenNode) -> Self {
    Element { ast }
  }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Mem {
  VarDeclStmt { has_type: bool },
  AsciiLit,
  IntLit(IntData),
  FnDecl(FnDeclData),
  ExprStmt,
  Block { num_stmts: u16 },
  CallExpr { num_args: u8 },
  MemberAccess { implicit: bool },
  PlatformKeyword,
  Ident,
  ReturnStmt,
  Fixup,
}

impl Mem {
  pub const fn is_stmt(&self) -> bool {
    match self {
      Mem::VarDeclStmt { .. } => true,
      Mem::Block { .. } => true,
      Mem::ExprStmt => true,
      Mem::ReturnStmt => true,
      Mem::AsciiLit => false,
      Mem::IntLit(_) => false,
      Mem::FnDecl(_) => false,
      Mem::CallExpr { .. } => false,
      Mem::MemberAccess { .. } => false,
      Mem::PlatformKeyword => false,
      Mem::Ident => false,
      Mem::Fixup => false,
    }
  }
  pub const fn is_decl(&self) -> bool {
    match self {
      Mem::FnDecl(_) => true,
      Mem::VarDeclStmt { .. } => false,
      Mem::ExprStmt => false,
      Mem::ReturnStmt => false,
      Mem::Block { .. } => false,
      Mem::AsciiLit => false,
      Mem::IntLit(_) => false,
      Mem::CallExpr { .. } => false,
      Mem::MemberAccess { .. } => false,
      Mem::PlatformKeyword => false,
      Mem::Ident => false,
      Mem::Fixup => false,
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
  assert!(std::mem::size_of::<TokenNode>() <= 8);
  assert!(std::mem::size_of::<Element>() <= 8);
}
