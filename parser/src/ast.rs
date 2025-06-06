#[derive(Debug, PartialEq, Eq)]
pub enum Decl {
  Function {
    num_args: u8,
    token: u32,
    is_pure: bool,
    discardable: bool,
  },
}

#[derive(Debug, PartialEq, Eq)]
pub enum Stmt {
  Let { token: u32, has_type: bool },
  Expression { token: u32 },
  Return { token: u32 },
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum Expr {
  CallExpr { token: u32, num_args: u8 },
  MemberAccess { token: u32, implicit: bool },
  Ident { token: u32 },
  AsciiLit { token: u32 },
  IntLit { token: u32, value: u64 },
  PlatformKeyword { token: u32 },
  Type { token: u32, num_tokens: u16 },
}

#[derive(Debug, PartialEq, Eq)]
pub enum AstNode {
  Declaration(Decl),
  Expression(Expr),
  Statement(Stmt),
}
