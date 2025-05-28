use crate::src_loc::SrcLoc;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum TokenKind {
  Arrow,
  Let,
  Rt,
  Ident,
  IntLit,
  AsciiLit,
  Eq,
  Fn,
  Semicolon,
  Pf,
  Dot,
  LParen,
  RParen,
  LBrace,
  RBrace,
  Eof,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Token {
  pub kind: TokenKind,
  pub loc: SrcLoc,
}

impl Token {
  pub fn new(kind: TokenKind, loc: SrcLoc) -> Self {
    Token { kind, loc }
  }
}
