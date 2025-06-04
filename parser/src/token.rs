use crate::str_pool::*;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum TokenKind {
  Arrow,
  Let,
  Comma,
  Routine,
  Ident,
  IntLit,
  InvalidUtf8,
  AsciiLit,
  Assign,
  Function,
  Semicolon,
  Pf,
  Dot,
  LParen,
  RParen,
  LBrace,
  RBrace,
  Eof,
}

impl Token {
  pub const fn new(kind: TokenKind, offset: u32, index: Index) -> Self {
    Token { kind, offset, index }
  }

  pub fn lexeme<'a>(&self, strings: &'a StringPool) -> &'a str {
    match self.kind {
      TokenKind::Arrow => "->",
      TokenKind::Comma => ",",
      TokenKind::Let => "let",
      TokenKind::Routine => "rt",
      TokenKind::Assign => "=",
      TokenKind::Function => "fn",
      TokenKind::Semicolon => ";",
      TokenKind::Pf => "pf",
      TokenKind::Dot => ".",
      TokenKind::LParen => "(",
      TokenKind::RParen => ")",
      TokenKind::LBrace => "{",
      TokenKind::RBrace => "}",
      TokenKind::InvalidUtf8 | TokenKind::Eof => "",
      TokenKind::Ident | TokenKind::IntLit | TokenKind::AsciiLit => strings.get(self.index),
    }
  }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Token {
  pub kind: TokenKind,
  pub offset: u32,
  pub index: Index,
}
