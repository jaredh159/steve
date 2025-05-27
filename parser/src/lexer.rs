use crate::src_loc::*;
use crate::token::{Token, TokenKind as T};

#[derive(Debug)]
pub struct Lexer {
  pub src: Vec<u8>,
  pos: usize,
  file_idx: u16,
}

impl Lexer {
  pub fn new(src: Vec<u8>) -> Self {
    assert!(src.len() <= u32::MAX as usize);
    Lexer { src, pos: 0, file_idx: 0 }
  }

  pub fn next_token(&mut self) -> Token {
    let mut token = Token::new(T::Eof, self.loc());
    if self.pos >= self.src.len() {
      token.loc.len = 0;
      return token;
    }
    while self.src[self.pos].is_ascii_whitespace() {
      self.pos += 1;
    }
    token.loc.offset = self.pos as u32;
    match self.src[self.pos] {
      b'{' => token.kind = T::LBrace,
      b'}' => token.kind = T::RBrace,
      b';' => token.kind = T::Semicolon,
      b'.' => token.kind = T::Dot,
      b'(' => token.kind = T::LParen,
      b')' => token.kind = T::RParen,
      b => todo!("unhandled byte {b}"),
    }
    self.pos += 1;
    token
  }

  pub fn lexeme(&self, loc: SrcLoc) -> &[u8] {
    loc.bytes(&self.src)
  }

  fn loc(&self) -> SrcLoc {
    SrcLoc::new(self.pos as u32, 1, self.file_idx)
  }
}

#[cfg(test)]
mod tests {
  use super::*;
  use crate::token::TokenKind as T;

  #[test]
  fn lex_single_byte_tokens_and_whitespace() {
    let mut lexer = Lexer::new(b"{ };.(\t)".to_vec());
    let cases: &[(T, (u32, u16), &[u8])] = &[
      (T::LBrace, (0, 1), b"{"),
      (T::RBrace, (2, 1), b"}"),
      (T::Semicolon, (3, 1), b";"),
      (T::Dot, (4, 1), b"."),
      (T::LParen, (5, 1), b"("),
      (T::RParen, (7, 1), b")"),
      (T::Eof, (8, 0), b""),
    ];
    for (tkind, (offset, len), lexeme) in cases {
      let actual = lexer.next_token();
      let expected = Token {
        kind: *tkind,
        loc: SrcLoc::new(*offset, *len, 0),
      };
      assert_eq!(actual, expected);
      assert_eq!(lexer.lexeme(actual.loc), *lexeme);
    }
  }
}
