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
    let len = self.src.len();
    while !self.eof() && self.src[self.pos].is_ascii_whitespace() {
      self.pos += 1;
    }
    let mut token = Token::new(T::Eof, self.loc());
    if self.pos >= len {
      token.loc.len = 0;
      return token;
    }
    token.loc.offset = self.pos as u32;
    match self.src[self.pos] {
      b'{' => token.kind = T::LBrace,
      b'}' => token.kind = T::RBrace,
      b';' => token.kind = T::Semicolon,
      b'.' => token.kind = T::Dot,
      b'(' => token.kind = T::LParen,
      b')' => token.kind = T::RParen,
      b'=' => token.kind = T::Eq,
      b'-' if self.peek() == b'>' => {
        self.pos += 1;
        token.loc.len += 1;
        token.kind = T::Arrow;
      }
      b'a' if self.peek() == b'"' => return self.asciilit(token),
      b if b.is_ascii_digit() => return self.intlit(token),
      b if b.is_ascii_alphabetic() => return self.ident(token),
      b => todo!("unhandled byte {b}"),
    }
    self.pos += 1;
    token
  }

  fn intlit(&mut self, mut token: Token) -> Token {
    self.pos += 1;
    while !self.eof() && self.src[self.pos].is_ascii_digit() {
      self.pos += 1;
      token.loc.len += 1;
    }
    token.kind = T::IntLit;
    token
  }

  fn asciilit(&mut self, mut token: Token) -> Token {
    self.pos += 2; // a"
    token.loc.offset += 2;
    // TODO: escaped quotes
    while !self.eof() && self.peek() != b'"' {
      self.pos += 1;
      token.loc.len += 1;
    }
    // TODO: handle newline, eof, errs...
    self.pos += 2;
    token.kind = T::AsciiLit;
    token
  }

  fn ident(&mut self, mut token: Token) -> Token {
    self.pos += 1;
    while !self.eof() && self.src[self.pos].is_ascii_alphanumeric() {
      self.pos += 1;
      token.loc.len += 1;
    }
    token.kind = match self.lexeme(token.loc) {
      b"let" => T::Let,
      b"fn" => T::Fn,
      b"rt" => T::Rt,
      b"pf" => T::Pf,
      _ => T::Ident,
    };
    token
  }

  fn peek(&self) -> u8 {
    *self.src.get(self.pos + 1).unwrap_or(&0)
  }

  fn eof(&self) -> bool {
    self.pos >= self.src.len()
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
  fn single_char_tokens_and_whitespace() {
    let mut lexer = Lexer::new(b"{ };.(\t)\n=".to_vec());
    let cases: &[(T, (u32, u16), &[u8])] = &[
      (T::LBrace, (0, 1), b"{"),
      (T::RBrace, (2, 1), b"}"),
      (T::Semicolon, (3, 1), b";"),
      (T::Dot, (4, 1), b"."),
      (T::LParen, (5, 1), b"("),
      (T::RParen, (7, 1), b")"),
      (T::Eq, (9, 1), b"="),
      (T::Eof, (10, 0), b""),
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

  #[test]
  fn multi_char_tokens() {
    let mut lexer = Lexer::new(b"let fn foo -> rt pf a\"bar\" 17".to_vec());
    let cases: &[(T, (u32, u16), &[u8])] = &[
      (T::Let, (0, 3), b"let"),
      (T::Fn, (4, 2), b"fn"),
      (T::Ident, (7, 3), b"foo"),
      (T::Arrow, (11, 2), b"->"),
      (T::Rt, (14, 2), b"rt"),
      (T::Pf, (17, 2), b"pf"),
      (T::AsciiLit, (22, 3), b"bar"),
      (T::IntLit, (27, 2), b"17"),
      (T::Eof, (29, 0), b""),
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

  #[test]
  fn simple_program() {
    let input = r#"
rt main() -> pf.MainReturn {
  let msg = a"hello steve!";
  pf.print(msg);
  .ok(17)
}"#;
    let mut lexer = Lexer::new(input.bytes().collect());
    let cases: &[(T, &[u8])] = &[
      (T::Rt, b"rt"),
      (T::Ident, b"main"),
      (T::LParen, b"("),
      (T::RParen, b")"),
      (T::Arrow, b"->"),
      (T::Pf, b"pf"),
      (T::Dot, b"."),
      (T::Ident, b"MainReturn"),
      (T::LBrace, b"{"),
      (T::Let, b"let"),
      (T::Ident, b"msg"),
      (T::Eq, b"="),
      (T::AsciiLit, b"hello steve!"),
      (T::Semicolon, b";"),
      (T::Pf, b"pf"),
      (T::Dot, b"."),
      (T::Ident, b"print"),
      (T::LParen, b"("),
      (T::Ident, b"msg"),
      (T::RParen, b")"),
      (T::Semicolon, b";"),
      (T::Dot, b"."),
      (T::Ident, b"ok"),
      (T::LParen, b"("),
      (T::IntLit, b"17"),
      (T::RParen, b")"),
      (T::RBrace, b"}"),
    ];
    for (tkind, lexeme) in cases {
      let token = lexer.next_token();
      assert_eq!(token.kind, *tkind);
      assert_eq!(lexer.lexeme(token.loc), *lexeme);
    }
  }
}
