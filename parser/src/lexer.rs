use crate::internal::{TokenKind as T, *};

#[derive(Debug)]
pub struct Lexer {
  pub src: Vec<u8>,
  strings: StringPool,
  pos: usize,
}

impl Lexer {
  pub fn new(src: Vec<u8>) -> Self {
    assert!(src.len() <= u32::MAX as usize);
    Lexer {
      src,
      pos: 0,
      strings: StringPool::new(),
    }
  }

  pub fn new_str(src: &str) -> Self {
    Self::new(src.bytes().collect())
  }

  pub fn lex(mut self) -> (Vec<Token>, Vec<u8>, StringPool) {
    let mut tokens = Vec::with_capacity(64);
    while !self.eof() {
      tokens.push(self.next_token());
    }
    (tokens, self.src, self.strings)
  }

  fn simple_token(&mut self, kind: TokenKind) -> Token {
    let token = Token::new(kind, self.pos as u32, self.strings.empty());
    let len = match kind {
      T::Assign
      | T::Semicolon
      | T::Dot
      | T::LParen
      | T::RParen
      | T::LBrace
      | T::RBrace
      | T::Comma => 1,
      T::Let => 3,
      T::Arrow | T::Function | T::Pf => 2,
      T::Routine => 5,
      T::Eof => 0,
      T::Ident | T::IntLit | T::AsciiLit | T::InvalidUtf8 => unreachable!(),
    };
    self.pos += len;
    token
  }

  pub fn next_token(&mut self) -> Token {
    let len = self.src.len();
    while !self.eof() && self.src[self.pos].is_ascii_whitespace() {
      self.pos += 1;
    }
    if self.pos >= len {
      return self.simple_token(T::Eof);
    }
    match self.src[self.pos] {
      b'{' => self.simple_token(T::LBrace),
      b'}' => self.simple_token(T::RBrace),
      b';' => self.simple_token(T::Semicolon),
      b'.' => self.simple_token(T::Dot),
      b'(' => self.simple_token(T::LParen),
      b')' => self.simple_token(T::RParen),
      b',' => self.simple_token(T::Comma),
      b'=' => self.simple_token(T::Assign),
      b'-' if self.peek() == b'>' => self.simple_token(T::Arrow),
      b'a' if self.peek() == b'"' => self.ascii_lit(),
      b if b.is_ascii_digit() => self.int_lit(),
      b if b.is_ascii_alphabetic() => self.ident(),
      b => todo!("unhandled byte {b}"),
    }
  }

  fn ascii_lit(&mut self) -> Token {
    self.pos += 2; // a"
    let start = self.pos as u32;

    // // TODO: escaped quotes
    while !self.eof() && self.peek() != b'"' {
      self.pos += 1;
    }
    self.pos += 1;
    // TODO: handle newline, eof, errs...

    let span = &self.src[start as usize..self.pos];
    let token = if let Ok(lexeme) = std::str::from_utf8(span) {
      Token::new(T::AsciiLit, start, self.strings.intern(lexeme))
    } else {
      let index = self.strings.sentinel(span.len() as u32);
      Token::new(T::InvalidUtf8, start, index)
    };
    self.pos += 1; // "
    token
  }

  fn int_lit(&mut self) -> Token {
    let start = self.pos as u32;
    self.pos += 1;
    while !self.eof() && self.src[self.pos].is_ascii_digit() {
      self.pos += 1;
    }
    let span = &self.src[start as usize..self.pos];
    // SAFETY: we only have ascii digits, so this is fine
    let lexeme = unsafe { std::str::from_utf8_unchecked(span) };
    let index = self.strings.intern(lexeme);
    Token::new(T::IntLit, start, index)
  }

  fn ident(&mut self) -> Token {
    let start = self.pos as u32;
    self.pos += 1;
    while !self.eof() && self.src[self.pos].is_ascii_alphanumeric() {
      self.pos += 1;
    }
    let span = &self.src[start as usize..self.pos];
    match span {
      b"let" => Token::new(T::Let, start, self.strings.empty()),
      b"fn" => Token::new(T::Function, start, self.strings.empty()),
      b"rt" => Token::new(T::Routine, start, self.strings.empty()),
      b"pf" => Token::new(T::Pf, start, self.strings.empty()),
      _ => {
        let Some(lexeme) = std::str::from_utf8(span).ok() else {
          let index = self.strings.sentinel(span.len() as u32);
          return Token::new(T::InvalidUtf8, start, index);
        };
        Token::new(T::Ident, start, self.strings.intern(lexeme))
      }
    }
  }

  fn peek(&self) -> u8 {
    *self.src.get(self.pos + 1).unwrap_or(&0)
  }

  const fn eof(&self) -> bool {
    self.pos >= self.src.len()
  }
}

#[cfg(test)]
mod tests {
  use super::*;
  use crate::token::TokenKind as T;

  #[test]
  fn single_char_tokens_and_whitespace() {
    let mut lexer = Lexer::new(b"{ };.(\t)\n=,".to_vec());
    let cases: &[(T, u32, &str)] = &[
      (T::LBrace, 0, "{"),
      (T::RBrace, 2, "}"),
      (T::Semicolon, 3, ";"),
      (T::Dot, 4, "."),
      (T::LParen, 5, "("),
      (T::RParen, 7, ")"),
      (T::Assign, 9, "="),
      (T::Comma, 10, ","),
      (T::Eof, 11, ""),
    ];
    for (kind, offset, lexeme) in cases {
      let token = lexer.next_token();
      assert_eq!(token.kind, *kind);
      assert_eq!(token.offset, *offset);
      assert_eq!(token.lexeme(&lexer.strings), *lexeme);
    }
  }

  #[test]
  fn multi_char_tokens() {
    let mut lexer = Lexer::new(b"let fn foo -> rt pf a\"bar\" 17".to_vec());
    let cases: &[(T, u32, &str)] = &[
      (T::Let, 0, "let"),
      (T::Function, 4, "fn"),
      (T::Ident, 7, "foo"),
      (T::Arrow, 11, "->"),
      (T::Routine, 14, "rt"),
      (T::Pf, 17, "pf"),
      (T::AsciiLit, 22, "bar"),
      (T::IntLit, 27, "17"),
      (T::Eof, 29, ""),
    ];
    for (kind, offset, lexeme) in cases {
      let token = lexer.next_token();
      assert_eq!(token.kind, *kind);
      assert_eq!(token.offset, *offset);
      assert_eq!(token.lexeme(&lexer.strings), *lexeme);
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
    let cases: &[(T, &str)] = &[
      (T::Routine, "rt"),
      (T::Ident, "main"),
      (T::LParen, "("),
      (T::RParen, ")"),
      (T::Arrow, "->"),
      (T::Pf, "pf"), // 5
      (T::Dot, "."),
      (T::Ident, "MainReturn"),
      (T::LBrace, "{"),
      (T::Let, "let"),
      (T::Ident, "msg"), // 10
      (T::Assign, "="),
      (T::AsciiLit, "hello steve!"),
      (T::Semicolon, ";"),
      (T::Pf, "pf"),
      (T::Dot, "."), // 15
      (T::Ident, "print"),
      (T::LParen, "("),
      (T::Ident, "msg"),
      (T::RParen, ")"),
      (T::Semicolon, ";"), // 20
      (T::Dot, "."),
      (T::Ident, "ok"),
      (T::LParen, "("),
      (T::IntLit, "17"),
      (T::RParen, ")"), // 25
      (T::RBrace, "}"),
    ];
    for (tkind, lexeme) in cases {
      let token = lexer.next_token();
      assert_eq!(token.kind, *tkind);
      assert_eq!(token.lexeme(&lexer.strings), *lexeme);
    }
  }
}
