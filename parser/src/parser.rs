#![allow(unused_imports)]
#![allow(dead_code)]
use crate::ast::*;
use crate::diag::Diagnostic;
use crate::lexer::Lexer;
use crate::node::{DataNodeKind as N, *};
use crate::str_pool::StringPool;
use crate::token::{Token, TokenKind, TokenKind as T};
use ParseError as E;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ParseError {
  ExpectedToken { kind: TokenKind, found: Token },
  ExpectedExpression(Token),
}

#[derive(Eq, Ord, PartialEq, PartialOrd, Copy, Clone, Debug)]
enum Prec {
  Lowest,
  Equals,
  LessGreater,
  Sum,
  Product,
  Prefix,
  Call,
  Index,
  Dot, // correct ???
}

type PrefixParseFn = fn(&mut Parser) -> Option<Expr>;
type InfixParseFn = fn(&mut Parser, &Expr) -> Option<Expr>;

#[derive(Debug)]
pub struct Parser {
  src: Vec<u8>,
  strings: StringPool,
  tokens: Vec<Token>,
  d_nodes: Vec<DataNode>,
  nodes: AstNodes,
  errors: Vec<ParseError>,
  pos: usize,
}

impl Parser {
  pub fn new_str(src: &str) -> Self {
    let lexer = Lexer::new_str(src);
    let (tokens, src, strings) = lexer.lex();
    Self {
      tokens,
      src,
      strings,
      d_nodes: Vec::with_capacity(64),
      nodes: AstNodes::with_capacity(32),
      errors: Vec::new(),
      pos: 0,
    }
  }

  pub fn parse(mut self) -> Result<Vec<DataNode>, Vec<ParseError>> {
    self.todo_skip_open_main();
    // while self.parse_stmt() {}
    // if self.errors.is_empty() {
    //   Ok(self.nodes)
    // } else {
    //   Err(self.errors)
    // }
    Ok(vec![])
  }

  fn parse_stmt(&mut self) -> Option<Stmt> {
    self
      .parse_var_decl_stmt()
      .or_else(|| self.parse_expr_stmt())
      .or_else(|| self.parse_return_stmt())
  }

  const fn parse_return_stmt(&mut self) -> Option<Stmt> {
    // remove const
    None
  }

  fn parse_var_decl_stmt(&mut self) -> Option<Stmt> {
    if !self.cur_token_is(T::Let) || !self.peek_token_is(T::Ident) {
      eprintln!("1");
      return None;
    }
    let token = self.advance();
    let ident = Expr::Ident { token: self.advance() };
    self.nodes.push_expr(ident);

    // TODO: parse opt. type annotation, setting decl node if present

    if self.consume_expecting(T::Assign).is_none() {
      self.synchronize();
      eprintln!("2");
      return None;
    }

    let Some(expr) = self.parse_expr(Prec::Lowest) else {
      self.errors.push(E::ExpectedExpression(self.cur_token()));
      eprintln!("3");
      return None;
    };

    self.consume_expecting(T::Semicolon);
    self.nodes.push_expr(expr);
    Some(Stmt::Let { token, has_type: false })
  }

  fn parse_expr_stmt(&mut self) -> Option<Stmt> {
    let stmt = Stmt::Expression { token: self.pos as u32 };
    let expr = self.parse_expr(Prec::Lowest)?;
    self.consume_expecting(T::Semicolon);
    self.nodes.push_expr(expr);
    Some(stmt)
    // monkey...
    // let initial_token = self.cur_token.clone();
    // let expression = self.parse_expression(Precedence::Lowest)?;
    // if self.peek_token == Token::Semicolon {
    //   self.advance();
    // }
    // Some(Statement::Expression(initial_token, expression))
    // None
  }

  fn parse_expr(&mut self, prec: Prec) -> Option<Expr> {
    let Some(prefix_parser) = self.prefix_parse_fn() else {
      self.errors.push(E::ExpectedExpression(self.cur_token()));
      return None;
    };
    let Some(mut expr) = prefix_parser(self) else {
      self.errors.push(E::ExpectedExpression(self.cur_token()));
      return None;
    };
    if self.cur_token_is(T::Semicolon) {
      self.advance();
      return Some(expr);
    }
    while !self.peek_token_is(T::Semicolon) && prec < precedence(Some(self.cur_token())) {
      if let Some(infix_fn) = self.infix_parse_fn() {
        if let Some(next) = infix_fn(self, &expr) {
          expr = next;
        }
      }
    }
    // while self.peek_token != Token::Semicolon && precedence < self.peek_token.precedence()
    // {
    //   if let Some(infix_fn) = self.infix_parse_fn() {
    //     self.advance();
    //     expr = infix_fn(self, expr.clone()).unwrap_or(expr);
    //   } else {
    //     return Some(expr);
    //   }
    // }
    Some(expr)
  }

  const fn parse_ascii_lit(&mut self) -> Option<Expr> {
    Some(Expr::AsciiLit { token: self.advance() })
  }

  const fn parse_ident(&mut self) -> Option<Expr> {
    Some(Expr::Ident { token: self.advance() })
  }

  const fn parse_platform_keyword(&mut self) -> Option<Expr> {
    Some(Expr::PlatformKeyword { token: self.advance() })
  }

  fn parse_field_access(&mut self, lhs: &Expr) -> Option<Expr> {
    let dot_token = self.advance(); // dot
    let Some(ident_token) = self.consume_expecting(T::Ident) else {
      self.synchronize();
      return None;
    };
    self.nodes.push_expr(lhs.clone());
    self.nodes.push_expr(Expr::Ident { token: ident_token });
    Some(Expr::FieldAccess { token: dot_token })
  }

  fn parse_call_expression(&mut self, lhs: &Expr) -> Option<Expr> {
    let token = self.advance(); // (
    self.nodes.push_expr(lhs.clone());
    let num_args = self.parse_expr_list(T::RParen);
    let expr = Expr::CallExpr { token, num_args };
    Some(expr)
  }

  fn parse_expr_list(&mut self, end: TokenKind) -> u8 {
    let mut count: u8 = 0;
    if self.cur_token_is(end) {
      self.advance();
      return count;
    }
    loop {
      let Some(arg) = self.parse_expr(Prec::Lowest) else {
        return count;
      };
      self.nodes.push_expr(arg);
      count += 1;
      if self.cur_token_is(end) {
        return count;
        // TODO: continue on comma, lol
      }
    }
  }

  fn push_node(&mut self, kind: DataNodeKind) {
    self.d_nodes.push(DataNode { kind, token: self.pos as u32 });
  }

  fn infix_parse_fn(&mut self) -> Option<InfixParseFn> {
    match self.cur_token().kind {
      T::Dot => Some(Self::parse_field_access),
      T::LParen => Some(Self::parse_call_expression),
      T::Arrow | T::Let | T::Routine | T::Ident | T::IntLit | T::InvalidUtf8 => todo!(),
      T::AsciiLit | T::Assign | T::Function | T::Semicolon | T::Pf | T::RParen => todo!(),
      T::LBrace | T::RBrace | T::Comma | T::Eof => todo!(),
    }
  }

  fn prefix_parse_fn(&mut self) -> Option<PrefixParseFn> {
    match self.cur_token().kind {
      T::AsciiLit => Some(Self::parse_ascii_lit),
      T::Pf => Some(Self::parse_platform_keyword),
      T::Ident => Some(Self::parse_ident),
      T::Arrow | T::Let | T::Routine | T::InvalidUtf8 | T::Assign => todo!(),
      T::Function | T::Semicolon | T::Dot | T::LParen | T::RParen => todo!(),
      T::IntLit | T::Comma | T::LBrace | T::RBrace | T::Eof => todo!(),
    }
  }

  fn cur_token(&self) -> Token {
    self.tokens[self.pos]
  }

  fn cur_token_is(&self, kind: TokenKind) -> bool {
    self.tokens[self.pos].kind == kind
  }

  fn peek_token(&self) -> Option<Token> {
    self.tokens.get(self.pos + 1).copied()
  }

  fn peek_token_is(&self, kind: TokenKind) -> bool {
    if self.pos >= self.tokens.len() {
      return kind == T::Eof;
    }
    self.tokens[self.pos + 1].kind == kind
  }

  fn consume_expecting(&mut self, kind: TokenKind) -> Option<u32> {
    let token = self.tokens[self.pos];
    if self.cur_token_is(kind) {
      let index = self.pos as u32;
      self.pos += 1;
      Some(index)
    } else {
      self.errors.push(E::ExpectedToken { kind, found: token });
      None
    }
  }

  const fn advance(&mut self) -> u32 {
    let pos = self.pos as u32;
    self.pos += 1;
    pos
  }

  fn synchronize(&mut self) {
    todo!("implement synchronize for error recovery")
  }

  fn todo_skip_open_main(&mut self) {
    let mut token = self.tokens[0];
    while token.kind != T::Eof && token.kind != T::LBrace {
      self.pos += 1;
      token = self.tokens[self.pos];
    }
    self.pos += 1;
  }
}

const fn precedence(token: Option<Token>) -> Prec {
  let Some(token) = token else {
    return Prec::Lowest;
  };
  match token.kind {
    TokenKind::Dot => Prec::Dot,
    TokenKind::LParen => Prec::Call,
    TokenKind::Arrow
    | TokenKind::Let
    | TokenKind::Routine
    | TokenKind::Ident
    | TokenKind::IntLit
    | TokenKind::InvalidUtf8
    | TokenKind::AsciiLit
    | TokenKind::Assign
    | TokenKind::Function
    | TokenKind::Semicolon
    | TokenKind::Pf
    | TokenKind::RParen
    | TokenKind::LBrace
    | TokenKind::RBrace
    | TokenKind::Comma
    | TokenKind::Eof => Prec::Lowest,
  }
}

#[cfg(test)]
mod tests {
  use super::*;
  use pretty_assertions::assert_eq;

  fn idx(idx: u32) -> Index {
    Index::new(idx)
  }

  #[test]
  fn first_goal_program() {
    let input = r#"
rt main() -> pf.MainReturn {
  let msg = a"hello steve!";
  pf.print(msg);
  .ok(17)
}"#;
    let mut parser = Parser::new_str(input);
    parser.todo_skip_open_main();

    let let_stmt = parser.parse_stmt().unwrap();
    assert_eq!(let_stmt, Stmt::Let { token: 9, has_type: false });

    let mut data_nodes = Vec::new();
    let_stmt.into_data_nodes(&mut parser.nodes, &mut data_nodes);
    assert_eq!(
      &data_nodes,
      &[
        DataNode {
          kind: N::VarDeclStmt(VarDecl_D::new(false)),
          token: 9
        },
        DataNode { kind: N::Ident, token: 10 }, // var decl ident `msg`
        DataNode { kind: N::AsciiLit, token: 12 },
      ]
    );
    assert!(parser.nodes.is_empty());

    let expr_stmt = parser.parse_stmt().unwrap();
    assert_eq!(expr_stmt, Stmt::Expression { token: 14 });
    assert_eq!(
      parser.nodes.last().unwrap(),
      &Expr::CallExpr { token: 17, num_args: 1 }
    );

    let mut data_nodes = Vec::new();
    expr_stmt.into_data_nodes(&mut parser.nodes, &mut data_nodes);
    assert_eq!(
      &data_nodes,
      &[
        DataNode { kind: N::ExprStmt, token: 14 },
        DataNode { kind: N::CallExpr(1), token: 17 },
        DataNode { kind: N::Ident, token: 18 }, // arg 0 ident "msg"
        DataNode { kind: N::FieldAccess, token: 15 }, // call expression callee
        DataNode { kind: N::PlatformKeyword, token: 14 }, // field access `receiver` (pf)
        DataNode { kind: N::Ident, token: 16 }, // field access`field` (print)
      ]
    );
    assert!(parser.nodes.is_empty());
  }
}
