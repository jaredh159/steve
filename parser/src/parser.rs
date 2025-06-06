#![allow(unused_imports)]
#![allow(dead_code)]
use bilge::prelude::*;

use crate::diag::Diagnostic;
use crate::into_nodes::IntoNodes;
use crate::lexer::Lexer;
use crate::node::{DataNodeKind as N, *};
use crate::str_pool::StringPool;
use crate::token::{Token, TokenKind, TokenKind as T};
use crate::{ast::*, ast_nodes::*};
use ParseError as E;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ParseError {
  ExpectedToken { kind: TokenKind, found: Token },
  ExpectedExpression(Token),
  ExpectedType(Token),
  InvalidIntLit(Token),
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
type InfixParseFn = fn(&mut Parser, Expr) -> Option<Expr>;

#[derive(Debug)]
pub struct Parser {
  src: Vec<u8>,
  strings: StringPool,
  tokens: Vec<Token>,
  nodes: Vec<Node>,
  stack: AstNodes,
  errors: Vec<ParseError>,
  pos: usize,
}

#[derive(Debug)]
pub struct Parsed {
  pub src: Vec<u8>,
  pub strings: StringPool,
  pub tokens: Vec<Token>,
  pub result: Result<Vec<Node>, Vec<ParseError>>,
}

impl Parser {
  pub fn new_str(src: &str) -> Self {
    let lexer = Lexer::new_str(src);
    let (tokens, src, strings) = lexer.lex();
    Self {
      tokens,
      src,
      strings,
      nodes: Vec::with_capacity(64),
      stack: AstNodes::with_capacity(32),
      errors: Vec::new(),
      pos: 0,
    }
  }

  pub fn parse(mut self) -> Parsed {
    while self.parse_decl() {}
    Parsed {
      src: self.src,
      strings: self.strings,
      tokens: self.tokens,
      result: if self.errors.is_empty() { Ok(self.nodes) } else { Err(self.errors) },
    }
  }

  fn parse_block_expr(&mut self) {
    loop {
      if self.cur_token_is(T::RBrace) {
        self.advance();
        break;
      } else if let Some(stmt) = self.parse_stmt() {
        stmt.into_nodes(&mut self.stack, &mut self.nodes);
      } else {
        break;
      }
    }
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
      return None;
    }
    let token = self.advance();
    let ident = Expr::Ident { token: self.advance() };
    self.stack.push_expr(ident);

    // TODO: parse opt. type annotation, setting decl node if present

    if self.consume_expecting(T::Assign).is_none() {
      self.synchronize();
      return None;
    }

    let Some(expr) = self.parse_expr(Prec::Lowest) else {
      self.errors.push(E::ExpectedExpression(self.cur_token()));
      return None;
    };

    self.consume_expecting(T::Semicolon);
    self.stack.push_expr(expr);
    Some(Stmt::Let { token, has_type: false })
  }

  fn parse_expr_stmt(&mut self) -> Option<Stmt> {
    let token = self.pos as u32;
    let expr = self.parse_expr(Prec::Lowest)?;
    self.stack.push_expr(expr);
    if self.cur_token_is(T::Semicolon) {
      self.advance();
      Some(Stmt::Expression { token })
    } else {
      Some(Stmt::Return { token })
    }
  }

  fn parse_expr(&mut self, prec: Prec) -> Option<Expr> {
    let Some(mut expr) = self.prefix_parse_fn().and_then(|f| f(self)) else {
      self.errors.push(E::ExpectedExpression(self.cur_token()));
      return None;
    };
    loop {
      if self.peek_token_is(T::Semicolon) || prec >= precedence(Some(self.cur_token())) {
        break;
      }
      if let Some(infix_fn) = self.infix_parse_fn() {
        expr = infix_fn(self, expr)?;
      } else {
        return Some(expr);
      }
    }
    Some(expr)
  }

  const fn parse_ascii_lit(&mut self) -> Option<Expr> {
    Some(Expr::AsciiLit { token: self.advance() })
  }

  const fn parse_ident(&mut self) -> Option<Expr> {
    Some(Expr::Ident { token: self.advance() })
  }

  fn parse_int_lit(&mut self) -> Option<Expr> {
    let lexeme = self.tokens[self.pos].lexeme(&self.strings);
    // TODO: handle negative numbers, etc...
    let value = match lexeme.parse::<u64>() {
      Ok(value) => value,
      Err(_) => {
        self.errors.push(E::InvalidIntLit(self.cur_token()));
        0
      }
    };
    Some(Expr::IntLit { value, token: self.advance() })
  }

  fn parse_decl(&mut self) -> bool {
    self.parse_fn_decl()
  }

  fn parse_fn_decl(&mut self) -> bool {
    if self.is_eof() {
      return false;
    }

    let kind = self.tokens[self.pos].kind;
    if !matches!(kind, T::Function | T::Routine) {
      return false;
    }
    let fn_token = self.advance();

    let Some(ident_token) = self.consume_expecting(T::Ident) else {
      self.synchronize();
      return false;
    };

    if self.consume_expecting(T::LParen).is_none() {
      self.synchronize();
      return false;
    };

    let num_args = self.parse_expr_list(T::RParen);
    if self.consume_expecting(T::RParen).is_none() {
      self.synchronize();
      return false;
    };

    if self.consume_expecting(T::Arrow).is_none() {
      self.synchronize();
      return false;
    };

    let type_token = self.pos;
    let type_expr = match self.parse_expr(Prec::Lowest) {
      Some(expr @ Expr::MemberAccess { .. }) => expr,
      Some(expr @ Expr::Ident { .. }) => expr,
      _ => {
        self.errors.push(E::ExpectedType(self.tokens[type_token]));
        self.synchronize();
        return false;
      }
    };

    self.stack.push_expr(type_expr);
    self.stack.push_expr(Expr::Ident { token: ident_token });

    if self.consume_expecting(T::LBrace).is_none() {
      self.synchronize();
      return false;
    };

    let fn_decl = Decl::Function {
      num_args,
      token: fn_token,
      is_pure: kind == T::Function,
      discardable: false, // TODO
    };

    fn_decl.into_nodes(&mut self.stack, &mut self.nodes);
    self.parse_block_expr();
    true
  }

  fn parse_implicit_member_access(&mut self) -> Option<Expr> {
    let dot_token = self.advance(); // `.`
    let Some(ident_token) = self.consume_expecting(T::Ident) else {
      self.synchronize();
      return None;
    };
    self.stack.push_expr(Expr::Ident { token: ident_token });
    Some(Expr::MemberAccess { token: dot_token, implicit: true })
  }

  const fn parse_platform_keyword(&mut self) -> Option<Expr> {
    Some(Expr::PlatformKeyword { token: self.advance() })
  }

  fn parse_infix_member_access(&mut self, lhs: Expr) -> Option<Expr> {
    let dot_token = self.advance(); // `.`
    let Some(ident_token) = self.consume_expecting(T::Ident) else {
      self.synchronize();
      return None;
    };
    self.stack.push_expr(lhs);
    self.stack.push_expr(Expr::Ident { token: ident_token });
    Some(Expr::MemberAccess { token: dot_token, implicit: false })
  }

  fn parse_call_expression(&mut self, lhs: Expr) -> Option<Expr> {
    let token = self.advance(); // `(`
    self.stack.push_expr(lhs);
    let num_args = self.parse_expr_list(T::RParen);
    let expr = Expr::CallExpr { token, num_args };
    Some(expr)
  }

  fn parse_expr_list(&mut self, end: TokenKind) -> u8 {
    let mut count: u8 = 0;
    if self.cur_token_is(end) {
      return count;
    }
    loop {
      let Some(arg) = self.parse_expr(Prec::Lowest) else {
        // TODO: error?
        return count;
      };
      self.stack.push_expr(arg);
      count += 1;
      if self.cur_token_is(end) {
        self.advance(); // `)`
        return count;
      } else if self.cur_token_is(T::Comma) {
        self.advance();
      }
    }
  }

  fn infix_parse_fn(&mut self) -> Option<InfixParseFn> {
    match self.cur_token().kind {
      T::Dot => Some(Self::parse_infix_member_access),
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
      T::Dot => Some(Self::parse_implicit_member_access),
      T::IntLit => Some(Self::parse_int_lit),
      T::Routine | T::Function => todo!(),
      T::Arrow | T::Let | T::InvalidUtf8 | T::Assign => todo!(),
      T::Semicolon | T::LParen | T::RParen => todo!(),
      T::Comma | T::LBrace | T::RBrace | T::Eof => todo!(),
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
    if self.is_eof() {
      return kind == T::Eof;
    }
    self.tokens[self.pos + 1].kind == kind
  }

  const fn is_eof(&self) -> bool {
    self.pos >= self.tokens.len() - 1
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

  #[test]
  fn first_goal_program() {
    let input = r#"
rt main() -> pf.MainReturn {
  let msg = a"hello steve!";
  pf.print(msg);
  .ok(17)
}"#;
    let parser = Parser::new_str(input);
    let nodes = parser.parse().result.unwrap();
    assert_eq!(
      &nodes.into_iter().map(Node::as_ast).collect::<Vec<_>>(),
      &[
        DataNode::new(N::FnDecl(FnDeclData::new(0, false, false)), 0),
        DataNode::new(N::Ident, 1),                            // "main"
        DataNode::new(N::MemberAccess { implicit: false }, 6), // type expr
        DataNode::new(N::PlatformKeyword, 5),                  // "pf"
        DataNode::new(N::Ident, 7),                            // "MainReturn"
        DataNode::new(N::VarDeclStmt { has_type: false }, 9),
        DataNode::new(N::Ident, 10), // "msg"
        DataNode::new(N::AsciiLit, 12),
        DataNode::new(N::ExprStmt, 14),
        DataNode::new(N::CallExpr { num_args: 1 }, 17),
        DataNode::new(N::Ident, 18),
        DataNode::new(N::MemberAccess { implicit: false }, 15),
        DataNode::new(N::PlatformKeyword, 14), // "pf"
        DataNode::new(N::Ident, 16),           // "print"
        DataNode::new(N::ReturnStmt, 21),
        DataNode::new(N::CallExpr { num_args: 1 }, 23),
        DataNode::new(N::IntLit(IntData::new(u2::new(0), u14::new(17))), 24),
        DataNode::new(N::MemberAccess { implicit: true }, 21),
        DataNode::new(N::Ident, 22),
      ]
    );
  }
}
