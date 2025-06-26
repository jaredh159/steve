#![allow(dead_code)]
use crate::internal::{TokenKind as T, *};
use into_mem::IntoAst;
use mem::{Element, Mem};
use parse_nodes::*;
use std::sync::Once;
use tracing_subscriber::fmt::format::FmtSpan;
use tracing_subscriber::{fmt, EnvFilter};
use ParseError as E;

mod into_mem;
mod parse_nodes;

#[derive(Debug)]
pub struct Parser {
  ctx: Context,
  stack: ParseNodes,
  tok_pos: usize,
}

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
  Dot,
}

type PrefixParseFn = fn(&mut Parser) -> Option<Expr>;
type InfixParseFn = fn(&mut Parser, Expr) -> Option<Expr>;

impl Parser {
  pub fn new_str(src: &str) -> Parser {
    #[cfg(debug_assertions)]
    configure_test_tracing();

    let lexer = Lexer::new(Context::new_str(src));
    let ctx = lexer.lex();
    Parser {
      ctx,
      stack: ParseNodes::with_capacity(32),
      tok_pos: 0,
    }
  }

  #[instrument(skip_all)]
  pub fn parse(mut self) -> Context {
    trace!("Parser::parse()");
    while self.parse_decl() {}
    self.ctx.reset()
  }

  #[instrument(skip_all)]
  fn parse_block_stmts(&mut self, token: u32) {
    let block_n = self.ctx.ast_data.push(Mem::Fixup, u32::MAX);
    let next_n = self.ctx.ast_data.push(Mem::Fixup, u32::MAX);
    let mut num_stmts = 0;
    loop {
      if self.cur_token_is(T::RBrace) {
        self.advance();
        break;
      } else if let Some(stmt) = self.parse_stmt() {
        num_stmts += 1;
        stmt.into_ast(&mut self.stack, &mut self.ctx.ast_data);
      } else {
        break;
      }
    }
    let block_e = self.ctx.ast_data.get_mut(block_n).unwrap();
    *block_e = Element::ast(Mem::Block { num_stmts }, token);
    let next_idx = self.ctx.ast_data.len() as u32;
    let next_e = self.ctx.ast_data.get_mut(next_n).unwrap();
    *next_e = Element::idx(idx::AstNode::new(next_idx));
  }

  #[instrument(skip_all)]
  fn parse_stmt(&mut self) -> Option<Stmt> {
    self
      .parse_var_decl_stmt()
      .or_else(|| self.parse_expr_stmt())
      .or_else(|| self.parse_return_stmt())
  }

  #[instrument(skip_all)]
  fn parse_return_stmt(&mut self) -> Option<Stmt> {
    // remove const
    None
  }

  #[instrument(skip_all)]
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
      self
        .ctx
        .parse_errors
        .push(E::ExpectedExpression(self.cur_token()));
      return None;
    };

    self.consume_expecting(T::Semicolon);
    self.stack.push_expr(expr);
    Some(Stmt::Let { token, has_type: false })
  }

  #[instrument(skip_all)]
  fn parse_expr_stmt(&mut self) -> Option<Stmt> {
    let token = self.tok_pos as u32;
    let expr = self.parse_expr(Prec::Lowest)?;
    self.stack.push_expr(expr);
    if self.cur_token_is(T::Semicolon) {
      self.advance();
      Some(Stmt::Expression { token })
    } else {
      Some(Stmt::Return { token })
    }
  }

  #[instrument(skip_all)]
  fn parse_expr(&mut self, prec: Prec) -> Option<Expr> {
    let Some(mut expr) = self.prefix_parse_fn().and_then(|f| f(self)) else {
      self
        .ctx
        .parse_errors
        .push(E::ExpectedExpression(self.cur_token()));
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

  #[instrument(skip_all)]
  fn parse_ascii_lit(&mut self) -> Option<Expr> {
    Some(Expr::AsciiLit { token: self.advance() })
  }

  #[instrument(skip_all)]
  fn parse_ident(&mut self) -> Option<Expr> {
    Some(Expr::Ident { token: self.advance() })
  }

  #[instrument(skip_all)]
  fn parse_int_lit(&mut self) -> Option<Expr> {
    let lexeme = self.ctx.tokens[self.tok_pos].lexeme(&self.ctx.strs);
    // TODO: handle negative numbers, etc...
    let value = match lexeme.parse::<u64>() {
      Ok(value) => value,
      Err(_) => {
        self
          .ctx
          .parse_errors
          .push(E::InvalidIntLit(self.cur_token()));
        0
      }
    };
    Some(Expr::IntLit { value, token: self.advance() })
  }

  #[instrument(skip_all)]
  fn parse_decl(&mut self) -> bool {
    self.parse_fn_decl()
  }

  #[instrument(skip_all)]
  fn parse_fn_decl(&mut self) -> bool {
    if self.is_eof() {
      return false;
    }

    let kind = self.ctx.tokens[self.tok_pos].kind;
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

    let type_token = self.tok_pos;
    let type_expr = match self.parse_expr(Prec::Lowest) {
      Some(expr @ Expr::MemberAccess { .. }) => expr,
      Some(expr @ Expr::Ident { .. }) => expr,
      _ => {
        self
          .ctx
          .parse_errors
          .push(E::ExpectedType(self.ctx.tokens[type_token]));
        self.synchronize();
        return false;
      }
    };

    self.stack.push_expr(type_expr);
    self.stack.push_expr(Expr::Ident { token: ident_token });

    let Some(block_token) = self.consume_expecting(T::LBrace) else {
      self.synchronize();
      return false;
    };

    let fn_decl = Decl::Function {
      num_args,
      token: fn_token,
      is_pure: kind == T::Function,
      discardable: false, // TODO
    };

    fn_decl.into_ast(&mut self.stack, &mut self.ctx.ast_data);
    self.parse_block_stmts(block_token);
    true
  }

  #[instrument(skip_all)]
  fn parse_implicit_member_access(&mut self) -> Option<Expr> {
    let dot_token = self.advance(); // `.`
    let Some(ident_token) = self.consume_expecting(T::Ident) else {
      self.synchronize();
      return None;
    };
    self.stack.push_expr(Expr::Ident { token: ident_token });
    Some(Expr::MemberAccess { token: dot_token, implicit: true })
  }

  #[instrument(skip_all)]
  fn parse_platform_keyword(&mut self) -> Option<Expr> {
    Some(Expr::PlatformKeyword { token: self.advance() })
  }

  #[instrument(skip_all)]
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

  #[instrument(skip_all)]
  fn parse_call_expression(&mut self, lhs: Expr) -> Option<Expr> {
    let token = self.advance(); // `(`
    self.stack.push_expr(lhs);
    let num_args = self.parse_expr_list(T::RParen);
    let expr = Expr::Call { token, num_args };
    Some(expr)
  }

  #[instrument(skip_all)]
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
    self.ctx.tokens[self.tok_pos]
  }

  fn cur_token_is(&self, kind: TokenKind) -> bool {
    self.ctx.tokens[self.tok_pos].kind == kind
  }

  fn peek_token(&self) -> Option<Token> {
    self.ctx.tokens.get(self.tok_pos + 1).copied()
  }

  fn peek_token_is(&self, kind: TokenKind) -> bool {
    if self.is_eof() {
      return kind == T::Eof;
    }
    self.ctx.tokens[self.tok_pos + 1].kind == kind
  }

  fn is_eof(&self) -> bool {
    self.tok_pos >= self.ctx.tokens.len() - 1
  }

  fn consume_expecting(&mut self, kind: TokenKind) -> Option<u32> {
    let token = self.ctx.tokens[self.tok_pos];
    if self.cur_token_is(kind) {
      let index = self.tok_pos as u32;
      self.tok_pos += 1;
      Some(index)
    } else {
      self
        .ctx
        .parse_errors
        .push(E::ExpectedToken { kind, found: token });
      None
    }
  }

  fn advance(&mut self) -> u32 {
    let pos = self.tok_pos as u32;
    self.tok_pos += 1;
    pos
  }

  fn synchronize(&mut self) {
    todo!("implement synchronize for error recovery")
  }
}

fn precedence(token: Option<Token>) -> Prec {
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

#[cfg(debug_assertions)]
static INIT: Once = Once::new();

#[cfg(debug_assertions)]
fn configure_test_tracing() {
  INIT.call_once(|| {
    let subscriber = fmt::Subscriber::builder()
      .with_env_filter(EnvFilter::from_default_env())
      .with_test_writer()
      .with_span_events(FmtSpan::ACTIVE)
      .finish();
    tracing::subscriber::set_global_default(subscriber)
      .expect("setting default tracing subscriber failed");
  });
}

#[cfg(test)]
mod tests {
  use super::*;
  use crate::ast::mem::*;
  use pretty_assertions::assert_eq;
  use Dissembled as D;
  use Mem::*;
  use TokenNode as TN;

  #[test]
  fn parse_first_goal_program() {
    let input = r#"
      rt main() -> pf.MainReturn {
        let msg = a"hello steve!";
        pf.print(msg);
        .err(17)
      }"#;
    let parser = Parser::new_str(input);
    let dissembled = parser.parse().ast_data.dissemble();
    assert_eq!(
      &dissembled,
      &[
        D::Node(TN::new(FnDecl(FnDeclData::new(0, false, false)), 0)),
        D::Node(TN::new(Ident, 1)), // "main"
        D::Node(TN::new(Mem::MemberAccess { implicit: false }, 6)), // type expr
        D::Node(TN::new(PlatformKeyword, 5)), // "pf"
        D::Node(TN::new(Ident, 7)), // "MainReturn"
        D::Node(TN::new(Block { num_stmts: 3 }, 8)),
        D::Idx(idx::AstNode::new(21)), // node after the block stmt
        D::Node(TN::new(VarDeclStmt { has_type: false }, 9)),
        D::Node(TN::new(Ident, 10)), // "msg"
        D::Node(TN::new(AsciiLit, 12)),
        D::Node(TN::new(ExprStmt, 14)),
        D::Node(TN::new(CallExpr { num_args: 1 }, 17)),
        D::Node(TN::new(Ident, 18)),
        D::Node(TN::new(Mem::MemberAccess { implicit: false }, 15)),
        D::Node(TN::new(PlatformKeyword, 14)), // "pf"
        D::Node(TN::new(Ident, 16)),           // "print"
        D::Node(TN::new(ReturnStmt, 21)),
        D::Node(TN::new(CallExpr { num_args: 1 }, 23)),
        D::Node(TN::new(IntLit(IntData::new(u2::new(0), u14::new(17))), 24)),
        D::Node(TN::new(Mem::MemberAccess { implicit: true }, 21)),
        D::Node(TN::new(Ident, 22)),
      ]
    );
  }

  #[test]
  fn parse_fn_return_mismatch() {
    let parser = Parser::new_str("fn bad() -> bool { 3 }");
    let dissembled = parser.parse().ast_data.dissemble();
    assert_eq!(
      &dissembled,
      &[
        D::Node(TN::new(FnDecl(FnDeclData::new(0, true, false)), 0)),
        D::Node(TN::new(Ident, 1)), // "bad"
        D::Node(TN::new(Ident, 5)), // "bool"
        D::Node(TN::new(Block { num_stmts: 1 }, 6)),
        D::Idx(idx::AstNode::new(7)), // node after the block stmt
        D::Node(TN::new(ReturnStmt, 7)),
        D::Node(TN::new(IntLit(IntData::new(u2::new(0), u14::new(3))), 7)),
      ]
    );
  }
}
