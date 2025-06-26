use crate::internal::*;

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
  Call { token: u32, num_args: u8 },
  MemberAccess { token: u32, implicit: bool },
  Ident { token: u32 },
  AsciiLit { token: u32 },
  IntLit { token: u32, value: u64 },
  PlatformKeyword { token: u32 },
}

#[derive(Debug, PartialEq, Eq)]
pub enum ParseNode {
  Declaration(Decl),
  Expression(Expr),
  Statement(Stmt),
}

#[derive(Debug, PartialEq, Eq)]
pub struct ParseNodes(Vec<ParseNode>);

impl ParseNodes {
  pub const fn new() -> Self {
    Self(Vec::new())
  }

  pub fn with_capacity(capacity: usize) -> Self {
    Self(Vec::with_capacity(capacity))
  }

  pub const fn is_empty(&self) -> bool {
    self.0.is_empty()
  }

  pub const fn len(&self) -> usize {
    self.0.len()
  }

  pub fn push_expr(&mut self, expr: Expr) -> idx::AstNode {
    let index = idx::AstNode::new(self.0.len() as u32);
    self.0.push(ParseNode::Expression(expr));
    index
  }

  pub fn pop_expr(&mut self) -> Expr {
    debug_assert!(!self.0.is_empty(), "ParseNodes index out of bounds");
    let node = self.0.pop().unwrap();
    match node {
      ParseNode::Expression(expr) => expr,
      ParseNode::Statement(_) => {
        panic!("ParseNodes.pop_expr() found statement instead of expression")
      }
      ParseNode::Declaration(_) => panic!("ParseNodes.pop_expr() found decl instead of expression"),
    }
  }

  pub fn last(&self) -> Option<&Expr> {
    if self.is_empty() {
      None
    } else {
      Some(self.get_expr(idx::AstNode::new((self.len() - 1) as u32)))
    }
  }

  pub fn get_expr(&self, index: idx::AstNode) -> &Expr {
    debug_assert!(
      index.usize() < self.0.len(),
      "ParseNodes index out of bounds"
    );
    let node = &self.0[index.usize()];
    match node {
      ParseNode::Expression(expr) => expr,
      ParseNode::Statement(_) => {
        panic!("ParseNodes.get_expr() found statement instead of expression")
      }
      ParseNode::Declaration(_) => panic!("ParseNodes.get_expr() found decl instead of expression"),
    }
  }

  pub fn get_stmt(&self, index: idx::AstNode) -> &Stmt {
    debug_assert!(
      index.usize() < self.0.len(),
      "ParseNodes index out of bounds"
    );
    let node = &self.0[index.usize()];
    match node {
      ParseNode::Statement(stmt) => stmt,
      ParseNode::Expression(_) => {
        panic!("ParseNodes.get_stmt() found expression instead of statement")
      }
      ParseNode::Declaration(_) => panic!("ParseNodes.get_stmt() found decl instead of statement"),
    }
  }
}
