use crate::ast::*;

#[derive(Debug, PartialEq, Eq)]
pub struct AstNodes(Vec<AstNode>);

impl AstNodes {
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

  pub fn push_expr(&mut self, expr: Expr) -> Index {
    let index = Index::new(self.0.len() as u32);
    self.0.push(AstNode::Expression(expr));
    index
  }

  pub fn pop_expr(&mut self) -> Expr {
    debug_assert!(!self.0.is_empty(), "AstNodes index out of bounds");
    let node = self.0.pop().unwrap();
    match node {
      AstNode::Expression(expr) => expr,
      AstNode::Statement(_) => panic!("AstNodes.pop_expr() found statement instead of expression"),
      AstNode::Declaration(_) => panic!("AstNodes.pop_expr() found decl instead of expression"),
    }
  }

  pub fn last(&self) -> Option<&Expr> {
    if self.is_empty() {
      None
    } else {
      Some(self.get_expr(Index::new((self.len() - 1) as u32)))
    }
  }

  pub fn get_expr(&self, index: Index) -> &Expr {
    debug_assert!(index.usize() < self.0.len(), "AstNodes index out of bounds");
    let node = &self.0[index.usize()];
    match node {
      AstNode::Expression(expr) => expr,
      AstNode::Statement(_) => panic!("AstNodes.get_expr() found statement instead of expression"),
      AstNode::Declaration(_) => panic!("AstNodes.get_expr() found decl instead of expression"),
    }
  }

  pub fn get_stmt(&self, index: Index) -> &Stmt {
    debug_assert!(index.usize() < self.0.len(), "AstNodes index out of bounds");
    let node = &self.0[index.usize()];
    match node {
      AstNode::Statement(stmt) => stmt,
      AstNode::Expression(_) => panic!("AstNodes.get_stmt() found expression instead of statement"),
      AstNode::Declaration(_) => panic!("AstNodes.get_stmt() found decl instead of statement"),
    }
  }
}

#[derive(Clone, Copy, PartialEq, Eq)]
pub struct Index(u32);

impl std::fmt::Debug for Index {
  fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    write!(f, "AstNodes.Index({})", self.0)
  }
}

impl Index {
  pub const fn new(idx: u32) -> Self {
    Self(idx)
  }

  pub const fn undefined() -> Self {
    Self(u32::MAX)
  }

  pub const fn is_undefined(&self) -> bool {
    self.0 == u32::MAX
  }

  pub const fn set(&mut self, idx: u32) {
    self.0 = idx;
  }

  pub const fn usize(&self) -> usize {
    self.0 as usize
  }

  pub const fn decr(&self) -> Self {
    Self(self.0 - 1)
  }
}
