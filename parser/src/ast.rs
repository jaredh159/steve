use crate::node::*;

#[derive(Debug)]
pub struct Module {
  pub kind: ModuleKind,
  pub platform: PlatformSpec,
  pub alloc: AllocModel,
  pub std: bool,
}

#[derive(Debug)]
pub struct PlatformSpec {
  // Fields for platform specification
}

#[derive(Debug, PartialEq, Eq)]
pub enum ModuleKind {
  Bin(Vec<DataNode>),
  Lib, // export map of some sort
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum AllocModel {
  Implicit,
  Explicit,
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

// pub enum Statement {
//   Let(Token, Identifier, Expr),
//   Return(Token, Expr),
//   Expression(Token, Expr),
//   Block(BlockStatement),
// }

#[derive(Debug, PartialEq, Eq)]
pub enum Stmt {
  Let { token: u32, has_type: bool },
  Expression { token: u32 },
  Return { token: u32, expr: Index },
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum Expr {
  CallExpr { token: u32, num_args: u8 },
  FieldAccess { token: u32 },
  Ident { token: u32 },
  AsciiLit { token: u32 },
  PlatformKeyword { token: u32 },
}

#[derive(Debug, PartialEq, Eq)]
pub enum AstNode {
  Expression(Expr),
  Statement(Stmt),
}

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
    }
  }

  pub fn get_stmt(&self, index: Index) -> &Stmt {
    debug_assert!(index.usize() < self.0.len(), "AstNodes index out of bounds");
    let node = &self.0[index.usize()];
    match node {
      AstNode::Statement(stmt) => stmt,
      AstNode::Expression(_) => panic!("AstNodes.get_stmt() found expression instead of statement"),
    }
  }
}

impl Stmt {
  pub fn into_data_nodes(&self, stack: &mut AstNodes, data_nodes: &mut Vec<DataNode>) {
    use DataNodeKind::*;
    match self {
      Stmt::Let { token, has_type } => {
        data_nodes.push(DataNode {
          kind: VarDeclStmt(VarDecl_D::new(*has_type)),
          token: *token,
        });
        let expr = stack.pop_expr();
        let Expr::Ident { token: ident_token } = stack.pop_expr() else {
          panic!("Expected ident on top of AstNodes stack");
        };
        data_nodes.push(DataNode { kind: Ident, token: ident_token });
        expr.into_data_nodes(stack, data_nodes);
      }
      Stmt::Expression { token } => {
        data_nodes.push(DataNode { kind: ExprStmt, token: *token });
        let expr = stack.pop_expr();
        expr.into_data_nodes(stack, data_nodes);
      }
      Stmt::Return { .. } => todo!(),
    }
  }
}

impl Expr {
  pub fn into_data_nodes(&self, stack: &mut AstNodes, data_nodes: &mut Vec<DataNode>) {
    use DataNodeKind::*;
    match self {
      Expr::AsciiLit { token } => data_nodes.push(DataNode { kind: AsciiLit, token: *token }),
      Expr::CallExpr { token, num_args } => {
        data_nodes.push(DataNode {
          kind: CallExpr(*num_args),
          token: *token,
        });
        for _ in 0..*num_args {
          let arg = stack.pop_expr();
          arg.into_data_nodes(stack, data_nodes);
        }

        let expr = stack.pop_expr();
        expr.into_data_nodes(stack, data_nodes);
      }
      Expr::FieldAccess { token } => {
        let ident = stack.pop_expr();
        let receiver = stack.pop_expr();
        data_nodes.push(DataNode { kind: FieldAccess, token: *token });
        receiver.into_data_nodes(stack, data_nodes);
        ident.into_data_nodes(stack, data_nodes);
      }
      Expr::Ident { token } => data_nodes.push(DataNode { kind: Ident, token: *token }),
      Expr::PlatformKeyword { token } => {
        data_nodes.push(DataNode { kind: PlatformKeyword, token: *token })
      }
    }
  }
}

impl AstNode {
  pub fn into_data_nodes(&self, stack: &mut AstNodes, data_nodes: &mut Vec<DataNode>) {
    match self {
      AstNode::Statement(stmt) => stmt.into_data_nodes(stack, data_nodes),
      AstNode::Expression(_) => todo!(),
    }
  }
}
