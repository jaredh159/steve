use crate::{ast_nodes::*, node::*};

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

#[derive(Debug, PartialEq, Eq)]
pub enum Stmt {
  Let { token: u32, has_type: bool },
  Expression { token: u32 },
  Return { token: u32 },
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum Expr {
  CallExpr { token: u32, num_args: u8 },
  MemberAccess { token: u32, implicit: bool },
  Ident { token: u32 },
  AsciiLit { token: u32 },
  PlatformKeyword { token: u32 },
}

#[derive(Debug, PartialEq, Eq)]
pub enum AstNode {
  Expression(Expr),
  Statement(Stmt),
}

impl Stmt {
  pub fn into_nodes(&self, stack: &mut AstNodes, nodes: &mut Vec<DataNode>) {
    use DataNodeKind::*;
    match self {
      Stmt::Let { token, has_type } => {
        nodes.push(DataNode {
          kind: VarDeclStmt { has_type: *has_type },
          token: *token,
        });
        let expr = stack.pop_expr();
        let Expr::Ident { token: ident_token } = stack.pop_expr() else {
          panic!("Expected ident on top of AstNodes stack");
        };
        nodes.push(DataNode { kind: Ident, token: ident_token });
        expr.into_nodes(stack, nodes);
      }
      Stmt::Expression { token } => {
        nodes.push(DataNode { kind: ExprStmt, token: *token });
        let expr = stack.pop_expr();
        expr.into_nodes(stack, nodes);
      }
      Stmt::Return { .. } => todo!(),
    }
  }
}

impl Expr {
  pub fn into_nodes(&self, stack: &mut AstNodes, nodes: &mut Vec<DataNode>) {
    use DataNodeKind::*;
    match self {
      Expr::AsciiLit { token } => nodes.push(DataNode { kind: AsciiLit, token: *token }),
      Expr::CallExpr { token, num_args } => {
        nodes.push(DataNode {
          kind: CallExpr { num_args: *num_args },
          token: *token,
        });
        for _ in 0..*num_args {
          let arg = stack.pop_expr();
          arg.into_nodes(stack, nodes);
        }

        let expr = stack.pop_expr();
        expr.into_nodes(stack, nodes);
      }
      Expr::MemberAccess { token, implicit } => {
        let ident = stack.pop_expr();
        let receiver = stack.pop_expr();
        nodes.push(DataNode {
          kind: MemberAccess { implicit: *implicit },
          token: *token,
        });
        receiver.into_nodes(stack, nodes);
        ident.into_nodes(stack, nodes);
      }
      Expr::Ident { token } => nodes.push(DataNode { kind: Ident, token: *token }),
      Expr::PlatformKeyword { token } => {
        nodes.push(DataNode { kind: PlatformKeyword, token: *token })
      }
    }
  }
}

impl AstNode {
  pub fn into_nodes(&self, stack: &mut AstNodes, nodes: &mut Vec<DataNode>) {
    match self {
      AstNode::Statement(stmt) => stmt.into_nodes(stack, nodes),
      AstNode::Expression(_) => todo!(),
    }
  }
}
