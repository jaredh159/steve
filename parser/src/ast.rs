use bilge::prelude::*;

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
  IntLit { token: u32, value: u64 },
  PlatformKeyword { token: u32 },
}

#[derive(Debug, PartialEq, Eq)]
pub enum AstNode {
  Expression(Expr),
  Statement(Stmt),
}

impl Stmt {
  pub fn into_nodes(&self, stack: &mut AstNodes, nodes: &mut Vec<Node>) {
    use DataNodeKind::*;
    match self {
      Stmt::Let { token, has_type } => {
        nodes.push(Node::ast(VarDeclStmt { has_type: *has_type }, *token));
        let expr = stack.pop_expr();
        let Expr::Ident { token: ident_token } = stack.pop_expr() else {
          panic!("Expected ident on top of AstNodes stack");
        };
        nodes.push(Node::ast(Ident, ident_token));
        expr.into_nodes(stack, nodes);
      }
      Stmt::Expression { token } => {
        nodes.push(Node::ast(ExprStmt, *token));
        let expr = stack.pop_expr();
        expr.into_nodes(stack, nodes);
      }
      Stmt::Return { token } => {
        nodes.push(Node::ast(ReturnStmt, *token));
        let expr = stack.pop_expr();
        expr.into_nodes(stack, nodes);
      }
    }
  }
}

impl Expr {
  pub fn into_nodes(&self, stack: &mut AstNodes, nodes: &mut Vec<Node>) {
    use DataNodeKind::*;
    match self {
      Expr::AsciiLit { token } => nodes.push(Node::ast(AsciiLit, *token)),
      Expr::CallExpr { token, num_args } => {
        nodes.push(Node::ast(CallExpr { num_args: *num_args }, *token));
        for _ in 0..*num_args {
          let arg = stack.pop_expr();
          arg.into_nodes(stack, nodes);
        }

        let expr = stack.pop_expr();
        expr.into_nodes(stack, nodes);
      }
      Expr::MemberAccess { token, implicit } => {
        nodes.push(Node::ast(MemberAccess { implicit: *implicit }, *token));
        let ident = stack.pop_expr();
        if !*implicit {
          let receiver = stack.pop_expr();
          receiver.into_nodes(stack, nodes);
        }
        ident.into_nodes(stack, nodes);
      }
      Expr::Ident { token } => nodes.push(Node::ast(Ident, *token)),
      Expr::PlatformKeyword { token } => nodes.push(Node::ast(PlatformKeyword, *token)),
      Expr::IntLit { token, value } => {
        let u14_max = 16383;
        if *value <= u14_max {
          nodes.push(Node::ast(
            IntLit(IntData::new(u2::new(0), u14::new(*value as u16))),
            *token,
          ));
        } else {
          panic!("TODO: ints larger than u14::MAX")
        }
      }
    }
  }
}

impl AstNode {
  pub fn into_nodes(&self, stack: &mut AstNodes, nodes: &mut Vec<Node>) {
    match self {
      AstNode::Statement(stmt) => stmt.into_nodes(stack, nodes),
      AstNode::Expression(_) => todo!(),
    }
  }
}
