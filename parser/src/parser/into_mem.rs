use super::parse_nodes::{self as parse, ParseNode, ParseNodes};
use crate::ast::mem::{Mem::*, *};
use crate::internal::*;

pub trait IntoAst {
  fn into_ast(self, stack: &mut ParseNodes, nodes: &mut AstData);
}

impl IntoAst for parse::Decl {
  fn into_ast(self, stack: &mut ParseNodes, nodes: &mut AstData) {
    match self {
      parse::Decl::Function {
        token,
        is_pure,
        discardable,
        num_args,
        ..
      } => {
        nodes.push(
          FnDecl(FnDeclData::new(num_args, is_pure, discardable)),
          token,
        );
        let parse::Expr::Ident { token: ident_token } = stack.pop_expr() else {
          panic!("Expected ident on top of ParseNodes stack");
        };
        nodes.push(Ident, ident_token);
        let type_expr = stack.pop_expr();
        type_expr.into_ast(stack, nodes);
      }
    }
  }
}

impl IntoAst for parse::Stmt {
  fn into_ast(self, stack: &mut ParseNodes, nodes: &mut AstData) {
    match self {
      parse::Stmt::Let { token, has_type } => {
        nodes.push(VarDeclStmt { has_type }, token);
        let expr = stack.pop_expr();
        let parse::Expr::Ident { token: ident_token } = stack.pop_expr() else {
          panic!("Expected ident on top of ParseNodes stack");
        };
        nodes.push(Ident, ident_token);
        expr.into_ast(stack, nodes);
      }
      parse::Stmt::Expression { token } => {
        nodes.push(ExprStmt, token);
        let expr = stack.pop_expr();
        expr.into_ast(stack, nodes);
      }
      parse::Stmt::Return { token } => {
        nodes.push(ReturnStmt, token);
        let expr = stack.pop_expr();
        expr.into_ast(stack, nodes);
      }
    }
  }
}

impl IntoAst for parse::Expr {
  fn into_ast(self, stack: &mut ParseNodes, nodes: &mut AstData) {
    match self {
      parse::Expr::AsciiLit { token } => {
        nodes.push(AsciiLit, token);
      }
      parse::Expr::Call { token, num_args } => {
        nodes.push(CallExpr { num_args }, token);
        for _ in 0..num_args {
          let arg = stack.pop_expr();
          arg.into_ast(stack, nodes);
        }

        let expr = stack.pop_expr();
        expr.into_ast(stack, nodes);
      }
      parse::Expr::MemberAccess { token, implicit } => {
        nodes.push(Mem::MemberAccess { implicit }, token);
        let ident = stack.pop_expr();
        if !implicit {
          let receiver = stack.pop_expr();
          receiver.into_ast(stack, nodes);
        }
        ident.into_ast(stack, nodes);
      }
      parse::Expr::Ident { token } => {
        nodes.push(Ident, token);
      }
      parse::Expr::PlatformKeyword { token } => {
        nodes.push(PlatformKeyword, token);
      }
      parse::Expr::IntLit { token, value } => {
        let u14_max = 16383;
        if value <= u14_max {
          nodes.push(
            IntLit(IntData::new(u2::new(0), u14::new(value as u16))),
            token,
          );
        } else {
          panic!("TODO: ints larger than u14::MAX")
        }
      }
    }
  }
}

impl IntoAst for ParseNode {
  fn into_ast(self, stack: &mut ParseNodes, nodes: &mut AstData) {
    match self {
      ParseNode::Statement(stmt) => stmt.into_ast(stack, nodes),
      ParseNode::Expression(expr) => expr.into_ast(stack, nodes),
      ParseNode::Declaration(decl) => decl.into_ast(stack, nodes),
    }
  }
}
