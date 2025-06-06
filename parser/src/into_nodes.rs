use crate::internal::{DataNodeKind::*, *};

pub trait IntoNodes {
  fn into_nodes(self, stack: &mut AstNodes, nodes: &mut Vec<Node>);
}

impl IntoNodes for Decl {
  fn into_nodes(self, stack: &mut AstNodes, nodes: &mut Vec<Node>) {
    match self {
      Decl::Function {
        token,
        is_pure,
        discardable,
        num_args,
      } => {
        nodes.push(Node::ast(
          FnDecl(FnDeclData::new(num_args, is_pure, discardable)),
          token,
        ));
        let Expr::Ident { token: ident_token } = stack.pop_expr() else {
          panic!("Expected ident on top of AstNodes stack");
        };
        nodes.push(Node::ast(Ident, ident_token));
        let type_expr = stack.pop_expr();
        type_expr.into_nodes(stack, nodes);
      }
    }
  }
}

impl IntoNodes for Stmt {
  fn into_nodes(self, stack: &mut AstNodes, nodes: &mut Vec<Node>) {
    match self {
      Stmt::Let { token, has_type } => {
        nodes.push(Node::ast(VarDeclStmt { has_type }, token));
        let expr = stack.pop_expr();
        let Expr::Ident { token: ident_token } = stack.pop_expr() else {
          panic!("Expected ident on top of AstNodes stack");
        };
        nodes.push(Node::ast(Ident, ident_token));
        expr.into_nodes(stack, nodes);
      }
      Stmt::Expression { token } => {
        nodes.push(Node::ast(ExprStmt, token));
        let expr = stack.pop_expr();
        expr.into_nodes(stack, nodes);
      }
      Stmt::Return { token } => {
        nodes.push(Node::ast(ReturnStmt, token));
        let expr = stack.pop_expr();
        expr.into_nodes(stack, nodes);
      }
    }
  }
}

impl IntoNodes for Expr {
  fn into_nodes(self, stack: &mut AstNodes, nodes: &mut Vec<Node>) {
    use DataNodeKind::*;
    match self {
      Expr::AsciiLit { token } => nodes.push(Node::ast(AsciiLit, token)),
      Expr::CallExpr { token, num_args } => {
        nodes.push(Node::ast(CallExpr { num_args }, token));
        for _ in 0..num_args {
          let arg = stack.pop_expr();
          arg.into_nodes(stack, nodes);
        }

        let expr = stack.pop_expr();
        expr.into_nodes(stack, nodes);
      }
      Expr::MemberAccess { token, implicit } => {
        nodes.push(Node::ast(MemberAccess { implicit }, token));
        let ident = stack.pop_expr();
        if !implicit {
          let receiver = stack.pop_expr();
          receiver.into_nodes(stack, nodes);
        }
        ident.into_nodes(stack, nodes);
      }
      Expr::Type { token, num_tokens } => nodes.push(Node::ast(Type { num_tokens }, token)),
      Expr::Ident { token } => nodes.push(Node::ast(Ident, token)),
      Expr::PlatformKeyword { token } => nodes.push(Node::ast(PlatformKeyword, token)),
      Expr::IntLit { token, value } => {
        let u14_max = 16383;
        if value <= u14_max {
          nodes.push(Node::ast(
            IntLit(IntData::new(u2::new(0), u14::new(value as u16))),
            token,
          ));
        } else {
          panic!("TODO: ints larger than u14::MAX")
        }
      }
    }
  }
}

impl IntoNodes for AstNode {
  fn into_nodes(self, stack: &mut AstNodes, nodes: &mut Vec<Node>) {
    match self {
      AstNode::Statement(stmt) => stmt.into_nodes(stack, nodes),
      AstNode::Expression(expr) => expr.into_nodes(stack, nodes),
      AstNode::Declaration(decl) => decl.into_nodes(stack, nodes),
    }
  }
}
