#![allow(dead_code)]
#![allow(unused_variables)]
#![allow(unused_imports)]

use std::collections::BTreeMap;

use crate::internal::*;

#[derive(Debug)]
pub struct Resolver {
  ctx: Context,
  node_pos: usize,
}

impl Resolver {
  pub const fn new(ctx: Context) -> Resolver {
    Resolver { ctx, node_pos: 0 }
  }

  pub fn resolve(mut self) -> Context {
    self.ctx.scopes.push(Scope::global());
    // while let Some(ast_node) = self.ctx.nodes.next_node() {
    //   match ast_node {
    //     AstNode::Declaration(Decl::Function {
    //       num_args,
    //       // first_arg,
    //       // num_stmts,
    //       // first_stmt,
    //       token,
    //       // name,
    //       is_pure,
    //       discardable,
    //     }) => {
    //       todo!()
    //       // let str_idx = self.ctx.str_idx(name);
    //       // let (outer, outer_idx) = self.cur_scope();
    //       // outer.symbols.insert(str_idx, Symbol::Function);
    //       // let fn_scope = Scope::child(outer_idx);
    //       // // 👍 sat jared: parse fn body
    //       // self.ctx.scopes.push(fn_scope);
    //     }
    //     AstNode::Expression(expr) => todo!(),
    //     AstNode::Statement(stmt) => todo!(),
    //   }
    // }
    // while self.ctx.nodes.current().kind.is_decl() {
    //   self.visit_cur_node();
    // }
    self.ctx.reset()

    // ac539f44-563c-485f-98cd-caf4fc323e69
  }

  // fn visit_fn_decl(&mut self, data: FnDeclData) {
  //   self.ctx.nodes.incr(1);
  //   let ident_node = self.ctx.nodes.incr(3);
  //   assert!(matches!(ident_node.kind, N::Ident));
  //   let str_idx = self.ctx.str_idx(ident_node.token);
  //   let (outer, outer_idx) = self.cur_scope();
  //   outer.symbols.insert(str_idx, Symbol::Function);
  //   let fn_scope = Scope::child(outer_idx);
  //   // 👍 sat jared: parse fn body
  //   self.ctx.scopes.push(fn_scope);
  // }

  // fn cur_scope(&mut self) -> (&mut Scope, idx::ScopeId) {
  //   assert!(!self.ctx.scopes.is_empty());
  //   let idx = idx::ScopeId::new((self.ctx.scopes.len() - 1) as u32);
  //   (self.ctx.scopes.last_mut().unwrap(), idx)
  // }

  // fn visit_cur_node(&mut self) {
  //   let node = self.ctx.nodes.current();
  //   match node.kind {
  //     N::FnDecl(fn_data) => self.visit_fn_decl(fn_data),
  //     N::Ident => todo!(),
  //     N::ReturnStmt => todo!(),
  //     N::IntLit(int_data) => todo!(),
  //     N::VarDeclStmt { has_type } => todo!(),
  //     N::AsciiLit => todo!(),
  //     N::ExprStmt => todo!(),
  //     N::CallExpr { num_args } => todo!(),
  //     N::MemberAccess { implicit } => todo!(),
  //     N::PlatformKeyword => todo!(),
  //   }
  // }
}

#[derive(Debug, PartialEq, Eq)]
pub enum Symbol {
  Variable,
  Function,
}

#[derive(Debug, PartialEq, Eq)]
pub struct Scope {
  parent: Option<idx::ScopeId>,
  symbols: BTreeMap<idx::StrPool, Symbol>,
}

impl Scope {
  pub const fn global() -> Scope {
    Scope {
      parent: None,
      symbols: BTreeMap::new(),
    }
  }

  pub fn child(parent: idx::ScopeId) -> Scope {
    Scope {
      parent: Some(parent),
      symbols: BTreeMap::new(),
    }
  }
}

#[cfg(test)]
mod tests {
  use super::*;

  #[test]
  fn first_goal_program() {
    let input = r#"
      rt main() -> pf.MainReturn {
        let msg = a"hello steve!";
        pf.print(msg);
        .err(17)
      }"#;
    let parser = Parser::new_str(input);
    let ctx = parser.parse();
    // ctx.nodes.dbg_n(5);
    // dbg!(ctx
    //   .nodes
    //   .clone()
    //   .into_iter()
    //   .take(5)
    //   .map(NodeUnion::as_ast)
    //   .collect::<Vec<_>>());
    let ctx = Resolver::new(ctx).resolve();
    assert_eq!(
      &ctx.scopes,
      &[
        Scope {
          parent: None,
          symbols: BTreeMap::from([(idx::StrPool::new(1), Symbol::Function)])
        },
        Scope {
          parent: Some(idx::ScopeId::new(0)),
          symbols: BTreeMap::from([(idx::StrPool::new(7), Symbol::Variable)])
        }
      ]
    )
  }
}
