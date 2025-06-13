#![allow(dead_code)]
#![allow(unused_variables)]
#![allow(unused_imports)]

use std::collections::BTreeMap;

use crate::internal::*;
use DataNodeKind as N;

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
    while self.cur_ast_node().kind.is_decl() {
      self.visit_cur_node();
    }
    self.ctx
  }

  fn visit_fn_decl(&mut self) {
    let fn_node = self.advance(1);
    assert!(matches!(fn_node.kind, N::FnDecl(_)));
    let ident_node = self.advance(3);
    assert!(matches!(ident_node.kind, N::Ident));
    let str_idx = self.ctx.str_idx(ident_node.token);
    let (outer, outer_idx) = self.cur_scope();
    outer.symbols.insert(str_idx, Symbol::Function);
    let fn_scope = Scope::child(outer_idx);
    // 👍 sat jared: parse fn body
    self.ctx.scopes.push(fn_scope);
  }

  fn cur_scope(&mut self) -> (&mut Scope, idx::ScopeId) {
    assert!(!self.ctx.scopes.is_empty());
    let idx = idx::ScopeId::new((self.ctx.scopes.len() - 1) as u32);
    (self.ctx.scopes.last_mut().unwrap(), idx)
  }

  fn visit_cur_node(&mut self) {
    let node = self.cur_ast_node();
    match node.kind {
      N::FnDecl(_) => self.visit_fn_decl(),
      N::Ident => todo!(),
      N::ReturnStmt => todo!(),
      N::IntLit(int_data) => todo!(),
      N::VarDeclStmt { has_type } => todo!(),
      N::AsciiLit => todo!(),
      N::Type { num_tokens } => todo!(),
      N::ExprStmt => todo!(),
      N::CallExpr { num_args } => todo!(),
      N::MemberAccess { implicit } => todo!(),
      N::PlatformKeyword => todo!(),
      N::ImplicitMemberAccess => todo!(),
    }
  }

  fn cur_ast_node(&self) -> DataNode {
    self.ctx.nodes[self.node_pos].as_ast()
  }

  fn advance(&mut self, n: usize) -> DataNode {
    let node = self.cur_ast_node();
    self.node_pos += n;
    node
  }

  const fn done(&self) -> bool {
    self.node_pos >= self.ctx.nodes.len() - 1
  }
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
    dbg!(ctx
      .nodes
      .clone()
      .into_iter()
      .take(5)
      .map(Node::as_ast)
      .collect::<Vec<_>>());
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
