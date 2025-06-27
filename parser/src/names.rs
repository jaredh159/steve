use std::collections::BTreeMap;

use crate::internal::*;

#[derive(Debug)]
pub struct Resolver {
  ctx: Context,
}

impl Resolver {
  pub const fn new(ctx: Context) -> Resolver {
    Resolver { ctx }
  }

  pub fn resolve(mut self) -> Context {
    self.ctx.scopes.push(Scope::global());
    let mut idx = idx::AstNode::new(0);
    while let Some(node) = self.ctx.ast_node_at(idx) {
      self.visit_node(&node);
      if let Some(next_idx) = self.ctx.ast_index_after(node.index()) {
        idx = next_idx;
      } else {
        break;
      }
    }
    self.ctx.reset()
  }

  #[instrument(skip_all)]
  fn visit_node(&mut self, node: &Node) {
    match node {
      Node::Decl(Decl::Fn(fn_decl)) => self.visit_fn_decl(fn_decl),
      Node::Decl(Decl::Var(var_decl)) => self.visit_var_decl(var_decl),
      _ => {} // we only care about nodes that declare names
    }
  }

  #[instrument(skip_all)]
  fn visit_fn_decl(&mut self, fn_decl: &FnDecl) {
    let name = fn_decl.name(&self.ctx);
    let str_idx = name.str_idx(&self.ctx);
    let (outer, outer_idx) = self.cur_scope();
    outer.symbols.insert(str_idx, Symbol::Function);
    let fn_scope = Scope::child(outer_idx);
    self.ctx.scopes.push(fn_scope);

    let mut stmts = fn_decl.statements(&self.ctx);
    while let Some(stmt) = stmts.next(&self.ctx) {
      self.visit_node(&stmt);
    }
  }

  #[instrument(skip_all)]
  fn visit_var_decl(&mut self, var_decl: &VarDecl) {
    let name = var_decl.name(&self.ctx);
    let str_idx = name.str_idx(&self.ctx);
    let (scope, _) = self.cur_scope();
    scope.symbols.insert(str_idx, Symbol::Variable);
  }

  fn cur_scope(&mut self) -> (&mut Scope, idx::ScopeId) {
    assert!(!self.ctx.scopes.is_empty());
    let idx = idx::ScopeId::new((self.ctx.scopes.len() - 1) as u32);
    (self.ctx.scopes.last_mut().unwrap(), idx)
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
          symbols: BTreeMap::from([(idx::StrPool::new(3), Symbol::Variable)])
        }
      ]
    )
  }
}
