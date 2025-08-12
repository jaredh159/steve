use crate::internal::*;

#[derive(Debug, PartialEq, Eq)]
pub struct Symbol {
  pub type_id: idx::TypeId,
  pub kind: SymbolKind,
}

#[derive(Debug, PartialEq, Eq)]
pub enum SymbolKind {
  Variable,
  Function,
}

impl Symbol {
  pub fn undefined_var() -> Self {
    Symbol {
      type_id: idx::TypeId::undefined(),
      kind: SymbolKind::Variable,
    }
  }
  pub fn undefined_fn() -> Self {
    Symbol {
      type_id: idx::TypeId::undefined(),
      kind: SymbolKind::Function,
    }
  }
}

#[derive(Debug, PartialEq, Eq)]
pub enum ScopeKind {
  Global,
  Child {
    node: idx::AstNode,
    parent: idx::ScopeId,
  },
}

#[derive(Debug)]
pub struct Scopes(Vec<Scope>);

impl Scopes {
  pub fn with_capacity(capacity: usize) -> Self {
    let mut scopes = Self(Vec::with_capacity(capacity));
    scopes.0.push(Scope {
      kind: ScopeKind::Global,
      symbols: BTreeMap::new(),
    });
    scopes
  }

  pub fn current_mut(&mut self) -> (&mut Scope, idx::ScopeId) {
    assert!(!self.0.is_empty());
    let idx = idx::ScopeId::new((self.0.len() - 1) as u32);
    (self.0.last_mut().unwrap(), idx)
  }

  pub fn push(&mut self, parent: idx::ScopeId) -> idx::ScopeId {
    let idx = idx::ScopeId::new(self.0.len() as u32);
    self.0.push(Scope {
      kind: ScopeKind::Child {
        node: idx::AstNode::undefined(),
        parent,
      },
      symbols: BTreeMap::new(),
    });
    idx
  }

  pub fn set_node(&mut self, scope_idx: idx::ScopeId, node_idx: idx::AstNode) {
    let scope = self.0.get_mut(scope_idx.usize()).unwrap();
    let ScopeKind::Child { ref mut node, .. } = scope.kind else {
      panic!("expected child scope");
    };
    *node = node_idx;
  }

  pub fn lookup(&self, str_idx: idx::StrPool) -> Option<&Symbol> {
    let mut current = self.0.last().unwrap();
    loop {
      if let Some(symbol) = current.symbols.get(&str_idx) {
        return Some(symbol);
      }
      let ScopeKind::Child { ref parent, .. } = current.kind else {
        return None;
      };
      current = self.0.get(parent.usize()).unwrap();
    }
  }

  pub fn as_slice(&self) -> &[Scope] {
    &self.0
  }
}

#[derive(Debug, PartialEq, Eq)]
pub struct Scope {
  pub kind: ScopeKind,
  pub symbols: BTreeMap<idx::StrPool, Symbol>,
}

impl Scope {
  pub fn set_type(&mut self, str_idx: idx::StrPool, type_id: idx::TypeId) {
    let symbol = self.symbols.get_mut(&str_idx).unwrap();
    symbol.type_id = type_id;
  }
}
