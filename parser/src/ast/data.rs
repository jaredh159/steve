use super::{mem::*, Node, *};

#[derive(Debug)]
pub struct AstData {
  elems: Vec<Element>,
  pos: usize,
}

impl AstData {
  pub fn push(&mut self, ast_node: Mem, token: u32) -> usize {
    let idx = self.elems.len();
    self.elems.push(Element::ast(ast_node, token));
    idx
  }

  pub fn with_capacity(capacity: usize) -> Self {
    Self {
      elems: Vec::with_capacity(capacity),
      pos: 0,
    }
  }

  pub fn get_mut(&mut self, index: usize) -> Option<&mut Element> {
    self.elems.get_mut(index)
  }

  pub fn get(&self, index: usize) -> Option<&Element> {
    self.elems.get(index)
  }

  pub fn reset(&mut self) {
    self.pos = 0;
  }

  pub fn len(&self) -> usize {
    self.elems.len()
  }

  pub fn is_empty(&self) -> bool {
    self.elems.is_empty()
  }

  pub const fn done(&self) -> bool {
    self.pos >= self.elems.len().saturating_sub(1)
  }

  pub fn index_after(&self, index: idx::AstNode) -> Option<idx::AstNode> {
    let selected = self.elems[index.usize()].as_ast();
    let next = match selected.node {
      Mem::ReturnStmt | Mem::Ident | Mem::PlatformKeyword => index + 1,
      Mem::FnDecl(fn_decl) => {
        // TODO: skip fn args
        let ret_annot = self.index_after(index + 1).unwrap();
        let block_stmt = self.index_after(ret_annot).unwrap();
        self.elems[block_stmt.usize() + 1].as_next()
      }
      Mem::IntLit(int_data) => todo!(),
      Mem::VarDeclStmt { has_type } => todo!(),
      Mem::AsciiLit => todo!(),
      Mem::ExprStmt => todo!(),
      Mem::Block { num_stmts } => todo!(),
      Mem::CallExpr { num_args } => todo!(),
      Mem::MemberAccess { implicit } => todo!(),
      Mem::Fixup => todo!(),
    };
    if next.usize() >= self.elems.len() {
      None
    } else {
      Some(next)
    }
  }

  pub fn node_after(&self, index: idx::AstNode) -> Option<Node> {
    self.node_at(self.index_after(index)?)
  }

  pub fn node_at(&self, idx: idx::AstNode) -> Option<Node> {
    if idx.usize() >= self.elems.len() {
      return None;
    }
    let selected = self.elems[idx.usize()].as_ast();
    let node = match selected.node {
      Mem::FnDecl(data) => Node::FnDecl(FnDecl {
        num_args: data.num_args(),
        is_pure: data.is_pure(),
        discardable: data.discardable(),
        idx,
      }),
      Mem::Ident => Node::Ident(Ident { token: selected.token, idx }),
      Mem::VarDeclStmt { has_type } => todo!(),
      Mem::AsciiLit => todo!(),
      Mem::IntLit(int_data) => Node::IntLit(IntLit {
        value: u64::from(int_data.payload()),
        idx,
      }),
      Mem::Block { num_stmts: 0 } => Node::BlockStmt(BlockStmt { num_stmts: 0, idx }),
      Mem::Block { num_stmts } => Node::BlockStmt(BlockStmt { num_stmts, idx }),
      Mem::ExprStmt => todo!(),
      Mem::CallExpr { num_args } => todo!(),
      Mem::MemberAccess { implicit } => Node::MemberAccess(MemberAccess { implicit, idx }),
      Mem::PlatformKeyword => todo!(),
      Mem::ReturnStmt => Node::ReturnStmt(ReturnStmt { idx }),
      Mem::Fixup => panic!(),
    };
    Some(node)
  }

  pub fn dissemble(&self) -> Vec<Dissembled> {
    #[derive(PartialEq, Eq)]
    enum Next {
      Node,
      Int,
      Index,
    }
    let mut dissembled = Vec::with_capacity(self.elems.len());
    let mut next = Next::Node;
    for i in 0..self.elems.len() {
      next = match next {
        Next::Int => {
          dissembled.push(Dissembled::Int(self.elems[i].as_int()));
          Next::Node
        }
        Next::Index => {
          dissembled.push(Dissembled::Idx(self.elems[i].as_next()));
          Next::Node
        }
        Next::Node => {
          let tok_node = self.elems[i].as_ast();
          dissembled.push(Dissembled::Node(tok_node));
          match tok_node.node {
            Mem::Block { .. } => Next::Index,
            _ => Next::Node,
          }
        }
      };
    }
    dissembled
  }
}

impl std::iter::IntoIterator for AstData {
  type Item = Element;
  type IntoIter = std::vec::IntoIter<Element>;
  fn into_iter(self) -> Self::IntoIter {
    self.elems.into_iter()
  }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Dissembled {
  Node(TokenNode),
  Int(u64),
  Idx(idx::AstNode),
}
