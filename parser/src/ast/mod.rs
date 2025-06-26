mod data;
pub(crate) mod mem;
pub use data::{AstData, Dissembled};

use crate::internal::*;

#[derive(Debug, PartialEq, Eq)]
pub enum Node {
  FnDecl(FnDecl),
  Ident(Ident),
  BlockStmt(BlockStmt),
  ReturnStmt(ReturnStmt),
  IntLit(IntLit),
  MemberAccess(MemberAccess),
}

impl Node {
  pub fn index(&self) -> idx::AstNode {
    match self {
      Node::FnDecl(fn_decl) => fn_decl.idx,
      Node::Ident(ident) => ident.idx,
      Node::BlockStmt(block_stmt) => block_stmt.idx,
      Node::ReturnStmt(return_stmt) => return_stmt.idx,
      Node::IntLit(int_lit) => int_lit.idx,
      Node::MemberAccess(member_access) => member_access.idx,
    }
  }
}

#[derive(Debug, PartialEq, Eq)]
pub struct BlockStmt {
  pub num_stmts: u16,
  pub idx: idx::AstNode,
}

#[derive(Debug, PartialEq, Eq)]
pub struct MemberAccess {
  pub implicit: bool,
  pub idx: idx::AstNode,
}

#[derive(Debug, PartialEq, Eq)]
pub struct ReturnStmt {
  pub idx: idx::AstNode,
}

#[derive(Debug, PartialEq, Eq)]
pub struct IntLit {
  pub value: u64,
  pub idx: idx::AstNode,
}

#[derive(Debug, PartialEq, Eq)]
pub struct FnDecl {
  pub num_args: u8,
  pub is_pure: bool,
  pub discardable: bool,
  pub idx: idx::AstNode,
}

impl FnDecl {
  pub fn return_annotation(&self, ctx: &Context) -> Node {
    // TODO: skip arguments when present
    ctx.ast_node_at(self.idx + 2).unwrap()
  }

  pub fn statements(&self, ctx: &Context) -> BlockStmts {
    // TODO: skip arguments when present
    let ret_annot_idx = self.idx + 2;
    let block_node = ctx.ast_node_after(ret_annot_idx).unwrap();
    let Node::BlockStmt(block_stmt) = block_node else {
      panic!("invalid ast mem data, expected BlockStmt");
    };

    BlockStmts {
      cur_idx: block_stmt.idx,
      num_stmts: block_stmt.num_stmts,
      progress: 0,
    }
  }
}

#[derive(Debug)]
pub struct BlockStmts {
  cur_idx: idx::AstNode,
  num_stmts: u16,
  progress: u16,
}

impl BlockStmts {
  pub fn next(&mut self, ctx: &Context) -> Option<Node> {
    if self.num_stmts == self.progress {
      return None;
    } else if self.progress == 0 {
      self.cur_idx += 2; // skip past index of next node
    } else {
      self.cur_idx = ctx.ast_index_after(self.cur_idx).unwrap();
    }
    self.progress += 1;
    ctx.ast_node_at(self.cur_idx)
  }
}

#[derive(Debug, PartialEq, Eq)]
pub struct Ident {
  pub token: u32,
  pub idx: idx::AstNode,
}

impl Ident {
  pub fn lexeme<'a>(&self, ctx: &'a Context) -> &'a str {
    let token = ctx.tokens[self.token as usize];
    token.lexeme(&ctx.strs)
  }
}
