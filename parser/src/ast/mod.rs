mod data;
pub(crate) mod mem;
pub use data::{AstData, Dissembled};

use crate::internal::*;

#[derive(Debug, PartialEq, Eq)]
pub enum Node {
  Decl(Decl),
  Expr(Expr),
  Stmt(Stmt),
}

#[derive(Debug, PartialEq, Eq)]
pub enum Decl {
  Fn(FnDecl),
  Var(VarDecl),
}

#[derive(Debug, PartialEq, Eq)]
pub enum Expr {
  Ident(Ident),
  IntLit(IntLit),
  MemberAccess(MemberAccess),
}

#[derive(Debug, PartialEq, Eq)]
pub enum Stmt {
  Block(BlockStmt),
  Return(ReturnStmt),
  Expr(ExprStmt),
}

impl Decl {
  pub fn index(&self) -> idx::AstNode {
    match self {
      Decl::Fn(fn_decl) => fn_decl.idx,
      Decl::Var(var_decl) => var_decl.idx,
    }
  }
}

impl Stmt {
  pub fn index(&self) -> idx::AstNode {
    match self {
      Stmt::Block(block_stmt) => block_stmt.idx,
      Stmt::Return(return_stmt) => return_stmt.idx,
      Stmt::Expr(expr_stmt) => expr_stmt.idx,
    }
  }
}

impl Expr {
  pub fn index(&self) -> idx::AstNode {
    match self {
      Expr::Ident(ident) => ident.idx,
      Expr::IntLit(int_lit) => int_lit.idx,
      Expr::MemberAccess(member_access) => member_access.idx,
    }
  }
}

impl Node {
  pub fn index(&self) -> idx::AstNode {
    match self {
      Node::Decl(decl) => decl.index(),
      Node::Expr(expr) => expr.index(),
      Node::Stmt(stmt) => stmt.index(),
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
pub struct ExprStmt {
  pub idx: idx::AstNode,
}

#[derive(Debug, PartialEq, Eq)]
pub struct IntLit {
  pub value: u64,
  pub idx: idx::AstNode,
}

#[derive(Debug, PartialEq, Eq)]
pub struct VarDecl {
  pub has_type: bool,
  pub idx: idx::AstNode,
}

impl VarDecl {
  pub fn name(&self, ctx: &Context) -> Ident {
    let node = ctx.ast_node_at(self.idx + 1).unwrap();
    let Node::Expr(Expr::Ident(ident)) = node else {
      panic!("invalid ast mem data, expected Ident");
    };
    ident
  }
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

  pub fn name(&self, ctx: &Context) -> Ident {
    let node = ctx.ast_node_at(self.idx + 1).unwrap();
    let Node::Expr(Expr::Ident(ident)) = node else {
      panic!("invalid ast mem data, expected Ident");
    };
    ident
  }

  pub fn statements(&self, ctx: &Context) -> BlockStmts {
    // TODO: skip arguments when present
    let ret_annot_idx = self.idx + 2;
    let block_node = ctx.ast_node_after(ret_annot_idx).unwrap();
    let Node::Stmt(Stmt::Block(block_stmt)) = block_node else {
      panic!("invalid ast mem data, expected BlockStmt");
    };

    BlockStmts {
      cur_idx: block_stmt.idx,
      num_stmts: block_stmt.num_stmts,
      progress: 0,
    }
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
  pub fn str_idx(&self, ctx: &Context) -> idx::StrPool {
    ctx.str_idx(self.token)
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
