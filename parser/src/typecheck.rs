#![allow(dead_code)]
#![allow(unused_variables)]
#![allow(unused_imports)]

use std::collections::BTreeMap;

use crate::{
  idx::StrPool,
  internal::{idx::TypeId, *},
};

pub struct TypeChecker {
  ctx: Context,
  type_db: Vec<Typ>,
  node_map: BTreeMap<idx::AstNode, TypeId>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct TypeMismatch {
  node: idx::AstNode,
  type_a: Typ,
  type_b: Typ,
}

#[derive(Debug, Clone, PartialEq, Eq)]
enum Typ {
  Concrete(ConcreteType),
  Constraint(TypeConstraint),
  Symlink(TypeId),
  Free(TypeId),
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
enum Prim {
  Bool,
  Void,
  Int { signed: bool, bits: u8 },
}

#[derive(Debug, Clone, PartialEq, Eq)]
enum ConcreteType {
  Primitive(Prim),
  // TODO: shouldn't be special cased, once i know how to handle enums
  Result(TypeId, TypeId),
  // T1, T2, ... -> R
  Function { args: Vec<TypeId>, ret: TypeId },
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
enum TypeConstraint {
  Numeric,
  Integer,
  Float,
}

impl TypeChecker {
  pub const fn new(ctx: Context) -> Self {
    TypeChecker {
      type_db: vec![],
      node_map: BTreeMap::new(),
      ctx,
    }
  }

  #[instrument(skip_all)]
  pub fn check(&mut self) {
    self.push_type(Typ::Concrete(ConcreteType::Primitive(Prim::Void)));
    let mut idx = idx::AstNode::new(0);
    while let Some(node) = self.ctx.ast_node_at(idx) {
      self.visit_node(&node);
      if let Some(next_idx) = self.ctx.ast_index_after(node.index()) {
        idx = next_idx;
      } else {
        break;
      }
    }
  }

  #[instrument(skip_all)]
  fn visit_node(&mut self, node: &Node) -> TypeId {
    match node {
      Node::Decl(Decl::Fn(fn_decl)) => self.visit_fn_decl(fn_decl),
      Node::Expr(Expr::Ident(ident)) => self.visit_ident(ident),
      Node::Stmt(Stmt::Block(block)) => todo!(),
      Node::Stmt(Stmt::Return(ret_stmt)) => self.visit_ret_stmt(ret_stmt),
      Node::Expr(Expr::IntLit(int_lit)) => self.visit_int_lit(int_lit),
      Node::Expr(Expr::MemberAccess(member_access)) => self.visit_member_access(member_access),
    }
  }

  #[instrument(skip_all)]
  fn visit_ret_stmt(&mut self, ret_stmt: &ReturnStmt) -> TypeId {
    let next_node = self.ctx.ast_node_after(ret_stmt.idx).unwrap();
    let type_id = self.visit_node(&next_node);
    self.insert_node(ret_stmt.idx, type_id)
  }

  #[instrument(skip_all)]
  fn visit_int_lit(&mut self, int_lit: &IntLit) -> TypeId {
    let type_id = self.push_type(Typ::Constraint(TypeConstraint::Numeric));
    self.insert_node(int_lit.idx, type_id)
  }

  fn insert_node(&mut self, node_idx: idx::AstNode, type_id: TypeId) -> TypeId {
    self.node_map.insert(node_idx, type_id);
    type_id
  }

  fn resolve_links_and_compress(&mut self, type_id: TypeId) -> TypeId {
    match &self.type_db[type_id.usize()] {
      // type variable, not unified yet
      Typ::Free(type_id) => *type_id,
      // follow symlink with path compression
      Typ::Symlink(type_id) => self.compress(*type_id),
      Typ::Concrete(_) | Typ::Constraint(_) => type_id,
    }
  }

  fn compress(&mut self, type_id: TypeId) -> TypeId {
    let final_type_id = self.resolve_links_and_compress(type_id);
    if final_type_id != type_id {
      self.type_db[type_id.usize()] = Typ::Symlink(final_type_id);
    }
    final_type_id
  }

  #[instrument(skip_all)]
  fn visit_fn_decl(&mut self, fn_decl: &FnDecl) -> TypeId {
    let return_annotation = fn_decl.return_annotation(&self.ctx);
    let return_annotation = self.visit_node(&return_annotation);
    let mut return_type = TypeId::new(0); // void
    let mut err_loc = fn_decl.idx;
    let mut stmts = fn_decl.statements(&self.ctx);
    while let Some(stmt) = stmts.next(&self.ctx) {
      err_loc = stmt.index();
      return_type = self.visit_node(&stmt);
    }
    self.unify(return_annotation, return_type, err_loc);
    let type_id = self.push_type(Typ::Concrete(ConcreteType::Function {
      args: vec![],
      ret: return_annotation,
    }));
    self.insert_node(fn_decl.idx, type_id)
  }

  fn unify(&mut self, a: TypeId, b: TypeId, node_idx: idx::AstNode) -> bool {
    let a = self.resolve_links_and_compress(a);
    let b = self.resolve_links_and_compress(b);
    if a == b {
      return true;
    }
    let a_type = &self.type_db[a.usize()];
    let b_type = &self.type_db[b.usize()];
    match (a_type, b_type) {
      (
        Typ::Concrete(ConcreteType::Primitive(Prim::Bool)),
        Typ::Constraint(TypeConstraint::Numeric),
      ) => {
        self.ctx.type_mismatches.push(TypeMismatch {
          node: node_idx,
          type_a: a_type.clone(),
          type_b: b_type.clone(),
        });
        false
      }
      _ => todo!("unhandled type comb: {:?}, {:?}", a_type, b_type),
    }
  }

  #[instrument(skip_all)]
  fn visit_ident(&mut self, ident: &Ident) -> TypeId {
    match ident.lexeme(&self.ctx) {
      "bool" => self.concrete(ConcreteType::Primitive(Prim::Bool)),
      _ => todo!("other idents!"),
    }
  }

  #[instrument(skip_all)]
  fn visit_member_access(&mut self, member_access: &MemberAccess) -> TypeId {
    // TODO: hardcoding this as `pf.MainReturn` for now
    let ok_id = self.push_type(Typ::Concrete(ConcreteType::Primitive(Prim::Void)));
    let err_id = self.push_type(Typ::Concrete(ConcreteType::Primitive(Prim::Int {
      signed: false,
      bits: 3,
    })));
    self.push_type(Typ::Concrete(ConcreteType::Result(ok_id, err_id)))
  }

  #[instrument(skip_all)]
  fn visit_var_decl(&mut self, has_type: bool) -> TypeId {
    todo!()
  }

  fn concrete(&mut self, concrete: ConcreteType) -> TypeId {
    self.push_type(Typ::Concrete(concrete))
  }

  fn push_type(&mut self, typ: Typ) -> TypeId {
    let id = TypeId::new(self.type_db.len() as u32);
    self.type_db.push(typ);
    id
  }
}

#[cfg(test)]
mod tests {
  use super::*;
  use pretty_assertions::assert_eq;

  fn checker_from(input: &str) -> TypeChecker {
    let parser = Parser::new_str(input);
    let ctx = parser.parse();
    TypeChecker::new(ctx)
  }

  #[test]
  fn typecheck_fn_return_mismatch() {
    let mut checker = checker_from("fn bad() -> bool { 3 }");
    checker.check();
    assert_eq!(
      &checker.ctx.type_mismatches,
      &[TypeMismatch {
        node: idx::AstNode::new(5),
        type_a: Typ::Concrete(ConcreteType::Primitive(Prim::Bool)),
        type_b: Typ::Constraint(TypeConstraint::Numeric),
      }]
    );
  }

  // #[test]
  // fn first_goal_program() {
  //   let input = r#"
  //     rt main() -> pf.MainReturn {
  //       let msg = a"hello steve!";
  //       pf.print(msg);
  //       .err(17)
  //     }"#;
  //   let mut checker = checker_from(input);
  //   checker.check();
  //   dbg!(&checker.type_db);
  // }

  // () -> Result<void, u8>
  // msg = asciistr
  // a"hello steve" = asciistr<u16>
  // pf = platform
  // print = (asciistr) -> Result<void, ???>
  // msg = asciistr  (same...)
  // err = Result.err<E>
  // 17 = UInt<8> (typealias u8)
}
