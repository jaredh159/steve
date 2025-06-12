#![allow(dead_code)]
#![allow(unused_variables)]
#![allow(unused_imports)]

use std::{collections::BTreeMap, iter::once};

use crate::{
  idx::StrPool,
  internal::{idx::TypeId, *},
};

pub struct TypeChecker {
  nodes: Vec<Node>,
  pos: usize,
  type_db: Vec<Typ>,
  node_map: BTreeMap<idx::AstNode, TypeId>,
  strings: StringPool,
  tokens: Vec<Token>,
  errors: Vec<TypeMismatch>,
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
  pub const fn new(nodes: Vec<Node>, strings: StringPool, tokens: Vec<Token>) -> Self {
    TypeChecker {
      pos: 0,
      type_db: vec![],
      node_map: BTreeMap::new(),
      errors: vec![],
      nodes,
      strings,
      tokens,
    }
  }

  pub fn check(&mut self) {
    self.push_type(Typ::Concrete(ConcreteType::Primitive(Prim::Void)));
    while self.cur_ast_node().kind.is_decl() {
      self.visit_cur_node();
    }
  }

  fn visit_cur_node(&mut self) -> TypeId {
    let node = self.cur_ast_node();
    match node.kind {
      DataNodeKind::FnDecl(data) => self.visit_fn_decl(data),
      DataNodeKind::Ident => self.visit_ident(node.token),
      DataNodeKind::ReturnStmt => self.visit_ret_stmt(),
      DataNodeKind::IntLit(int_data) => self.visit_int_lit(),
      DataNodeKind::VarDeclStmt { has_type } => self.visit_var_decl(has_type),
      DataNodeKind::AsciiLit => todo!(),
      DataNodeKind::Type { num_tokens } => todo!(),
      DataNodeKind::ExprStmt => todo!(),
      DataNodeKind::CallExpr { num_args } => todo!(),
      DataNodeKind::MemberAccess { implicit } => self.visit_member_access(implicit),
      DataNodeKind::PlatformKeyword => todo!(),
      DataNodeKind::ImplicitMemberAccess => todo!(),
    }
  }

  fn visit_next_node(&mut self) -> TypeId {
    self.pos += 1;
    self.visit_cur_node()
  }

  fn visit_ret_stmt(&mut self) -> TypeId {
    let node_idx = self.node_idx();
    let type_id = self.visit_next_node();
    self.insert_node(node_idx, type_id)
  }

  fn visit_int_lit(&mut self) -> TypeId {
    let node_idx = self.node_idx();
    let type_id = self.push_type(Typ::Constraint(TypeConstraint::Numeric));
    self.insert_node(node_idx, type_id)
  }

  fn insert_node(&mut self, node_idx: idx::AstNode, type_id: TypeId) -> TypeId {
    self.node_map.insert(node_idx, type_id);
    type_id
  }

  const fn node_idx(&self) -> idx::AstNode {
    idx::AstNode::new(self.pos as u32)
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

  fn visit_fn_decl(&mut self, data: FnDeclData) -> TypeId {
    let fn_node_idx = self.node_idx();
    self.pos += 2; // skip fn decl, ident
    let return_annotation = self.visit_cur_node();
    dbg!(self.pos);
    let mut return_type = TypeId::new(0); // void
    while self.cur_ast_node().kind.is_stmt() {
      return_type = self.visit_cur_node();
    }
    self.unify(return_annotation, return_type, self.node_idx());
    let type_id = self.push_type(Typ::Concrete(ConcreteType::Function {
      args: vec![],
      ret: return_annotation,
    }));
    self.insert_node(fn_node_idx, type_id)
  }

  fn visit_member_access(&mut self, implicit: bool) -> TypeId {
    let ok_id = self.push_type(Typ::Concrete(ConcreteType::Primitive(Prim::Void)));
    let err_id = self.push_type(Typ::Concrete(ConcreteType::Primitive(Prim::Int {
      signed: false,
      bits: 3,
    })));
    self.pos += 3;
    // TODO: hardcoding this as `pf.MainReturn` for now
    self.push_type(Typ::Concrete(ConcreteType::Result(ok_id, err_id)))
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
        self.errors.push(TypeMismatch {
          node: node_idx,
          type_a: a_type.clone(),
          type_b: b_type.clone(),
        });
        false
      }
      _ => todo!("unhandled type comb: {:?}, {:?}", a_type, b_type),
    }
  }

  fn cur_ast_node(&self) -> DataNode {
    self.nodes[self.pos].as_ast()
  }

  fn visit_ident(&mut self, token: u32) -> TypeId {
    let token = self.tokens[token as usize];
    let lexeme = token.lexeme(&self.strings);
    self.pos += 1; // move past ident
    match lexeme {
      "bool" => self.concrete(ConcreteType::Primitive(Prim::Bool)),
      _ => todo!("other idents!"),
    }
  }

  fn visit_var_decl(&mut self, has_type: bool) -> TypeId {
    todo!()
  }

  fn concrete(&mut self, concrete: ConcreteType) -> TypeId {
    self.push_type(Typ::Concrete(concrete))
  }

  const fn done(&self) -> bool {
    self.pos >= self.nodes.len() - 1
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
    let parsed = parser.parse();
    let nodes = parsed.result.unwrap();
    dbg!(nodes
      .clone()
      .into_iter()
      .take(5)
      .map(Node::as_ast)
      .collect::<Vec<_>>());
    TypeChecker::new(nodes, parsed.strings, parsed.tokens)
  }

  #[test]
  fn fn_return_mismatch() {
    let mut checker = checker_from("fn bad() -> bool { 3 }");
    checker.check();
    assert_eq!(
      &checker.errors,
      &[TypeMismatch {
        node: idx::AstNode::new(4),
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
