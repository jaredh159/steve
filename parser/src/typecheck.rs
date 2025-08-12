use crate::idx::TypeId;
use crate::internal::*;

pub struct TypeChecker {
  ctx: Context,
  type_db: Vec<Typ>,
  node_map: BTreeMap<idx::AstNode, TypeId>,
  stack: Vec<TypeCtx>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
enum Typ {
  Void,
  Bool,
  Int { signed: bool, bits: u8 },
  IntLit,
  Function { args: Vec<TypeId>, ret: TypeId },
  // TODO: shouldn't be special cased, once i know how to handle enums
  Result(TypeId, TypeId),
  Deferred,
  Symlink(TypeId),
  AsciiLit,
  AsciiStr,
  UnresolvedMemberAccess(Ident),
  Err,
  Tmp,
}

#[derive(Debug, Clone, PartialEq, Eq)]
enum TypeCtx {
  Function(TypeId),
  Return,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum TypeErr {
  NotCallable(TypeId),
  Arity,
  VarNotFound(idx::StrPool),
  NoMember(TypeId, Ident),
  UnresolvableMemberAccess(Ident),
  Mismatch { expected: TypeId, found: TypeId },
}

impl TypeChecker {
  pub fn check(&mut self) {
    self.push_type(Typ::Void);
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
      Node::Decl(Decl::Var(var_decl)) => self.visit_var_decl(var_decl),
      Node::Stmt(Stmt::Expr(expr_stmt)) => self.visit_expr_stmt(expr_stmt),
      Node::Stmt(Stmt::Return(ret_stmt)) => self.visit_ret_stmt(ret_stmt),
      Node::Expr(expr) => self.visit_expr(expr),
      _ => todo!("unhandled node {:?}", node),
    }
  }

  #[instrument(skip_all)]
  fn visit_expr(&mut self, expr: &Expr) -> TypeId {
    match expr {
      Expr::Ident(ident) => self.visit_ident(ident),
      Expr::IntLit(int_lit) => self.visit_int_lit(int_lit),
      Expr::AsciiLit(ascii_lit) => self.visit_ascii_lit(ascii_lit),
      Expr::MemberAccess(member_access) => self.visit_member_access(member_access),
      Expr::Call(call) => self.visit_call_expr(call),
      _ => todo!("unhandled expr {:?}", expr),
    }
  }

  #[instrument(skip_all)]
  fn visit_expr_stmt(&mut self, expr_stmt: &ExprStmt) -> TypeId {
    let expr = expr_stmt.expr(&self.ctx);
    let expr_id = self.visit_expr(&expr);
    self.insert_node(expr.index(), expr_id);
    self.insert_node(expr_stmt.idx, self.void())
  }

  #[instrument(skip_all)]
  fn visit_ret_stmt(&mut self, ret_stmt: &ReturnStmt) -> TypeId {
    self.push_stack(TypeCtx::Return);
    let type_id = self.visit_expr(&ret_stmt.expr(&self.ctx));
    self.pop_stack_expecting(TypeCtx::Return);
    type_id
  }

  #[instrument(skip_all)]
  fn visit_member_access(&mut self, member_access: &MemberAccess) -> TypeId {
    let member = member_access.member(&self.ctx);
    let receiver = member_access.receiver(&self.ctx);
    match (receiver, member.lexeme(&self.ctx)) {
      // TODO: platform types should be preloaded, reused
      (Some(Node::Expr(Expr::PlatformKeyword(_))), "MainReturn") => {
        let err_id = self.push_type(Typ::Int { signed: false, bits: 8 });
        self.push_type(Typ::Result(self.void(), err_id))
      }
      (Some(Node::Expr(Expr::PlatformKeyword(_))), "print") => {
        let str_id = self.push_type(Typ::AsciiStr);
        self.push_type(Typ::Function { args: vec![str_id], ret: self.void() })
      }
      (None, _) => self.push_type(Typ::UnresolvedMemberAccess(member)),
      _ => todo!("unhandled member access"),
    }
  }

  #[instrument(skip_all)]
  fn visit_var_decl(&mut self, var_decl: &VarDecl) -> TypeId {
    // TODO: handle type annotation
    let ident_id = self.new_type_id();
    let str_idx = var_decl.name(&self.ctx).str_idx(&self.ctx);
    let (cur_scope, _) = self.ctx.scopes.current_mut();
    cur_scope.set_type(str_idx, ident_id);
    self.insert_node(var_decl.idx, ident_id);
    let expr = var_decl.expr(&self.ctx);
    let expr_id = self.visit_expr(&expr);
    self.insert_node(expr.index(), expr_id);
    self.unify(ident_id, expr_id, expr.index());
    self.void()
  }

  #[instrument(skip_all)]
  fn visit_fn_decl(&mut self, fn_decl: &FnDecl) -> TypeId {
    let fn_type_id = self.new_type_id();
    self.insert_node(fn_decl.idx, fn_type_id);
    let ret_annot_expr = fn_decl.return_annotation(&self.ctx);
    let ret_annot_type_id = self.visit_expr(&ret_annot_expr);
    self.push_stack(TypeCtx::Function(ret_annot_type_id));

    // TODO: should i be recursing and calling typecheck on the block stmt?
    let mut ret_type_id = self.void();
    let mut stmts = fn_decl.statements(&self.ctx);
    while let Some(stmt) = stmts.next(&self.ctx) {
      ret_type_id = self.visit_node(&stmt);
    }
    if !self.unify(ret_annot_type_id, ret_type_id, fn_decl.idx) {
      self.ctx.type_errors.push(TypeErr::Mismatch {
        expected: ret_annot_type_id,
        found: ret_type_id,
      });
    }
    self.pop_stack_expecting(TypeCtx::Function(ret_annot_type_id));
    // TODO: handle arg types
    self.push_type(Typ::Function { args: vec![], ret: ret_annot_type_id })
  }

  #[instrument(skip_all)]
  fn visit_call_expr(&mut self, call: &CallExpr) -> TypeId {
    let callee = call.callee(&self.ctx);
    let callee_type_id = self.visit_node(&callee);
    let fn_type = std::mem::replace(&mut self.type_db[callee_type_id.usize()], Typ::Tmp);
    match fn_type {
      Typ::Function { args, ret } => {
        return self.finish_call_expr(call, args, ret, callee_type_id);
      }
      Typ::UnresolvedMemberAccess(ident) => {
        if self.stack.last() == Some(&TypeCtx::Return) {
          if let Some(&TypeCtx::Function(ret_id)) = self.stack.get(self.stack.len() - 2) {
            return self.resolve_member_access_call_expr(&ident, ret_id, call, callee_type_id);
          }
        }
        self.type_db[callee_type_id.usize()] = fn_type;
        return self.push_set_err(TypeErr::UnresolvableMemberAccess(ident));
      }
      _ => {
        self.type_db[callee_type_id.usize()] = fn_type;
        return self.push_set_err(TypeErr::NotCallable(callee_type_id));
      }
    }
  }

  fn finish_call_expr(
    &mut self,
    call_expr: &CallExpr,
    arg_types: Vec<TypeId>,
    ret_id: TypeId,
    callee_type: TypeId,
  ) -> TypeId {
    if call_expr.num_args as usize != arg_types.len() {
      self.type_db[callee_type.usize()] = Typ::Function { args: arg_types, ret: ret_id };
      return self.push_set_err(TypeErr::Arity);
    }
    let mut had_arg_err = false;
    let mut call_args = call_expr.args(&self.ctx);
    let mut index = 0;
    while let Some(call_arg_expr) = call_args.next(&self.ctx) {
      let call_arg_id = self.visit_expr(&call_arg_expr);
      had_arg_err &= self.unify(call_arg_id, arg_types[index], call_arg_expr.index());
      index += 1;
    }
    self.type_db[callee_type.usize()] = Typ::Function { args: arg_types, ret: ret_id };
    if had_arg_err { self.push_type(Typ::Err) } else { ret_id }
  }

  fn resolve_member_access_call_expr(
    &mut self,
    ident: &Ident,
    self_type: TypeId,
    call_expr: &CallExpr,
    callee_type: TypeId,
  ) -> TypeId {
    let expected_type = &self.type_db[self_type.usize()];
    match (expected_type, ident.lexeme(&self.ctx)) {
      (&Typ::Result(_, error), "err") => {
        self.finish_call_expr(call_expr, vec![error], self_type, callee_type)
      }
      (&Typ::Result(success, _), "ok") => {
        self.finish_call_expr(call_expr, vec![success], self_type, callee_type)
      }
      _ => self.push_set_err(TypeErr::NoMember(self_type, ident.clone())),
    }
  }

  #[instrument(skip_all)]
  fn visit_ident(&mut self, ident: &Ident) -> TypeId {
    match ident.lexeme(&self.ctx) {
      "bool" => self.push_type(Typ::Bool),
      _ => {
        let str_idx = ident.str_idx(&self.ctx);
        if let Some(symbol) = self.ctx.scopes.lookup(str_idx) {
          symbol.type_id
        } else {
          self.push_set_err(TypeErr::VarNotFound(str_idx))
        }
      }
    }
  }

  fn unify(&mut self, a: TypeId, b: TypeId, err_idx: idx::AstNode) -> bool {
    let a = self.resolve_links_and_compress(a);
    let b = self.resolve_links_and_compress(b);
    if a == b {
      return true;
    }
    let a_type = &self.type_db[a.usize()];
    let b_type = &self.type_db[b.usize()];
    if a_type == b_type {
      return true;
    }
    match (a_type, b_type) {
      (Typ::Deferred, Typ::AsciiLit) => {
        self.type_db[a.usize()] = Typ::AsciiStr;
        true
      }
      (Typ::IntLit, Typ::Int { .. }) => {
        self.type_db[a.usize()] = Typ::Symlink(b);
        true
      }
      _ => false,
    }
  }

  fn resolve_links_and_compress(&mut self, type_id: TypeId) -> TypeId {
    match &self.type_db[type_id.usize()] {
      // type variable, not unified yet
      // follow symlink with path compression
      Typ::Symlink(type_id) => self.compress(*type_id),
      _ => type_id,
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
  fn visit_ascii_lit(&mut self, ascii_lit: &AsciiLit) -> TypeId {
    self.push_type(Typ::AsciiLit)
  }

  #[instrument(skip_all)]
  fn visit_int_lit(&mut self, int_lit: &IntLit) -> TypeId {
    self.push_type(Typ::IntLit)
  }

  fn insert_node(&mut self, node_idx: idx::AstNode, type_id: TypeId) -> TypeId {
    self.node_map.insert(node_idx, type_id);
    type_id
  }

  fn new_type_id(&mut self) -> TypeId {
    self.push_type(Typ::Deferred)
  }

  fn push_type(&mut self, typ: Typ) -> TypeId {
    let id = TypeId::new(self.type_db.len() as u32);
    self.type_db.push(typ);
    id
  }

  fn push_set_err(&mut self, err: TypeErr) -> TypeId {
    self.ctx.type_errors.push(err);
    self.push_type(Typ::Err)
  }

  fn void(&self) -> TypeId {
    TypeId::new(0)
  }

  fn push_stack(&mut self, typ: TypeCtx) {
    self.stack.push(typ)
  }

  fn pop_stack_expecting(&mut self, typ: TypeCtx) {
    let actual = self.stack.pop();
    assert!(actual == Some(typ))
  }
}

#[cfg(test)]
mod tests {
  use super::*;
  use pretty_assertions::assert_eq;

  fn checker_from(input: &str) -> TypeChecker {
    let parser = Parser::new_str(input);
    let ctx = parser.parse();
    TypeChecker {
      ctx,
      type_db: Vec::new(),
      node_map: BTreeMap::new(),
      stack: Vec::new(),
    }
  }

  #[test]
  fn first_goal_program() {
    let input = r#"
      rt main() -> pf.MainReturn {
        let msg = a"hello steve!";
        pf.print(msg);
        .err(17)
      }"#;
    let mut checker = checker_from(input);
    checker.check();
    assert!(checker.ctx.type_errors.is_empty());
  }

  #[test]
  fn typecheck_fn_return_mismatch() {
    let mut checker = checker_from("fn bad() -> bool { 3 }");
    checker.check();
    assert_eq!(checker.ctx.type_errors.len(), 1);
    match checker.ctx.type_errors[0] {
      TypeErr::Mismatch { .. } => {}
      _ => panic!("expected type mismatch"),
    }
  }
}
