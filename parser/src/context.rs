use crate::internal::*;

#[derive(Debug)]
pub struct Context {
  pub src: Vec<u8>,
  pub strs: StringPool,
  pub tokens: Vec<Token>,
  pub ast_data: AstData,
  pub scopes: Vec<Scope>,
  pub type_mismatches: Vec<TypeMismatch>,
  pub parse_errors: Vec<ParseError>,
}

impl Context {
  pub fn new() -> Self {
    Context {
      src: Vec::new(),
      strs: StringPool::new(),
      tokens: Vec::with_capacity(128),
      ast_data: AstData::with_capacity(64),
      scopes: Vec::with_capacity(16),
      type_mismatches: Vec::new(),
      parse_errors: Vec::new(),
    }
  }

  pub fn new_str(input: &str) -> Self {
    assert!(input.len() <= u32::MAX as usize);
    let mut ctx = Context::new();
    ctx.src = input.bytes().collect();
    ctx
  }

  pub fn str_idx(&self, token_idx: u32) -> idx::StrPool {
    self.tokens[token_idx as usize].index
  }

  pub fn ast_node_at(&self, index: idx::AstNode) -> Option<Node> {
    self.ast_data.node_at(index)
  }

  pub fn ast_index_after(&self, index: idx::AstNode) -> Option<idx::AstNode> {
    self.ast_data.index_after(index)
  }

  pub fn ast_node_after(&self, index: idx::AstNode) -> Option<Node> {
    self.ast_data.node_after(index)
  }

  pub fn reset(mut self) -> Self {
    self.ast_data.reset();
    self
  }
}
