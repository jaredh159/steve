use crate::internal::*;

#[derive(Debug)]
pub struct Context {
  pub src: Vec<u8>,
  pub strs: StringPool,
  pub tokens: Vec<Token>,
  pub nodes: Vec<Node>,
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
      nodes: Vec::with_capacity(64),
      scopes: Vec::with_capacity(16),
      type_mismatches: Vec::new(),
      parse_errors: Vec::new(),
    }
  }

  pub fn new_str(input: &str) -> Self {
    assert!(input.len() <= u32::MAX as usize);
    Context {
      src: input.bytes().collect(),
      strs: StringPool::new(),
      tokens: Vec::with_capacity(128),
      nodes: Vec::with_capacity(64),
      scopes: Vec::with_capacity(16),
      type_mismatches: Vec::new(),
      parse_errors: Vec::new(),
    }
  }
}
