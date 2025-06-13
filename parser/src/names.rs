#![allow(dead_code)]
#![allow(unused_variables)]
#![allow(unused_imports)]

use std::collections::BTreeMap;

use crate::internal::*;

#[derive(Debug)]
pub struct Scope {
  parent: Option<idx::ScopeId>,
  symbols: BTreeMap<idx::StrPool, Symbol>,
}

#[derive(Debug)]
pub enum Symbol {}

#[cfg(test)]
mod tests {
  use super::*;
}
