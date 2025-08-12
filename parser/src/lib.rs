#![allow(dead_code)]
#![allow(unused_variables)]
#![allow(unused_imports)]

pub mod ast;
pub mod context;
pub mod diag;
pub mod idx;
pub mod lexer;
pub mod meta;
pub mod parser;
pub mod preproc;
pub mod scopes;
pub mod str_pool;
pub mod token;
pub mod typecheck;

pub mod internal {
  pub use crate::ast::*;
  pub use crate::context::*;
  pub use crate::diag::*;
  pub use crate::idx;
  pub use crate::lexer::*;
  pub use crate::meta::*;
  pub use crate::parser::*;
  pub use crate::preproc::*;
  pub use crate::scopes::*;
  pub use crate::str_pool::*;
  pub use crate::token::*;
  pub use crate::typecheck::*;
  pub use bilge::prelude::*;
  pub use std::collections::BTreeMap;
  pub use tracing::instrument;
  pub use tracing::{debug, error, info, trace, warn};
}
