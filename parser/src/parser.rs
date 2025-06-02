#![allow(unused_imports)]
#![allow(dead_code)]
use crate::ast::{NodeKind as N, *};
use crate::diag::Diagnostic;
use crate::lexer::Lexer;
use crate::token::Token;

#[derive(Debug)]
pub struct Parser {
  lexer: Lexer,
  tokens: Vec<Token>,
  diagnostics: Vec<Diagnostic>,
}

impl Parser {
  pub fn new_str(src: &str) -> Self {
    let mut lexer = Lexer::new_str(src);
    let tokens = lexer.tokens();
    Self { tokens, lexer, diagnostics: vec![] }
  }

  pub fn parse(self) -> Result<Vec<Node>, Vec<Diagnostic>> {
    Ok(vec![])
  }
}

#[cfg(test)]
mod tests {
  use super::*;

  #[test]
  fn first_goal_program() {
    let input = r#"
rt main() -> pf.MainReturn {
  let msg = a"hello steve!";
  pf.print(msg);
  .ok(17)
}"#;
    let parser = Parser::new_str(input);
    let nodes = parser.parse().unwrap();
    assert_eq!(
      &nodes,
      &[
        // Node {
        //   kind: N::VarDeclStmt(N_VarDecl::new(false)),
        //   token: 9
        // },
        // Node { kind: N::AsciiLit, token: 12 },
        // Node { kind: N::ExprStmt, token: 14 },
        // Node { kind: N::CallExpr, token: 14 },
        // Node { kind: N::VarDeclStmt, token: 9 }
      ]
    );
  }
}
