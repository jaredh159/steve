#[derive(Debug)]
pub struct Diagnostic {
  pub line: u32,
  pub col: u32,
  pub width: u32,
  pub msg: String,
}
