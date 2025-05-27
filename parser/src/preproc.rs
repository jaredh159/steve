use crate::ast::*;
use crate::diag::Diagnostic;

pub fn prelude(_entry: &[u8]) -> Result<Module, Diagnostic> {
  Ok(Module {
    kind: ModuleKind::Bin(vec![]),
    platform: PlatformSpec {},
    alloc: AllocModel::Implicit,
    std: true,
  })
}
