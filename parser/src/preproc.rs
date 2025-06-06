use crate::{diag::Diagnostic, meta::*};

pub const fn prelude(_entry: &[u8]) -> Result<Module, Diagnostic> {
  Ok(Module {
    kind: ModuleKind::Bin(vec![]),
    platform: PlatformSpec {},
    alloc: AllocModel::Implicit,
    std: true,
  })
}
