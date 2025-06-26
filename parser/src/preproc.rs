use crate::internal::*;

pub const fn prelude(_entry: &[u8]) -> Result<Module, Diagnostic> {
  Ok(Module {
    kind: ModuleKind::Bin,
    platform: PlatformSpec {},
    alloc: AllocModel::Implicit,
    std: true,
  })
}
