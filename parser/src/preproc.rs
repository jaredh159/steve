use crate::internal::*;

pub const fn prelude(_entry: &[u8]) -> Result<Module, Diagnostic> {
  Ok(Module {
    kind: ModuleKind::Bin(vec![]),
    platform: PlatformSpec {},
    alloc: AllocModel::Implicit,
    std: true,
  })
}
