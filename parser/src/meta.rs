#[derive(Debug)]
pub struct Module {
  pub kind: ModuleKind,
  pub platform: PlatformSpec,
  pub alloc: AllocModel,
  pub std: bool,
}

#[derive(Debug)]
pub struct PlatformSpec {
  // Fields for platform specification
}

#[derive(Debug, PartialEq, Eq)]
pub enum ModuleKind {
  Bin, //(Vec<Node>),
  Lib, // export map of some sort
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum AllocModel {
  Implicit,
  Explicit,
}
