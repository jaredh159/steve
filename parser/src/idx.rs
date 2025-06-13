macro_rules! index_impl_for {
  ($type:ty) => {
    impl $type {
      pub const fn new(idx: u32) -> Self {
        Self(idx)
      }

      pub const fn undefined() -> Self {
        Self(u32::MAX)
      }

      pub const fn is_undefined(&self) -> bool {
        self.0 == u32::MAX
      }

      pub const fn set(&mut self, idx: u32) {
        self.0 = idx;
      }

      pub const fn usize(&self) -> usize {
        self.0 as usize
      }

      pub const fn decr(&self) -> Self {
        Self(self.0 - 1)
      }
    }
    impl std::fmt::Debug for $type {
      fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "idx::{}({})", stringify!($type), self.0)
      }
    }
  };
}

#[derive(PartialEq, Eq, Clone, Copy, Hash, PartialOrd, Ord)]
pub struct StrPool(u32);
index_impl_for!(StrPool);

#[derive(Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct AstNode(u32);
index_impl_for!(AstNode);

#[derive(Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct TypeId(u32);
index_impl_for!(TypeId);

#[derive(PartialEq, Eq, Clone, Copy, Hash, PartialOrd, Ord)]
pub struct ScopeId(u32);
index_impl_for!(ScopeId);
