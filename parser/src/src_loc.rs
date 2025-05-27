#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct SrcLoc {
  pub offset: u32,
  pub len: u16,
  pub file_idx: u16,
}

impl SrcLoc {
  pub fn new(offset: u32, len: u16, file_idx: u16) -> Self {
    SrcLoc { offset, len, file_idx }
  }

  pub fn bytes<'a>(&self, src: &'a [u8]) -> &'a [u8] {
    let offset = self.offset as usize;
    &src[offset..offset + self.len as usize]
  }
}
