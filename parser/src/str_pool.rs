use crate::internal::*;

#[derive(Debug)]
pub struct StringPool {
  data: Vec<u8>,
  strs: Vec<Interned>,
}

#[derive(Debug)]
struct Interned {
  start: u32,
  len: u16,
  first_byte: u8,
}

impl Interned {
  const fn new(start: u32, len: u16, first_byte: u8) -> Self {
    Self { start, len, first_byte }
  }

  fn is(&self, s: &[u8], pool: &StringPool) -> bool {
    debug_assert!(!s.is_empty());
    if self.len as usize != s.len() {
      false
    } else if self.first_byte != s[0] {
      false
    } else {
      self.span(pool) == s
    }
  }

  fn span<'a>(&self, pool: &'a StringPool) -> &'a [u8] {
    &pool.data[self.start as usize..(self.start as usize + self.len as usize)]
  }

  fn str<'a>(&self, pool: &'a StringPool) -> &'a str {
    // SAFETY: the only way to get something into the pool is as a &str
    unsafe { std::str::from_utf8_unchecked(self.span(pool)) }
  }
}

impl StringPool {
  pub fn new() -> Self {
    let mut pool = Self {
      data: Vec::with_capacity(1024),
      strs: Vec::with_capacity(64),
    };
    // special case empty string
    pool.strs.push(Interned::new(0, 0, 0));
    pool
  }

  pub const fn empty(&self) -> idx::StrPool {
    idx::StrPool::new(0)
  }

  pub const fn sentinel(&self, value: u32) -> idx::StrPool {
    idx::StrPool::new(value)
  }

  pub fn intern(&mut self, s: &str) -> idx::StrPool {
    if s.is_empty() {
      return idx::StrPool::new(0);
    };
    for (i, interned) in self.strs.iter().enumerate().skip(1) {
      if interned.is(s.as_bytes(), self) {
        return idx::StrPool::new(i as u32);
      }
    }
    assert!(self.strs.len() < u32::MAX as usize);
    let index = self.strs.len() as u32;
    let start = self.data.len() as u32;
    let bytes = s.as_bytes();
    let first = bytes[0];
    self.data.extend_from_slice(bytes);
    self.strs.push(Interned::new(start, s.len() as u16, first));
    idx::StrPool::new(index)
  }

  pub fn get(&self, index: idx::StrPool) -> &str {
    let interned = &self.strs[index.usize()];
    interned.str(self)
  }

  pub fn debug_print(&self) {
    eprintln!("StrPool ({})", self.strs.len());
    for i in 0..self.strs.len() {
      eprintln!(" - {i}: `{}`", self.get(idx::StrPool::new(i as u32)));
    }
  }
}

impl Default for StringPool {
  fn default() -> Self {
    Self::new()
  }
}

#[cfg(test)]
mod tests {
  use super::*;

  #[test]
  fn interning_strs() {
    let mut pool = StringPool::new();
    assert_eq!(pool.intern(""), idx::StrPool::new(0));
    assert_eq!(pool.intern(""), idx::StrPool::new(0));
    assert_eq!(pool.get(idx::StrPool::new(0)), "");
    assert_eq!(pool.intern("foo"), idx::StrPool::new(1));
    assert_eq!(pool.data, b"foo");
    assert_eq!(pool.intern("foo"), idx::StrPool::new(1));
    assert_eq!(pool.get(idx::StrPool::new(1)), "foo");
    assert_eq!(pool.intern("bar"), idx::StrPool::new(2));
    assert_eq!(pool.data, b"foobar");
    assert_eq!(pool.get(idx::StrPool::new(2)), "bar");
    assert_eq!(pool.intern("bar"), idx::StrPool::new(2));
    assert_eq!(pool.intern("foo"), idx::StrPool::new(1));
    assert_eq!(pool.intern("foot"), idx::StrPool::new(3));
    assert_eq!(pool.data, b"foobarfoot");
  }
}
