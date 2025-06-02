#![allow(dead_code)]

#[derive(Debug)]
pub struct StringPool {
  data: Vec<u8>,
  strs: Vec<Interned>,
}

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub struct Index(u32);

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

  pub const fn empty(&self) -> Index {
    Index(0)
  }

  pub const fn sentinel(&self, value: u32) -> Index {
    Index(value)
  }

  pub fn intern(&mut self, s: &str) -> Index {
    if s.is_empty() {
      return Index(0);
    };
    for (i, interned) in self.strs.iter().enumerate().skip(1) {
      if interned.is(s.as_bytes(), self) {
        return Index(i as u32);
      }
    }
    assert!(self.strs.len() < u32::MAX as usize);
    let index = self.strs.len() as u32;
    let start = self.data.len() as u32;
    let bytes = s.as_bytes();
    let first = bytes[0];
    self.data.extend_from_slice(bytes);
    self.strs.push(Interned::new(start, s.len() as u16, first));
    Index(index)
  }

  pub fn get(&self, index: Index) -> &str {
    let interned = &self.strs[index.0 as usize];
    interned.str(self)
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
    assert_eq!(pool.intern(""), Index(0));
    assert_eq!(pool.intern(""), Index(0));
    assert_eq!(pool.get(Index(0)), "");
    assert_eq!(pool.intern("foo"), Index(1));
    assert_eq!(pool.data, b"foo");
    assert_eq!(pool.intern("foo"), Index(1));
    assert_eq!(pool.get(Index(1)), "foo");
    assert_eq!(pool.intern("bar"), Index(2));
    assert_eq!(pool.data, b"foobar");
    assert_eq!(pool.get(Index(2)), "bar");
    assert_eq!(pool.intern("bar"), Index(2));
    assert_eq!(pool.intern("foo"), Index(1));
    assert_eq!(pool.intern("foot"), Index(3));
    assert_eq!(pool.data, b"foobarfoot");
  }
}
