- [ ] lex, parse, typecheck snippet
- [ ] treesitter grammar
- [ ] basic lsp server
- [ ] emit arm assembly

```stv
#bin: stv.platforms.concrete.cli
#mem: implicit
#std: true

rt main() -> pf.MainReturn {
  let msg = a"hello steve!";
  pf.print(msg);
  .ok(0)
}
```
