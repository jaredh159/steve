# Steve

> The programming language Amish Cavapoos deserve

### test debug tracing

```sh
# enable tracing logs when unit testing
RUST_LOG=trace cargo test # or debug, info...

# filter out specific function/string
RUST_LOG="parser::typecheck" cargo test
```
