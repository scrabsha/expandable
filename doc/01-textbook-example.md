## Textbook example

`rustc` treats macro definitions as some opaque piece of tokens and don't do any
check on them. For instance, the following macro definition is valid:

```rust
macro_rules! my_vec {
  ($($t:expr),*) => {{
      let mut buffer = Vec::new();

      $(
          buffer->push($t);
      )*

      buffer
  }};
}
```

However, any call to the `my_vec` macro is invalid, as `->` can't be used for
method calls.
