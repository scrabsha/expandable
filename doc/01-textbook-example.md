## Textbook example

`rustc` treats macro definitions as some opaque piece of tokens and don't
do any check on them. For instance, the following macro definition is valid:

```rust
macro_rules! js_concat {
    ($left:expr, $right:expr) => {
        $left ++ $right
    };
}
```

However, any call to the `js_concat` macro is invalid, as the `++` operator
does not exist in Rust. 