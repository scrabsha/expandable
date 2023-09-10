<div class="title-block" style="text-align: center;" align="center">
<h1><code>expandable</code></h1>
An attribute-macro based <code>macro_rules!</code> expansion checker.
</div>

## Textbook example

`rustc` treats macro definitions as some opaque piece of tokens and don't
do any check on it. For instance, the following macro definition is valid:

```rust
macro_rules! js_concat {
    ($left:expr, $right:expr) => {
        $left ++ $right
    };
}
```

However, any call to the `js_concat` macro is invalid, as the `++` operator
does not exist in Rust. Luckily for us, this crate provides the
[`expandable::expr`] macro, that checks that the macro expands to a valid
expression. Let's use it on `js_concat`:

[`expandable::expr`]: https://example.org

```rust,compile_fail
#[expandable::expr]
macro_rules! js_concat {
    ($left:expr, $right:expr) => {
        $left ++ $right
    };
}
```

This emits the following error [^error-message]:
```none
error: Potentially invalid expansion. Expected an identifier.
 --> tests/ui/fail/js_concat.rs:4:16
  |
4 |         $left ++ $right
  |                ^
```

[^error-message]: The Rust grammar is not fully implemented at the moment,
    leading to incomplete "expected xxx" list this will be fixed before the
    first non-alpha release of this crate.
