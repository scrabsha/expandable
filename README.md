<div class="title-block" style="text-align: center;" align="center">
<h1><code>expandable</code></h1>
An attribute-macro based <code>macro_rules!</code> expansion checker.
</div>

<br />
<br />

<span style="text-align:center">
<img src="https://cdn.githubraw.com/scrabsha/expendable/main/assets/top_image.png"
     width=70%
     style="text-align:center"
     alt="Sylvester Stallone in Rambo: First Blood Part II. The image has been edited such that his two hands are thumbsup-ing. His body is covered with dust and sweat and a bit of blood (not his). At the bottom of the image is written 'POV: #[expandable::expr] stops complaining'." />
</span>

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
does not exist in Rust. Luckily for us, this crate provides the
[`expandable::expr`] macro, that checks that the macro expands to a valid
expression. Let's use it on `js_concat`:

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

## Expansion context

Macros can expand to different things depending on where they are called.
As a result, `expandable` must know what the macro expands to. To do so,
multiple macros are available:
- Macros that expand to expressions are checked by [`expandable::expr`],
- Macros that expand to items are checked by [`expandable::item`],
- TODO: pattern, statements, type.

[`expandable::expr`]: macro@expr
[`expandable::item`]: macro@item

[^error-message]: The Rust grammar is not fully implemented at the moment,
    leading to incomplete "expected xxx" list this will be fixed before the
    first non-alpha release of this crate.
