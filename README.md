<div class="title-block" style="text-align: center;" align="center">
<h1><code>expandable</code></h1>
An opinionated attribute-macro based <code>macro_rules!</code> expansion
checker.
</div>

<br />
<br />
<p style="text-align:center" align="center">
<img src="https://cdn.githubraw.com/scrabsha/expendable/main/assets/top_image.png"
     width=70%
     style="text-align:center"
     alt="Sylvester Stallone in Rambo: First Blood Part II. The image has been edited such that his two hands are thumbsup-ing. His body is covered with dust and sweat and a bit of blood (not his). At the bottom of the image is written 'POV: #[expandable::expr] stops complaining'." />
</p>

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
Luckily for us, this crate provides the [`expandable::expr`] macro, that
checks that the macro expands to a valid expression. Let's use it on
`js_concat`:

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
error: Potentially invalid expansion. Expected an identifier, a literal, `if`, a `[`.
 --> tests/ui/fail/js_concat.rs:5:16
  |
5 |         $left ++ $right
  |                ^
```
 
[^error-message]: The Rust grammar is not fully implemented at the moment,
    leading to incomplete "expected xxx" list this will be fixed before the
    first non-alpha release of this crate.

## Expansion context

Macros can expand to different things depending on where they are called.
As a result, `expandable` must know what the macro expands to. To do so,
multiple macros are available:
- Macros that expand to expressions are checked by [`expandable::expr`],
- Macros that expand to items are checked by [`expandable::item`],
- TODO: pattern, statements, type.

[`expandable::expr`]: macro@expr
[`expandable::item`]: macro@item
## What can it detect?

This section briefly describes what the macros can detect and what they
can't. All the terminology used in this section is taken from the
[Ferrocene Language Specification][ferrocene-spec].

This crate aims to detect all the things that are not considered by `rustc`
as an invalid macro definition. It may or may not detect what `rustc`
already detects. Some errors can't be detected because they are not
possible to detect in the first place.

[ferrocene-spec]: https://spec.ferrocene.dev/

### Invalid matcher

`rustc` is quite good at detecting invalid matchers. This crate deliberately
does not try to compete with it. Whether if this crate returns an error on
an invalid matcher is left unspecified.

### Invalid transcription

Some macros can't be expanded because they don't respect the rules for
a transcription to happen. For instance, the following macro exhibits
a transcription error:

```rust
macro_rules! invalid_expansion {
  ( $( $a:ident )? ) => { $a }
}
```

In this example, the repetition nesting of `$a` used in the macro
transcription does not match the repetition nesting of `$a` defined in the
matcher.

This crate aims to detect all the possible invalid transcription.

### Invalid produced AST

Some macro expand correctly (ie: they respect the transcription rules), but
the produced AST is invalid. For instance, the following macro expands to
an invalid AST:

```rust
macro_rules! invalid_expansion {
    () => { * }
}
```

(`*` is an invalid item, an invalid expression, an invalid pattern, an
invalid statement and an invalid type -- there is no context in which it can
be called).

`rustc` doesn't (and can't) detect any potentially invalid AST. This is the
_raison d'être_ of this crate.

## Opinionated?

In the general case, proving that a macro is well-formed is impossible. This
crates has to make assumptions about the macros it is checking. This section
lists them and gives a short rationale of why they are needed.

### No recursive macro definition

This crate assumes that the macro expansion does not contain any macro
definition. For instance, the following macro definition can't be checked
with `expandable`:

```rust
macro_rules! foo {
    () => {
        macro_rules! bar {
            () => { 42 }
        }
    }
}
```

This limitation is caused by the fact that it's impossible to tell if a
sequence of tokens is a macro definition or not. For instance, the
`macro_rules` token may be passed by the user when invoking the macro.
Having no guarantee of what's a macro and what is not makes it impossible
to ensure that a metavariable is properly defined.

### No macro call (for now)

This crate assumes that _one expansion_ of a macro works. It does not check
that the recursive expansion of the macro works. It checks that the macro
invocation itself is legal, but don't check that it matches any rule.

This is caused by the fact that other macros may add more constrains on the
macro invocation. _This requirement may be lifted in the future for macros
that call themselves._

### The repetition stack must match

This crate requires a metavariable indication used in a transcriber to have
the same repetition stack as the corresponding metavariable that appears in
the matcher.

For instance, the following code is rejected by `expandable`:

```rust,compile_fail
#[expandable::expr]
macro_rules! fns {
    ($($a:ident)*) => {
        $(
            fn $a() {}
        )+
    }
}
```

## Minimal Supported Rust Version (MSRV), syntax support and stability

_`expandable` supports Rust 1.65 and above. Bumping the MSRV is
considered a breaking change._

Note that the embedded parser will support syntax that was introduced
_after_ Rust 1.65.

Adding support for newer syntax is _not_ considered a breaking change.

The error messages generated by `expandable` are not stable and may change
without notice.

Any change that may trigger errors on previously accepted code is considered
a breaking change.

## License

Licensed under the [MIT license].

[MIT license]: https://github.com/scrabsha/expandable/blob/main/LICENSE
