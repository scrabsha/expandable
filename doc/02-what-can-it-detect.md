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
