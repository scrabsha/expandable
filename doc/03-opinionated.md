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