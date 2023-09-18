// Architecture invariant: this module contains the definition of various AST
// nodes that are used to represent the parsed Rust code.

use proc_macro2::extra::DelimSpan;
use proc_macro2::{Delimiter, Ident, TokenStream};
use syn::{
    bracketed,
    parse::{discouraged::AnyDelimiter, Parse, ParseStream},
    token::Bracket,
    Token,
};

pub(crate) struct ItemMacroRules {
    #[allow(dead_code)]
    attrs: Vec<Attribute>,
    #[allow(dead_code)]
    macro_rules: inner::macro_rules,
    #[allow(dead_code)]
    bang: Token![!],
    #[allow(dead_code)]
    name: Ident,
    #[allow(dead_code)]
    delim: MacroDelimiter,
    pub(crate) tokens: TokenStream,
}

impl Parse for ItemMacroRules {
    fn parse(input: ParseStream) -> syn::Result<ItemMacroRules> {
        let tokens;
        Ok(ItemMacroRules {
            attrs: Attribute::parse_outer(input)?,
            macro_rules: input.parse()?,
            bang: input.parse()?,
            name: input.parse()?,
            delim: {
                let (delimiter, content) = parse_delimited(input)?;
                tokens = content;
                delimiter
            },
            tokens,
        })
    }
}

#[allow(dead_code)]
struct Attribute {
    pound: Token![#],
    bracket: Bracket,
    inner: TokenStream,
}

impl Parse for Attribute {
    fn parse(input: ParseStream) -> syn::Result<Attribute> {
        let tokens;
        Ok(Attribute {
            pound: input.parse()?,
            bracket: bracketed!(tokens in input),
            inner: tokens.parse()?,
        })
    }
}

impl Attribute {
    fn parse_outer(input: ParseStream) -> syn::Result<Vec<Attribute>> {
        let mut attrs = Vec::new();

        while input.peek(Token![#]) {
            attrs.push(input.parse()?);
        }

        Ok(attrs)
    }
}

fn parse_delimited(input: ParseStream) -> syn::Result<(MacroDelimiter, TokenStream)> {
    let (delim, span, inner) = input.parse_any_delimiter()?;
    let delim = match delim {
        Delimiter::Parenthesis => MacroDelimiter(span),
        Delimiter::Brace => MacroDelimiter(span),
        Delimiter::Bracket => MacroDelimiter(span),
        Delimiter::None => {
            return Err(syn::Error::new(
                span.join(),
                "`expandable` does not support « None delimiters »",
            ))
        }
    };

    let inner = inner.parse()?;

    Ok((delim, inner))
}

struct MacroDelimiter(DelimSpan);

mod inner {
    // The `custom_keyword` macro defines a `pub` item, which is not allowed at
    // the root of the `proc_macro` crates.
    syn::custom_keyword!(macro_rules);
}
