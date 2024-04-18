macro_rules! quote {
    (@mk_term $sb:expr, $term:expr $(,)?) => {{
        $sb.push($term);
    }};

    (@inner $sb:expr, ( $( $tt:tt )* ) ) => {{
        $sb.push($crate::generated::TokenDescription::LParen);
        quote! { @with_sb $sb, $( $tt )* }
        $sb.push($crate::generated::TokenDescription::RParen);
    }};

    (@inner $sb:expr, { $( $tt:tt )* } ) => {{
        $sb.push($crate::generated::TokenDescription::LBrace);
        quote! { @with_sb $sb, $( $tt )* }
        $sb.push($crate::generated::TokenDescription::RBrace);
    }};

    (@inner $sb:expr, [ $( $tt:tt )* ] ) => {
        $sb.push($crate::generated::TokenDescription::LBracket);
        quote! { @with_sb $sb, $( $tt )* }
        $sb.push($crate::generated::TokenDescription::RBracket);
    };

    // Keywords
    (@inner $sb:expr, as) => {
        quote!(@mk_term $sb, $crate::generated::TokenDescription::As)
    };

    (@inner $sb:expr, async) => {
        quote!(@mk_term $sb, $crate::generated::TokenDescription::Async)
    };

    (@inner $sb:expr, break) => {
        quote!(@mk_term $sb, $crate::generated::TokenDescription::Break)
    };

    (@inner $sb:expr, const) => {
        quote!(@mk_term $sb, $crate::generated::TokenDescription::Const)
    };

    (@inner $sb:expr, continue) => {
        quote!(@mk_term $sb, $crate::generated::TokenDescription::Continue)
    };

    (@inner $sb:expr, crate) => {
        quote!(@mk_term $sb, $crate::generated::TokenDescription::Crate)
    };

    (@inner $sb:expr, else) => {
        quote!(@mk_term $sb, $crate::generated::TokenDescription::Else)
    };

    (@inner $sb:expr, enum) => {
        quote!(@mk_term $sb, $crate::generated::TokenDescription::Enum)
    };

    (@inner $sb:expr, extern) => {
        quote!(@mk_term $sb, $crate::generated::TokenDescription::Extern)
    };

    (@inner $sb:expr, false) => {
        quote!(@mk_term $sb, $crate::generated::TokenDescription::False)
    };

    (@inner $sb:expr, fn) => {
        quote!(@mk_term $sb, $crate::generated::TokenDescription::Fn)
    };

    (@inner $sb:expr, for) => {
        quote!(@mk_term $sb, $crate::generated::TokenDescription::For)
    };

    (@inner $sb:expr, if) => {
        quote!(@mk_term $sb, $crate::generated::TokenDescription::If)
    };

    (@inner $sb:expr, impl) => {
        quote!(@mk_term $sb, $crate::generated::TokenDescription::Impl)
    };

    (@inner $sb:expr, in) => {
        quote!(@mk_term $sb, $crate::generated::TokenDescription::In)
    };

    (@inner $sb:expr, let) => {
        quote!(@mk_term $sb, $crate::generated::TokenDescription::Let)
    };

    (@inner $sb:expr, loop) => {
        quote!(@mk_term $sb, $crate::generated::TokenDescription::Loop)
    };

    (@inner $sb:expr, match) => {
        quote!(@mk_term $sb, $crate::generated::TokenDescription::Match)
    };

    (@inner $sb:expr, mod) => {
        quote!(@mk_term $sb, $crate::generated::TokenDescription::Mod)
    };

    (@inner $sb:expr, move) => {
        quote!(@mk_term $sb, $crate::generated::TokenDescription::Move)
    };

    (@inner $sb:expr, mut) => {
        quote!(@mk_term $sb, $crate::generated::TokenDescription::Mut)
    };

    (@inner $sb:expr, pub) => {
        quote!(@mk_term $sb, $crate::generated::TokenDescription::Pub)
    };

    (@inner $sb:expr, ref) => {
        quote!(@mk_term $sb, $crate::generated::TokenDescription::Ref)
    };

    (@inner $sb:expr, return) => {
        quote!(@mk_term $sb, $crate::generated::TokenDescription::Return)
    };

    (@inner $sb:expr, self) => {
        quote!(@mk_term $sb, $crate::generated::TokenDescription::Self_)
    };

    (@inner $sb:expr, Self) => {
        quote!(@mk_term $sb, $crate::generated::TokenDescription::SelfUpper)
    };

    (@inner $sb:expr, static) => {
        quote!(@mk_term $sb, $crate::generated::TokenDescription::Static)
    };

    (@inner $sb:expr, struct) => {
        quote!(@mk_term $sb, $crate::generated::TokenDescription::Struct)
    };

    (@inner $sb:expr, super) => {
        quote!(@mk_term $sb, $crate::generated::TokenDescription::Super)
    };

    (@inner $sb:expr, trait) => {
        quote!(@mk_term $sb, $crate::generated::TokenDescription::Trait)
    };

    (@inner $sb:expr, true) => {
        quote!(@mk_term $sb, $crate::generated::TokenDescription::True)
    };

    (@inner $sb:expr, type) => {
        quote!(@mk_term $sb, $crate::generated::TokenDescription::Type)
    };

    (@inner $sb:expr, unsafe) => {
        quote!(@mk_term $sb, $crate::generated::TokenDescription::Unsafe)
    };

    (@inner $sb:expr, use) => {
        quote!(@mk_term $sb, $crate::generated::TokenDescription::Use)
    };

    (@inner $sb:expr, where) => {
        quote!(@mk_term $sb, $crate::generated::TokenDescription::Where)
    };

    (@inner $sb:expr, while) => {
        quote!(@mk_term $sb, $crate::generated::TokenDescription::While)
    };

    // Keywords that are also reserved
    (@inner $sb:expr, abstract) => {
        quote!(@mk_term $sb, $crate::generated::TokenDescription::Abstract)
    };

    (@inner $sb:expr, become) => {
        quote!(@mk_term $sb, $crate::generated::TokenDescription::Become)
    };

    (@inner $sb:expr, box) => {
        quote!(@mk_term $sb, $crate::generated::TokenDescription::Box)
    };

    (@inner $sb:expr, do) => {
        quote!(@mk_term $sb, $crate::generated::TokenDescription::Do)
    };

    (@inner $sb:expr, final) => {
        quote!(@mk_term $sb, $crate::generated::TokenDescription::Final)
    };

    (@inner $sb:expr, macro) => {
        quote!(@mk_term $sb, $crate::generated::TokenDescription::Macro)
    };

    (@inner $sb:expr, offsetof) => {
        quote!(@mk_term $sb, $crate::generated::TokenDescription::Offsetof)
    };

    (@inner $sb:expr, override) => {
        quote!(@mk_term $sb, $crate::generated::TokenDescription::Override)
    };

    (@inner $sb:expr, priv) => {
        quote!(@mk_term $sb, $crate::generated::TokenDescription::Priv)
    };

    (@inner $sb:expr, proc) => {
        quote!(@mk_term $sb, $crate::generated::TokenDescription::Proc)
    };

    (@inner $sb:expr, sizeof) => {
        quote!(@mk_term $sb, $crate::generated::TokenDescription::Sizeof)
    };

    (@inner $sb:expr, typeof) => {
        quote!(@mk_term $sb, $crate::generated::TokenDescription::Typeof)
    };

    (@inner $sb:expr, unsized) => {
        quote!(@mk_term $sb, $crate::generated::TokenDescription::Unsized)
    };

    (@inner $sb:expr, virtual) => {
        quote!(@mk_term $sb, $crate::generated::TokenDescription::Virtual)
    };

    (@inner $sb:expr, yield) => {
        quote!(@mk_term $sb, $crate::generated::TokenDescription::Yield)
    };

    (@inner $sb:expr, await) => {
        quote!(@mk_term $sb, $crate::generated::TokenDescription::Await)
    };

    (@inner $sb:expr, dyn) => {
        quote!(@mk_term $sb, $crate::generated::TokenDescription::Dyn)
    };

    (@inner $sb:expr, abstract) => {
        quote!(@mk_term $sb, $crate::generated::TokenDescription::Abstract)
    };

    (@inner $sb:expr, catch) => {
        quote!(@mk_term $sb, $crate::generated::TokenDescription::Catch)
    };

    (@inner $sb:expr, final) => {
        quote!(@mk_term $sb, $crate::generated::TokenDescription::Final)
    };

    (@inner $sb:expr, macro) => {
        quote!(@mk_term $sb, $crate::generated::TokenDescription::Macro)
    };

    (@inner $sb:expr, override) => {
        quote!(@mk_term $sb, $crate::generated::TokenDescription::Override)
    };

    (@inner $sb:expr, priv) => {
        quote!(@mk_term $sb, $crate::generated::TokenDescription::Priv)
    };

    (@inner $sb:expr, try) => {
        quote!(@mk_term $sb, $crate::generated::TokenDescription::Try)
    };

    (@inner $sb:expr, union) => {
        quote!(@mk_term $sb, $crate::generated::TokenDescription::Union)
    };

    (@inner $sb:expr, $id:ident) => {
        quote!(@mk_term $sb, $crate::generated::TokenDescription::Ident)
    };

    // Punctuates
    (@inner $sb:expr, +) => {
        quote!(@mk_term $sb, $crate::generated::TokenDescription::Plus)
    };

    (@inner $sb:expr, -) => {
        quote!(@mk_term $sb, $crate::generated::TokenDescription::Minus)
    };

    (@inner $sb:expr, *) => {
        quote!(@mk_term $sb, $crate::generated::TokenDescription::Star)
    };

    (@inner $sb:expr, /) => {
        quote!(@mk_term $sb, $crate::generated::TokenDescription::Slash)
    };

    (@inner $sb:expr, %) => {
        quote!(@mk_term $sb, $crate::generated::TokenDescription::Percent)
    };

    (@inner $sb:expr, ^) => {
        quote!(@mk_term $sb, $crate::generated::TokenDescription::Caret)
    };

    (@inner $sb:expr, !) => {
        quote!(@mk_term $sb, $crate::generated::TokenDescription::Not)
    };

    (@inner $sb:expr, &) => {
        quote!(@mk_term $sb, $crate::generated::TokenDescription::And)
    };

    (@inner $sb:expr, |) => {
        quote!(@mk_term $sb, $crate::generated::TokenDescription::Or)
    };

    (@inner $sb:expr, &&) => {
        quote!(@mk_term $sb, $crate::generated::TokenDescription::AndAnd)
    };

    (@inner $sb:expr, ||) => {
        quote!(@mk_term $sb, $crate::generated::TokenDescription::OrOr)
    };

    (@inner $sb:expr, <<) => {
        quote!(@mk_term $sb, $crate::generated::TokenDescription::Shl)
    };

    (@inner $sb:expr, >>) => {
        quote!(@mk_term $sb, $crate::generated::TokenDescription::Shr)
    };

    (@inner $sb:expr, +=) => {
        quote!(@mk_term $sb, $crate::generated::TokenDescription::PlusEquals)
    };

    (@inner $sb:expr, -=) => {
        quote!(@mk_term $sb, $crate::generated::TokenDescription::MinusEquals)
    };

    (@inner $sb:expr, *=) => {
        quote!(@mk_term $sb, $crate::generated::TokenDescription::StarEquals)
    };

    (@inner $sb:expr, /=) => {
        quote!(@mk_term $sb, $crate::generated::TokenDescription::SlashEquals)
    };

    (@inner $sb:expr, %=) => {
        quote!(@mk_term $sb, $crate::generated::TokenDescription::PercentEquals)
    };

    (@inner $sb:expr, ^=) => {
        quote!(@mk_term $sb, $crate::generated::TokenDescription::CaretEquals)
    };

    (@inner $sb:expr, &=) => {
        quote!(@mk_term $sb, $crate::generated::TokenDescription::AndEquals)
    };

    (@inner $sb:expr, |=) => {
        quote!(@mk_term $sb, $crate::generated::TokenDescription::OrEquals)
    };

    (@inner $sb:expr, <<=) => {
        quote!(@mk_term $sb, $crate::generated::TokenDescription::ShlEquals)
    };

    (@inner $sb:expr, >>=) => {
        quote!(@mk_term $sb, $crate::generated::TokenDescription::ShrEquals)
    };

    (@inner $sb:expr, =) => {
        quote!(@mk_term $sb, $crate::generated::TokenDescription::Equals)
    };

    (@inner $sb:expr, ==) => {
        quote!(@mk_term $sb, $crate::generated::TokenDescription::EqualsEquals)
    };

    (@inner $sb:expr, !=) => {
        quote!(@mk_term $sb, $crate::generated::TokenDescription::NotEquals)
    };

    (@inner $sb:expr, >) => {
        quote!(@mk_term $sb, $crate::generated::TokenDescription::GreaterThan)
    };

    (@inner $sb:expr, <) => {
        quote!(@mk_term $sb, $crate::generated::TokenDescription::LessThan)
    };

    (@inner $sb:expr, >=) => {
        quote!(@mk_term $sb, $crate::generated::TokenDescription::GreaterThanEquals)
    };

    (@inner $sb:expr, <=) => {
        quote!(@mk_term $sb, $crate::generated::TokenDescription::LessThanEquals)
    };

    (@inner $sb:expr, @) => {
        quote!(@mk_term $sb, $crate::generated::TokenDescription::At)
    };

    (@inner $sb:expr, _) => {
        quote!(@mk_term $sb, $crate::generated::TokenDescription::Underscore)
    };

    (@inner $sb:expr, .) => {
        quote!(@mk_term $sb, $crate::generated::TokenDescription::Dot)
    };

    (@inner $sb:expr, ..) => {
        quote!(@mk_term $sb, $crate::generated::TokenDescription::DotDot)
    };

    (@inner $sb:expr, ...) => {
        quote!(@mk_term $sb, $crate::generated::TokenDescription::DotDotDot)
    };

    (@inner $sb:expr, ..=) => {
        quote!(@mk_term $sb, $crate::generated::TokenDescription::DotDotEquals)
    };

    (@inner $sb:expr, ,) => {
        quote!(@mk_term $sb, $crate::generated::TokenDescription::Comma)
    };

    (@inner $sb:expr, ; ) => {
        quote!(@mk_term $sb, $crate::generated::TokenDescription::Semicolon)
    };

    (@inner $sb:expr, : ) => {
        quote!(@mk_term $sb, $crate::generated::TokenDescription::Colon)
    };

    (@inner $sb:expr, ::) => {
        quote!(@mk_term $sb, $crate::generated::TokenDescription::ColonColon)
    };

    (@inner $sb:expr, ->) => {
        quote!(@mk_term $sb, $crate::generated::TokenDescription::RightArrow)
    };

    (@inner $sb:expr, =>) => {
        quote!(@mk_term $sb, $crate::generated::TokenDescription::FatArrow)
    };

    // Warning: this is equivalent to the `$` sign.
    (@inner $sb:expr, #) => {
        quote!(@mk_term $sb, $crate::generated::TokenDescription::Dollar)
    };

    (@inner $sb:expr, ?) => {
        quote!(@mk_term $sb, $crate::generated::TokenDescription::QuestionMark)
    };

    // Keep this rule at the end of the list, so that it does not match `true`
    // and `false`, which are keywords.
    (@inner $sb:expr, $lit:literal) => {
        quote!(
            @mk_term $sb,
            $crate::generated::TokenDescription::Literal(stringify!($lit).to_string()),
        )
    };

    (@inner $sb:expr, $tt:tt) => {
        compile_error!(concat!("Unexpected token: ", stringify!($tt)))
    };

    (@with_sb $sb:expr, ) => {};

    (@with_sb $sb:expr, $( $tt:tt )* ) => {{
        $(
            quote!(@inner $sb, $tt);
        )*
    }};

    (@inner $sb:expr, $tt:tt) => {
        compile_error!(concat!("Unexpected token: ", stringify!($tt)))
    };

    (@with_sb $sb:expr, ) => {};

    (@with_sb $sb:expr, $( $tt:tt )* ) => {{
        $crate::quote!(@inner $sb, $tt);
    }};

    ( $( $tt:tt )* ) => {{
        let mut container = Vec::new();
        quote! { @with_sb container, $( $tt )* };
        container
    }};
}
