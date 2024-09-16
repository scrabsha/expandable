macro_rules! quote {
    (@mk_term $sb:expr, $term:expr $(,)?) => {{
        $sb.push($term);
    }};

    (@inner $sb:expr, ( $( $tt:tt )* ) ) => {{
        $sb.push($crate::token::TokenDescription::LParen);
        quote! { @with_sb $sb, $( $tt )* }
        $sb.push($crate::token::TokenDescription::RParen);
    }};

    (@inner $sb:expr, { $( $tt:tt )* } ) => {{
        $sb.push($crate::token::TokenDescription::LBrace);
        quote! { @with_sb $sb, $( $tt )* }
        $sb.push($crate::token::TokenDescription::RBrace);
    }};

    (@inner $sb:expr, [ $( $tt:tt )* ] ) => {
        $sb.push($crate::token::TokenDescription::LBracket);
        quote! { @with_sb $sb, $( $tt )* }
        $sb.push($crate::token::TokenDescription::RBracket);
    };

    // Keywords
    (@inner $sb:expr, as) => {
        quote!(@mk_term $sb, $crate::token::TokenDescription::As)
    };

    (@inner $sb:expr, async) => {
        quote!(@mk_term $sb, $crate::token::TokenDescription::Async)
    };

    (@inner $sb:expr, break) => {
        quote!(@mk_term $sb, $crate::token::TokenDescription::Break)
    };

    (@inner $sb:expr, const) => {
        quote!(@mk_term $sb, $crate::token::TokenDescription::Const)
    };

    (@inner $sb:expr, continue) => {
        quote!(@mk_term $sb, $crate::token::TokenDescription::Continue)
    };

    (@inner $sb:expr, crate) => {
        quote!(@mk_term $sb, $crate::token::TokenDescription::Crate)
    };

    (@inner $sb:expr, else) => {
        quote!(@mk_term $sb, $crate::token::TokenDescription::Else)
    };

    (@inner $sb:expr, enum) => {
        quote!(@mk_term $sb, $crate::token::TokenDescription::Enum)
    };

    (@inner $sb:expr, extern) => {
        quote!(@mk_term $sb, $crate::token::TokenDescription::Extern)
    };

    (@inner $sb:expr, false) => {
        quote!(@mk_term $sb, $crate::token::TokenDescription::False)
    };

    (@inner $sb:expr, fn) => {
        quote!(@mk_term $sb, $crate::token::TokenDescription::Fn)
    };

    (@inner $sb:expr, for) => {
        quote!(@mk_term $sb, $crate::token::TokenDescription::For)
    };

    (@inner $sb:expr, if) => {
        quote!(@mk_term $sb, $crate::token::TokenDescription::If)
    };

    (@inner $sb:expr, impl) => {
        quote!(@mk_term $sb, $crate::token::TokenDescription::Impl)
    };

    (@inner $sb:expr, in) => {
        quote!(@mk_term $sb, $crate::token::TokenDescription::In)
    };

    (@inner $sb:expr, let) => {
        quote!(@mk_term $sb, $crate::token::TokenDescription::Let)
    };

    (@inner $sb:expr, loop) => {
        quote!(@mk_term $sb, $crate::token::TokenDescription::Loop)
    };

    (@inner $sb:expr, match) => {
        quote!(@mk_term $sb, $crate::token::TokenDescription::Match)
    };

    (@inner $sb:expr, mod) => {
        quote!(@mk_term $sb, $crate::token::TokenDescription::Mod)
    };

    (@inner $sb:expr, move) => {
        quote!(@mk_term $sb, $crate::token::TokenDescription::Move)
    };

    (@inner $sb:expr, mut) => {
        quote!(@mk_term $sb, $crate::token::TokenDescription::Mut)
    };

    (@inner $sb:expr, pub) => {
        quote!(@mk_term $sb, $crate::token::TokenDescription::Pub)
    };

    (@inner $sb:expr, ref) => {
        quote!(@mk_term $sb, $crate::token::TokenDescription::Ref)
    };

    (@inner $sb:expr, return) => {
        quote!(@mk_term $sb, $crate::token::TokenDescription::Return)
    };

    (@inner $sb:expr, self) => {
        quote!(@mk_term $sb, $crate::token::TokenDescription::Self_)
    };

    (@inner $sb:expr, Self) => {
        quote!(@mk_term $sb, $crate::token::TokenDescription::SelfUpper)
    };

    (@inner $sb:expr, static) => {
        quote!(@mk_term $sb, $crate::token::TokenDescription::Static)
    };

    (@inner $sb:expr, struct) => {
        quote!(@mk_term $sb, $crate::token::TokenDescription::Struct)
    };

    (@inner $sb:expr, super) => {
        quote!(@mk_term $sb, $crate::token::TokenDescription::Super)
    };

    (@inner $sb:expr, trait) => {
        quote!(@mk_term $sb, $crate::token::TokenDescription::Trait)
    };

    (@inner $sb:expr, true) => {
        quote!(@mk_term $sb, $crate::token::TokenDescription::True)
    };

    (@inner $sb:expr, type) => {
        quote!(@mk_term $sb, $crate::token::TokenDescription::Type)
    };

    (@inner $sb:expr, unsafe) => {
        quote!(@mk_term $sb, $crate::token::TokenDescription::Unsafe)
    };

    (@inner $sb:expr, use) => {
        quote!(@mk_term $sb, $crate::token::TokenDescription::Use)
    };

    (@inner $sb:expr, where) => {
        quote!(@mk_term $sb, $crate::token::TokenDescription::Where)
    };

    (@inner $sb:expr, while) => {
        quote!(@mk_term $sb, $crate::token::TokenDescription::While)
    };

    // Keywords that are also reserved
    (@inner $sb:expr, abstract) => {
        quote!(@mk_term $sb, $crate::token::TokenDescription::Abstract)
    };

    (@inner $sb:expr, become) => {
        quote!(@mk_term $sb, $crate::token::TokenDescription::Become)
    };

    (@inner $sb:expr, box) => {
        quote!(@mk_term $sb, $crate::token::TokenDescription::Box)
    };

    (@inner $sb:expr, do) => {
        quote!(@mk_term $sb, $crate::token::TokenDescription::Do)
    };

    (@inner $sb:expr, final) => {
        quote!(@mk_term $sb, $crate::token::TokenDescription::Final)
    };

    (@inner $sb:expr, macro) => {
        quote!(@mk_term $sb, $crate::token::TokenDescription::Macro)
    };

    (@inner $sb:expr, offsetof) => {
        quote!(@mk_term $sb, $crate::token::TokenDescription::Offsetof)
    };

    (@inner $sb:expr, override) => {
        quote!(@mk_term $sb, $crate::token::TokenDescription::Override)
    };

    (@inner $sb:expr, priv) => {
        quote!(@mk_term $sb, $crate::token::TokenDescription::Priv)
    };

    (@inner $sb:expr, proc) => {
        quote!(@mk_term $sb, $crate::token::TokenDescription::Proc)
    };

    (@inner $sb:expr, sizeof) => {
        quote!(@mk_term $sb, $crate::token::TokenDescription::Sizeof)
    };

    (@inner $sb:expr, typeof) => {
        quote!(@mk_term $sb, $crate::token::TokenDescription::Typeof)
    };

    (@inner $sb:expr, unsized) => {
        quote!(@mk_term $sb, $crate::token::TokenDescription::Unsized)
    };

    (@inner $sb:expr, virtual) => {
        quote!(@mk_term $sb, $crate::token::TokenDescription::Virtual)
    };

    (@inner $sb:expr, yield) => {
        quote!(@mk_term $sb, $crate::token::TokenDescription::Yield)
    };

    (@inner $sb:expr, await) => {
        quote!(@mk_term $sb, $crate::token::TokenDescription::Await)
    };

    (@inner $sb:expr, dyn) => {
        quote!(@mk_term $sb, $crate::token::TokenDescription::Dyn)
    };

    (@inner $sb:expr, abstract) => {
        quote!(@mk_term $sb, $crate::token::TokenDescription::Abstract)
    };

    (@inner $sb:expr, catch) => {
        quote!(@mk_term $sb, $crate::token::TokenDescription::Catch)
    };

    (@inner $sb:expr, final) => {
        quote!(@mk_term $sb, $crate::token::TokenDescription::Final)
    };

    (@inner $sb:expr, macro) => {
        quote!(@mk_term $sb, $crate::token::TokenDescription::Macro)
    };

    (@inner $sb:expr, override) => {
        quote!(@mk_term $sb, $crate::token::TokenDescription::Override)
    };

    (@inner $sb:expr, priv) => {
        quote!(@mk_term $sb, $crate::token::TokenDescription::Priv)
    };

    (@inner $sb:expr, try) => {
        quote!(@mk_term $sb, $crate::token::TokenDescription::Try)
    };

    (@inner $sb:expr, union) => {
        quote!(@mk_term $sb, $crate::token::TokenDescription::Union)
    };

    (@inner $sb:expr, $id:ident) => {
        quote!(@mk_term $sb, $crate::token::TokenDescription::Ident)
    };

    // Punctuates
    (@inner $sb:expr, +) => {
        quote!(@mk_term $sb, $crate::token::TokenDescription::Plus)
    };

    (@inner $sb:expr, -) => {
        quote!(@mk_term $sb, $crate::token::TokenDescription::Minus)
    };

    (@inner $sb:expr, *) => {
        quote!(@mk_term $sb, $crate::token::TokenDescription::Star)
    };

    (@inner $sb:expr, /) => {
        quote!(@mk_term $sb, $crate::token::TokenDescription::Slash)
    };

    (@inner $sb:expr, %) => {
        quote!(@mk_term $sb, $crate::token::TokenDescription::Percent)
    };

    (@inner $sb:expr, ^) => {
        quote!(@mk_term $sb, $crate::token::TokenDescription::Caret)
    };

    (@inner $sb:expr, !) => {
        quote!(@mk_term $sb, $crate::token::TokenDescription::Not)
    };

    (@inner $sb:expr, &) => {
        quote!(@mk_term $sb, $crate::token::TokenDescription::And)
    };

    (@inner $sb:expr, |) => {
        quote!(@mk_term $sb, $crate::token::TokenDescription::Or)
    };

    (@inner $sb:expr, &&) => {
        quote!(@mk_term $sb, $crate::token::TokenDescription::AndAnd)
    };

    (@inner $sb:expr, ||) => {
        quote!(@mk_term $sb, $crate::token::TokenDescription::OrOr)
    };

    (@inner $sb:expr, <<) => {
        quote!(@mk_term $sb, $crate::token::TokenDescription::Shl)
    };

    (@inner $sb:expr, >>) => {
        quote!(@mk_term $sb, $crate::token::TokenDescription::Shr)
    };

    (@inner $sb:expr, +=) => {
        quote!(@mk_term $sb, $crate::token::TokenDescription::PlusEquals)
    };

    (@inner $sb:expr, -=) => {
        quote!(@mk_term $sb, $crate::token::TokenDescription::MinusEquals)
    };

    (@inner $sb:expr, *=) => {
        quote!(@mk_term $sb, $crate::token::TokenDescription::StarEquals)
    };

    (@inner $sb:expr, /=) => {
        quote!(@mk_term $sb, $crate::token::TokenDescription::SlashEquals)
    };

    (@inner $sb:expr, %=) => {
        quote!(@mk_term $sb, $crate::token::TokenDescription::PercentEquals)
    };

    (@inner $sb:expr, ^=) => {
        quote!(@mk_term $sb, $crate::token::TokenDescription::CaretEquals)
    };

    (@inner $sb:expr, &=) => {
        quote!(@mk_term $sb, $crate::token::TokenDescription::AndEquals)
    };

    (@inner $sb:expr, |=) => {
        quote!(@mk_term $sb, $crate::token::TokenDescription::OrEquals)
    };

    (@inner $sb:expr, <<=) => {
        quote!(@mk_term $sb, $crate::token::TokenDescription::ShlEquals)
    };

    (@inner $sb:expr, >>=) => {
        quote!(@mk_term $sb, $crate::token::TokenDescription::ShrEquals)
    };

    (@inner $sb:expr, =) => {
        quote!(@mk_term $sb, $crate::token::TokenDescription::Equals)
    };

    (@inner $sb:expr, ==) => {
        quote!(@mk_term $sb, $crate::token::TokenDescription::EqualsEquals)
    };

    (@inner $sb:expr, !=) => {
        quote!(@mk_term $sb, $crate::token::TokenDescription::NotEquals)
    };

    (@inner $sb:expr, >) => {
        quote!(@mk_term $sb, $crate::token::TokenDescription::GreaterThan)
    };

    (@inner $sb:expr, <) => {
        quote!(@mk_term $sb, $crate::token::TokenDescription::LessThan)
    };

    (@inner $sb:expr, >=) => {
        quote!(@mk_term $sb, $crate::token::TokenDescription::GreaterThanEquals)
    };

    (@inner $sb:expr, <=) => {
        quote!(@mk_term $sb, $crate::token::TokenDescription::LessThanEquals)
    };

    (@inner $sb:expr, @) => {
        quote!(@mk_term $sb, $crate::token::TokenDescription::At)
    };

    (@inner $sb:expr, _) => {
        quote!(@mk_term $sb, $crate::token::TokenDescription::Underscore)
    };

    (@inner $sb:expr, .) => {
        quote!(@mk_term $sb, $crate::token::TokenDescription::Dot)
    };

    (@inner $sb:expr, ..) => {
        quote!(@mk_term $sb, $crate::token::TokenDescription::DotDot)
    };

    (@inner $sb:expr, ...) => {
        quote!(@mk_term $sb, $crate::token::TokenDescription::DotDotDot)
    };

    (@inner $sb:expr, ..=) => {
        quote!(@mk_term $sb, $crate::token::TokenDescription::DotDotEquals)
    };

    (@inner $sb:expr, ,) => {
        quote!(@mk_term $sb, $crate::token::TokenDescription::Comma)
    };

    (@inner $sb:expr, ; ) => {
        quote!(@mk_term $sb, $crate::token::TokenDescription::Semicolon)
    };

    (@inner $sb:expr, : ) => {
        quote!(@mk_term $sb, $crate::token::TokenDescription::Colon)
    };

    (@inner $sb:expr, ::) => {
        quote!(@mk_term $sb, $crate::token::TokenDescription::ColonColon)
    };

    (@inner $sb:expr, ->) => {
        quote!(@mk_term $sb, $crate::token::TokenDescription::RightArrow)
    };

    (@inner $sb:expr, =>) => {
        quote!(@mk_term $sb, $crate::token::TokenDescription::FatArrow)
    };

    // Warning: this is equivalent to the `$` sign.
    (@inner $sb:expr, #) => {
        quote!(@mk_term $sb, $crate::token::TokenDescription::Dollar)
    };

    (@inner $sb:expr, ?) => {
        quote!(@mk_term $sb, $crate::token::TokenDescription::QuestionMark)
    };

    // Keep this rule at the end of the list, so that it does not match `true`
    // and `false`, which are keywords.
    (@inner $sb:expr, $lit:literal) => {
        quote!(
            @mk_term $sb,
            $crate::token::TokenDescription::Literal,
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
