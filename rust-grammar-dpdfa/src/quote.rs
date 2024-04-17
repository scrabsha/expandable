#[macro_export]
macro_rules! quote {
    (@mk_term $sb:expr, $term:expr $(,)?) => {{
        $sb.push($term);
    }};

    (@inner $sb:expr, ( $( $tt:tt )* ) ) => {{
        $sb.push($crate::generated::TokenDescription::LParen);
        $crate::quote! { @with_sb $sb, $( $tt )* }
        $sb.push($crate::generated::TokenDescription::RParen);
    }};

    (@inner $sb:expr, { $( $tt:tt )* } ) => {{
        $sb.push($crate::generated::TokenDescription::LBrace);
        $crate::quote! { @with_sb $sb, $( $tt )* }
        $sb.push($crate::generated::TokenDescription::RBrace);
    }};

    (@inner $sb:expr, [ $( $tt:tt )* ] ) => {
        $sb.push($crate::generated::TokenDescription::LBracket);
        $crate::quote! { @with_sb $sb, $( $tt )* }
        $sb.push($crate::generated::TokenDescription::RBracket);
    };

    // Keywords
    (@inner $sb:expr, as) => {
        $crate::quote!(@mk_term $sb, $crate::generated::TokenDescription::As)
    };

    (@inner $sb:expr, async) => {
        $crate::quote!(@mk_term $sb, $crate::generated::TokenDescription::Async)
    };

    (@inner $sb:expr, break) => {
        $crate::quote!(@mk_term $sb, $crate::generated::TokenDescription::Break)
    };

    (@inner $sb:expr, const) => {
        $crate::quote!(@mk_term $sb, $crate::generated::TokenDescription::Const)
    };

    (@inner $sb:expr, continue) => {
        $crate::quote!(@mk_term $sb, $crate::generated::TokenDescription::Continue)
    };

    (@inner $sb:expr, crate) => {
        $crate::quote!(@mk_term $sb, $crate::generated::TokenDescription::Crate)
    };

    (@inner $sb:expr, else) => {
        $crate::quote!(@mk_term $sb, $crate::generated::TokenDescription::Else)
    };

    (@inner $sb:expr, enum) => {
        $crate::quote!(@mk_term $sb, $crate::generated::TokenDescription::Enum)
    };

    (@inner $sb:expr, extern) => {
        $crate::quote!(@mk_term $sb, $crate::generated::TokenDescription::Extern)
    };

    (@inner $sb:expr, false) => {
        $crate::quote!(@mk_term $sb, $crate::generated::TokenDescription::False)
    };

    (@inner $sb:expr, fn) => {
        $crate::quote!(@mk_term $sb, $crate::generated::TokenDescription::Fn)
    };

    (@inner $sb:expr, for) => {
        $crate::quote!(@mk_term $sb, $crate::generated::TokenDescription::For)
    };

    (@inner $sb:expr, if) => {
        $crate::quote!(@mk_term $sb, $crate::generated::TokenDescription::If)
    };

    (@inner $sb:expr, impl) => {
        $crate::quote!(@mk_term $sb, $crate::generated::TokenDescription::Impl)
    };

    (@inner $sb:expr, in) => {
        $crate::quote!(@mk_term $sb, $crate::generated::TokenDescription::In)
    };

    (@inner $sb:expr, let) => {
        $crate::quote!(@mk_term $sb, $crate::generated::TokenDescription::Let)
    };

    (@inner $sb:expr, loop) => {
        $crate::quote!(@mk_term $sb, $crate::generated::TokenDescription::Loop)
    };

    (@inner $sb:expr, match) => {
        $crate::quote!(@mk_term $sb, $crate::generated::TokenDescription::Match)
    };

    (@inner $sb:expr, mod) => {
        $crate::quote!(@mk_term $sb, $crate::generated::TokenDescription::Mod)
    };

    (@inner $sb:expr, move) => {
        $crate::quote!(@mk_term $sb, $crate::generated::TokenDescription::Move)
    };

    (@inner $sb:expr, mut) => {
        $crate::quote!(@mk_term $sb, $crate::generated::TokenDescription::Mut)
    };

    (@inner $sb:expr, pub) => {
        $crate::quote!(@mk_term $sb, $crate::generated::TokenDescription::Pub)
    };

    (@inner $sb:expr, ref) => {
        $crate::quote!(@mk_term $sb, $crate::generated::TokenDescription::Ref)
    };

    (@inner $sb:expr, return) => {
        $crate::quote!(@mk_term $sb, $crate::generated::TokenDescription::Return)
    };

    (@inner $sb:expr, self) => {
        $crate::quote!(@mk_term $sb, $crate::generated::TokenDescription::Self_)
    };

    (@inner $sb:expr, Self) => {
        $crate::quote!(@mk_term $sb, $crate::generated::TokenDescription::SelfUpper)
    };

    (@inner $sb:expr, static) => {
        $crate::quote!(@mk_term $sb, $crate::generated::TokenDescription::Static)
    };

    (@inner $sb:expr, struct) => {
        $crate::quote!(@mk_term $sb, $crate::generated::TokenDescription::Struct)
    };

    (@inner $sb:expr, super) => {
        $crate::quote!(@mk_term $sb, $crate::generated::TokenDescription::Super)
    };

    (@inner $sb:expr, trait) => {
        $crate::quote!(@mk_term $sb, $crate::generated::TokenDescription::Trait)
    };

    (@inner $sb:expr, true) => {
        $crate::quote!(@mk_term $sb, $crate::generated::TokenDescription::True)
    };

    (@inner $sb:expr, type) => {
        $crate::quote!(@mk_term $sb, $crate::generated::TokenDescription::Type)
    };

    (@inner $sb:expr, unsafe) => {
        $crate::quote!(@mk_term $sb, $crate::generated::TokenDescription::Unsafe)
    };

    (@inner $sb:expr, use) => {
        $crate::quote!(@mk_term $sb, $crate::generated::TokenDescription::Use)
    };

    (@inner $sb:expr, where) => {
        $crate::quote!(@mk_term $sb, $crate::generated::TokenDescription::Where)
    };

    (@inner $sb:expr, while) => {
        $crate::quote!(@mk_term $sb, $crate::generated::TokenDescription::While)
    };

    // Keywords that are also reserved
    (@inner $sb:expr, abstract) => {
        $crate::quote!(@mk_term $sb, $crate::generated::TokenDescription::Abstract)
    };

    (@inner $sb:expr, become) => {
        $crate::quote!(@mk_term $sb, $crate::generated::TokenDescription::Become)
    };

    (@inner $sb:expr, box) => {
        $crate::quote!(@mk_term $sb, $crate::generated::TokenDescription::Box)
    };

    (@inner $sb:expr, do) => {
        $crate::quote!(@mk_term $sb, $crate::generated::TokenDescription::Do)
    };

    (@inner $sb:expr, final) => {
        $crate::quote!(@mk_term $sb, $crate::generated::TokenDescription::Final)
    };

    (@inner $sb:expr, macro) => {
        $crate::quote!(@mk_term $sb, $crate::generated::TokenDescription::Macro)
    };

    (@inner $sb:expr, offsetof) => {
        $crate::quote!(@mk_term $sb, $crate::generated::TokenDescription::Offsetof)
    };

    (@inner $sb:expr, override) => {
        $crate::quote!(@mk_term $sb, $crate::generated::TokenDescription::Override)
    };

    (@inner $sb:expr, priv) => {
        $crate::quote!(@mk_term $sb, $crate::generated::TokenDescription::Priv)
    };

    (@inner $sb:expr, proc) => {
        $crate::quote!(@mk_term $sb, $crate::generated::TokenDescription::Proc)
    };

    (@inner $sb:expr, sizeof) => {
        $crate::quote!(@mk_term $sb, $crate::generated::TokenDescription::Sizeof)
    };

    (@inner $sb:expr, typeof) => {
        $crate::quote!(@mk_term $sb, $crate::generated::TokenDescription::Typeof)
    };

    (@inner $sb:expr, unsized) => {
        $crate::quote!(@mk_term $sb, $crate::generated::TokenDescription::Unsized)
    };

    (@inner $sb:expr, virtual) => {
        $crate::quote!(@mk_term $sb, $crate::generated::TokenDescription::Virtual)
    };

    (@inner $sb:expr, yield) => {
        $crate::quote!(@mk_term $sb, $crate::generated::TokenDescription::Yield)
    };

    (@inner $sb:expr, await) => {
        $crate::quote!(@mk_term $sb, $crate::generated::TokenDescription::Await)
    };

    (@inner $sb:expr, dyn) => {
        $crate::quote!(@mk_term $sb, $crate::generated::TokenDescription::Dyn)
    };

    (@inner $sb:expr, abstract) => {
        $crate::quote!(@mk_term $sb, $crate::generated::TokenDescription::Abstract)
    };

    (@inner $sb:expr, catch) => {
        $crate::quote!(@mk_term $sb, $crate::generated::TokenDescription::Catch)
    };

    (@inner $sb:expr, final) => {
        $crate::quote!(@mk_term $sb, $crate::generated::TokenDescription::Final)
    };

    (@inner $sb:expr, macro) => {
        $crate::quote!(@mk_term $sb, $crate::generated::TokenDescription::Macro)
    };

    (@inner $sb:expr, override) => {
        $crate::quote!(@mk_term $sb, $crate::generated::TokenDescription::Override)
    };

    (@inner $sb:expr, priv) => {
        $crate::quote!(@mk_term $sb, $crate::generated::TokenDescription::Priv)
    };

    (@inner $sb:expr, try) => {
        $crate::quote!(@mk_term $sb, $crate::generated::TokenDescription::Try)
    };

    (@inner $sb:expr, union) => {
        $crate::quote!(@mk_term $sb, $crate::generated::TokenDescription::Union)
    };

    (@inner $sb:expr, $id:ident) => {
        $crate::quote!(@mk_term $sb, $crate::generated::TokenDescription::Ident)
    };

    // Punctuates
    (@inner $sb:expr, +) => {
        $crate::quote!(@mk_term $sb, $crate::generated::TokenDescription::Plus)
    };

    (@inner $sb:expr, -) => {
        $crate::quote!(@mk_term $sb, $crate::generated::TokenDescription::Minus)
    };

    (@inner $sb:expr, *) => {
        $crate::quote!(@mk_term $sb, $crate::generated::TokenDescription::Star)
    };

    (@inner $sb:expr, /) => {
        $crate::quote!(@mk_term $sb, $crate::generated::TokenDescription::Slash)
    };

    (@inner $sb:expr, %) => {
        $crate::quote!(@mk_term $sb, $crate::generated::TokenDescription::Percent)
    };

    (@inner $sb:expr, ^) => {
        $crate::quote!(@mk_term $sb, $crate::generated::TokenDescription::Caret)
    };

    (@inner $sb:expr, !) => {
        $crate::quote!(@mk_term $sb, $crate::generated::TokenDescription::Not)
    };

    (@inner $sb:expr, &) => {
        $crate::quote!(@mk_term $sb, $crate::generated::TokenDescription::And)
    };

    (@inner $sb:expr, |) => {
        $crate::quote!(@mk_term $sb, $crate::generated::TokenDescription::Or)
    };

    (@inner $sb:expr, &&) => {
        $crate::quote!(@mk_term $sb, $crate::generated::TokenDescription::AndAnd)
    };

    (@inner $sb:expr, ||) => {
        $crate::quote!(@mk_term $sb, $crate::generated::TokenDescription::OrOr)
    };

    (@inner $sb:expr, <<) => {
        $crate::quote!(@mk_term $sb, $crate::generated::TokenDescription::Shl)
    };

    (@inner $sb:expr, >>) => {
        $crate::quote!(@mk_term $sb, $crate::generated::TokenDescription::Shr)
    };

    (@inner $sb:expr, +=) => {
        $crate::quote!(@mk_term $sb, $crate::generated::TokenDescription::PlusEquals)
    };

    (@inner $sb:expr, -=) => {
       $crate::quote!(@mk_term $sb, $crate::generated::TokenDescription::MinusEquals)
    };

    (@inner $sb:expr, *=) => {
        $crate::quote!(@mk_term $sb, $crate::generated::TokenDescription::StarEquals)
    };

    (@inner $sb:expr, /=) => {
        $crate::quote!(@mk_term $sb, $crate::generated::TokenDescription::SlashEquals)
    };

    (@inner $sb:expr, %=) => {
        $crate::quote!(@mk_term $sb, $crate::generated::TokenDescription::PercentEquals)
    };

    (@inner $sb:expr, ^=) => {
        $crate::quote!(@mk_term $sb, $crate::generated::TokenDescription::CaretEquals)
    };

    (@inner $sb:expr, &=) => {
        $crate::quote!(@mk_term $sb, $crate::generated::TokenDescription::AndEquals)
    };

    (@inner $sb:expr, |=) => {
        $crate::quote!(@mk_term $sb, $crate::generated::TokenDescription::OrEquals)
    };

    (@inner $sb:expr, <<=) => {
        $crate::quote!(@mk_term $sb, $crate::generated::TokenDescription::ShlEquals)
    };

    (@inner $sb:expr, >>=) => {
        $crate::quote!(@mk_term $sb, $crate::generated::TokenDescription::ShrEquals)
    };

    (@inner $sb:expr, =) => {
        $crate::quote!(@mk_term $sb, $crate::generated::TokenDescription::Equals)
    };

    (@inner $sb:expr, ==) => {
        $crate::quote!(@mk_term $sb, $crate::generated::TokenDescription::EqualsEquals)
    };

    (@inner $sb:expr, !=) => {
        $crate::quote!(@mk_term $sb, $crate::generated::TokenDescription::NotEquals)
    };

    (@inner $sb:expr, >) => {
        $crate::quote!(@mk_term $sb, $crate::generated::TokenDescription::GreaterThan)
    };

    (@inner $sb:expr, <) => {
        $crate::quote!(@mk_term $sb, $crate::generated::TokenDescription::LessThan)
    };

    (@inner $sb:expr, >=) => {
        $crate::quote!(@mk_term $sb, $crate::generated::TokenDescription::GreaterThanEquals)
    };

    (@inner $sb:expr, <=) => {
        $crate::quote!(@mk_term $sb, $crate::generated::TokenDescription::LessThanEquals)
    };

    (@inner $sb:expr, @) => {
        $crate::quote!(@mk_term $sb, $crate::generated::TokenDescription::At)
    };

    (@inner $sb:expr, _) => {
        $crate::quote!(@mk_term $sb, $crate::generated::TokenDescription::Underscore)
    };

    (@inner $sb:expr, .) => {
        $crate::quote!(@mk_term $sb, $crate::generated::TokenDescription::Dot)
    };

    (@inner $sb:expr, ..) => {
        $crate::quote!(@mk_term $sb, $crate::generated::TokenDescription::DotDot)
    };

    (@inner $sb:expr, ...) => {
        $crate::quote!(@mk_term $sb, $crate::generated::TokenDescription::DotDotDot)
    };

    (@inner $sb:expr, ..=) => {
        $crate::quote!(@mk_term $sb, $crate::generated::TokenDescription::DotDotEquals)
    };

    (@inner $sb:expr, ,) => {
        $crate::quote!(@mk_term $sb, $crate::generated::TokenDescription::Comma)
    };

    (@inner $sb:expr, ; ) => {
        $crate::quote!(@mk_term $sb, $crate::generated::TokenDescription::Semicolon)
    };

    (@inner $sb:expr, : ) => {
        $crate::quote!(@mk_term $sb, $crate::generated::TokenDescription::Colon)
    };

    (@inner $sb:expr, ::) => {
        $crate::quote!(@mk_term $sb, $crate::generated::TokenDescription::ColonColon)
    };

    (@inner $sb:expr, ->) => {
        $crate::quote!(@mk_term $sb, $crate::generated::TokenDescription::RightArrow)
    };

    (@inner $sb:expr, =>) => {
        $crate::quote!(@mk_term $sb, $crate::generated::TokenDescription::FatArrow)
    };

    // Warning: this is equivalent to the `$` sign.
    (@inner $sb:expr, #) => {
        $crate::quote!(@mk_term $sb, $crate::generated::TokenDescription::Dollar)
    };

    (@inner $sb:expr, ?) => {
        $crate::quote!(@mk_term $sb, $crate::generated::TokenDescription::QuestionMark)
    };

    // Keep this rule at the end of the list, so that it does not match `true`
    // and `false`, which are keywords.
    (@inner $sb:expr, $lit:literal) => {
        $crate::quote!(
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
            $crate::quote!(@inner $sb, $tt);
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
        $crate::quote! { @with_sb container, $( $tt )* };
        container
    }};
}
