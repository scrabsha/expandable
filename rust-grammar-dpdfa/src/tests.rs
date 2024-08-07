use crate::generated::RustParser;
macro_rules! check_parse {
    (
        $( #[$attr:meta] )*
        fn $test_name:ident() {
            $start_state:ident,

            {
                $( $code:tt )*
            }
        }
    ) => {
        #[test]
        $( #[$attr] )*
        fn $test_name() {
            let mut parser = RustParser::$start_state();

            #[allow(unused_mut)]
            let input = quote! {
                $( $code )*
            };

            for (idx, token) in input.into_iter().enumerate() {
                match parser.step(token, idx) {
                    Ok(_) => {},

                    Err((idx, expected)) => {
                        panic!("Failed to parse token `{:?}` at index {}. Expected {:?}", token, idx, expected);
                    }
                }
            }

            match parser.finish() {
                Ok(()) => {},

                Err(e) => {
                    panic!("Failed to finish parsing: {:?}", e);
                }
            }
        }
    };
}

check_parse! {
    fn empty_function() {
        item,
        {
            fn foo() {}
        }
    }
}

check_parse! {
    fn moar_complex_function() {
        item,
        {
            pub(crate) fn foo() -> i32 {}
        }
    }
}

check_parse! {
    fn function_with_args() {
        item,
        {
            fn foo(a: i32, b: i32) {}
        }
    }
}

check_parse! {
    fn fn_trailing_comma() {
        item,
        {
            fn foo(a: u8,) {}
        }
    }
}

check_parse! {
    fn fn_trailing_comma_2() {
        item,
        {
            fn foo(a: u8, b: u8,) {}
        }
    }
}

check_parse! {
    fn expr_ident() {
        expr,
        {
            foo
        }
    }
}

check_parse! {
    fn expr_chained_binop() {
        expr,
        {
            foo + bar * baz / qux
        }
    }
}

check_parse! {
    fn function_without_fragment() {
        item,
        {
            fn foo() { a }
        }
    }
}

check_parse! {
    fn very_simple_block() {
        expr,
        {
            { }
        }
    }
}

check_parse! {
    fn simple_block_2() {
        expr,
        {
            { a }
        }
    }
}

check_parse! {
    fn array_1() {
        expr,
        {
            [ ]
        }
    }
}

check_parse! {
    fn array_2() {
        expr,
        {
            [ a ]
        }
    }
}

check_parse! {
    fn array_3() {
        expr,
        {
            [ a, ]
        }
    }
}

check_parse! {
    fn array_4() {
        expr,
        {
            [ a, b ]
        }
    }
}

check_parse! {
    fn array_5() {
        expr,
        {
            [ a,b, ]
        }
    }
}

check_parse! {
    fn with_repetition_plus_1() {
        expr,
        {
            a * c
        }
    }
}

check_parse! {
    fn with_repetition_plus_2() {
        expr,
        {
            a * b * c
        }
    }
}

check_parse! {
    fn with_repetition_plus_3() {
        expr,
        {
            a * b * b * c
        }
    }
}

check_parse! {
    fn with_repetition_plus_4() {
        expr,
        {
            a * b * b * b * c
        }
    }
}

check_parse! {
    #[should_panic]
    fn empty_expr() {
        expr,
        {}
    }
}

check_parse! {
    fn fn_call_no_arg() {
        expr,
        {
            function()
        }
    }
}

check_parse! {
    fn numeric_range() {
        expr,
        {
            {
                let (13..=19, _) = ();
            }
        }
    }
}

check_parse! {
    fn head_tail_pat() {
        expr,
        {
            {
                let [head, tail @ ..] = ();
            }
        }
    }
}

check_parse! {
    fn weird_path_error() {
        expr,
        {
            (0..10).collect::<Vec<_>>()
        }
    }
}

check_parse! {
    fn path_starting_with_crate() {
        expr,
        {
            self::foo()
        }
    }
}

check_parse! {
    fn braced_macro_nosemi() {
        expr,
        {
            {
                macro_call! {}
                42
            }
        }
    }
}

check_parse! {
    fn stmt_entry_point() {
        stmt,
        {
            let _ = 42;
        }
    }
}

check_parse! {
    fn stmt_without_semicolon() {
        expr,
        {
            {
                loop {}
                42
            }
        }
    }
}

check_parse! {
    fn something_is_wrong_with_block_initialization() {
        item,
        {
            fn foo() {
                loop {}
                42
            }
        }
    }
}

check_parse! {
    fn match_arms_are_broken() {
        expr,
        {
            match () {
                () => {},
                () => {},
            }
        }
    }
}
