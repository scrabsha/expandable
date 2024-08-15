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
            let mut parser = crate::generated::$start_state();

            #[allow(unused_mut)]
            let input = quote! {
                $( $code )*
            };

            for (idx, token) in input.into_iter().enumerate() {
                match parser.step(token, idx) {
                    Ok(_) => {},

                    Err((expected, idx)) => {
                        panic!("Failed to parse token `{:?}` at index {}. Expected {:?}", token, idx, expected);
                    }
                }
            }

            match parser.finish(42101) {
                Ok(_) => {},

                Err(e) => {
                    panic!("Failed to finish parsing: {:?}", e);
                }
            }
        }
    };
}

check_parse! {
    fn empty_function() {
        new_item,
        {
            fn foo() {}
        }
    }
}

check_parse! {
    fn moar_complex_function() {
        new_item,
        {
            pub(crate) fn foo() -> i32 {}
        }
    }
}

check_parse! {
    fn function_with_args() {
        new_item,
        {
            fn foo(a: i32, b: i32) {}
        }
    }
}

check_parse! {
    fn fn_trailing_comma() {
        new_item,
        {
            fn foo(a: u8,) {}
        }
    }
}

check_parse! {
    fn fn_trailing_comma_2() {
        new_item,
        {
            fn foo(a: u8, b: u8,) {}
        }
    }
}

check_parse! {
    fn expr_ident() {
        new_expr,
        {
            foo
        }
    }
}

check_parse! {
    fn expr_chained_binop() {
        new_expr,
        {
            foo + bar * baz / qux
        }
    }
}

check_parse! {
    fn function_without_fragment() {
        new_item,
        {
            fn foo() { a }
        }
    }
}

check_parse! {
    fn very_simple_block() {
        new_expr,
        {
            { }
        }
    }
}

check_parse! {
    fn simple_block_2() {
        new_expr,
        {
            { a }
        }
    }
}

check_parse! {
    fn array_1() {
        new_expr,
        {
            [ ]
        }
    }
}

check_parse! {
    fn array_2() {
        new_expr,
        {
            [ a ]
        }
    }
}

check_parse! {
    fn array_3() {
        new_expr,
        {
            [ a, ]
        }
    }
}

check_parse! {
    fn array_4() {
        new_expr,
        {
            [ a, b ]
        }
    }
}

check_parse! {
    fn array_5() {
        new_expr,
        {
            [ a, b, ]
        }
    }
}

check_parse! {
    fn with_repetition_plus_1() {
        new_expr,
        {
            a * c
        }
    }
}

check_parse! {
    fn with_repetition_plus_2() {
        new_expr,
        {
            a * b * c
        }
    }
}

check_parse! {
    fn with_repetition_plus_3() {
        new_expr,
        {
            a * b * b * c
        }
    }
}

check_parse! {
    fn with_repetition_plus_4() {
        new_expr,
        {
            a * b * b * b * c
        }
    }
}

check_parse! {
    #[should_panic]
    fn empty_expr() {
        new_expr,
        {}
    }
}

check_parse! {
    fn fn_call_no_arg() {
        new_expr,
        {
            function()
        }
    }
}

check_parse! {
    fn numeric_range() {
        new_expr,
        {
            {
                let (13..=19, _) = ();
            }
        }
    }
}

check_parse! {
    fn head_tail_pat() {
        new_expr,
        {
            {
                let [head, tail @ ..] = ();
            }
        }
    }
}

check_parse! {
    fn weird_path_error() {
        new_expr,
        {
            (0..10).collect::<Vec<_>>()
        }
    }
}

check_parse! {
    fn path_starting_with_crate() {
        new_expr,
        {
            self::foo()
        }
    }
}

check_parse! {
    fn braced_macro_nosemi() {
        new_expr,
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
        new_stmt,
        {
            let _ = 42;
        }
    }
}

check_parse! {
    fn stmt_without_semicolon() {
        new_expr,
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
        new_item,
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
        new_expr,
        {
            match () {
                () => {},
                () => {},
            }
        }
    }
}

check_parse! {
    fn dot_await_syntax() {
        new_expr,
        {
            42.await
        }
    }
}
