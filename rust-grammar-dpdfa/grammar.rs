fn vis() {
    bump(Pub);
    if peek(LParen) {
        // Avoid bumping the left paren too early because we need to handle eg
        // `struct Foo(pub (u32));`
        if peek2(Crate) || peek2(Self_) || peek2(Super) {
            bump(LParen);
            bump();
            bump(RParen);
        } else if peek2(In) {
            bump(LParen);
            bump(In);
            expr_path();
            bump(RParen);
        }
    }
}

fn vis_opt() {
    if peek(Pub) {
        vis();
    }
}

pub fn item() {
    if peek(ColonColon)
        || peek(Ident)
        || peek(FragmentIdent)
        || peek(Super)
        || peek(Self_)
        || peek(Crate)
        || peek(FragmentPath)
    {
        // Macro calls.
        expr_path();

        if peek2(LBrace) {
            macro_call_tail();
        } else {
            macro_call_tail();
            bump(Semicolon);
        }
    } else {
        vis_opt();

        if peek(Fn) {
            fn_item();
        } else if peek(Struct) {
            struct_item();
        } else {
            error();
        }
    }

    if peek() {
        item();
    }
}

fn struct_item() {
    vis_opt();
    bump(Struct);

    if peek(Ident) || peek(FragmentIdent) {
        bump();
    } else {
        error();
    }

    // TODO: where clause (warning: position changes depending on the kind of
    // struct).

    if peek(LParen) {
        bump(LParen);
        tuple_struct_fields();
        bump(RParen);
        bump(Semicolon);
    } else if peek(LBrace) {
        bump(LBrace);
        struct_fields();
        bump(RBrace);
    } else if peek(Semicolon) {
        bump(Semicolon);
    }
}

fn tuple_struct_fields() {
    if peek(RParen) {
    } else {
        tuple_struct_field();
        tuple_struct_fields_();
    }
}

fn tuple_struct_fields_() {
    if peek(RParen) {
    } else {
        bump(Comma);

        if peek(RParen) {
        } else {
            tuple_struct_field();
            tuple_struct_fields_();
        }
    }
}

fn struct_fields() {
    if peek(RBrace) {
    } else {
        struct_field();
        struct_fields_();
    }
}

fn struct_fields_() {
    if peek(RBrace) {
    } else {
        bump(Comma);

        if peek(RBrace) {
        } else {
            struct_field();
            struct_fields_();
        }
    }
}

fn tuple_struct_field() {
    vis_opt();
    ty();
}

fn struct_field() {
    vis_opt();
    if peek(Ident) || peek(FragmentIdent) {
        bump();
    } else {
        error();
    }
    bump(Colon);
    ty();
}

fn fn_item() {
    bump(Fn);

    if peek(Ident) {
        bump(Ident);
    } else if peek(FragmentIdent) {
        bump(FragmentIdent);
    } else {
        error();
    }

    // TODO: generic type parameters

    fn_args();

    if peek(RightArrow) {
        bump();
        ty();
    }

    block();
}

pub fn stmt() {
    stmt_tail();
}

fn block() {
    bump(LBrace);
    stmt_tail();
    bump(RBrace);
}

// Parses stmts until the end of the block
fn stmt_tail() {
    if peek() {
        if peek(RBrace) {
            // return
        } else if peek(Semicolon) {
            bump(Semicolon);
            stmt_tail();
        } else if peek(Let) {
            bump(Let);
            pat();
            if peek(Colon) {
                bump(Colon);
                ty();
            }
            bump(Equals);
            expr();

            // Let statements are always ended by semicolons.
            bump(Semicolon);

            stmt_tail();
        } else if peek(ColonColon)
            || peek(Ident)
            || peek(FragmentIdent)
            || peek(Super)
            || peek(Self_)
            || peek(Crate)
            || peek(FragmentPath)
        {
            // Potential macro/struct expression
            expr_path();
            if peek(Not) {
                if peek2(LBrace) {
                    macro_call_tail();

                    // TODO: make sure this is the FIRST set of macro_call_tail
                    if peek(Plus)
                        || peek(Minus)
                        || peek(Star)
                        || peek(Slash)
                        || peek(Percent)
                        || peek(And)
                        || peek(Or)
                        || peek(Caret)
                        || peek(Shl)
                        || peek(Shr)
                        || peek(EqualsEquals)
                        || peek(NotEquals)
                        || peek(GreaterThan)
                        || peek(LessThan)
                        || peek(GreaterThanEquals)
                        || peek(LessThanEquals)
                        || peek(OrOr)
                        || peek(AndAnd)
                        || peek(DotDot)
                        || peek(DotDotEquals)
                        || peek(LParen)
                        || peek(LBracket)
                        || peek(Dot)
                    {
                        expr_after_atom();
                        stmt_end_semi();
                    } else {
                        stmt_end_nosemi();
                    }
                } else {
                    macro_call_tail();
                    expr_after_atom();
                    stmt_end_semi();
                }
            } else {
                // Not a macro - that was just an expression.
                //
                // TODO: add struct creation here.
                expr_after_atom();
                stmt_end_semi();
            }
        } else {
            expr();
            stmt_end_semi();
        }
    }
}

fn stmt_end_semi() {
    if peek(Semicolon) {
        bump(Semicolon);
        stmt_tail();
    } else if peek(RBrace) {
        // return
    } else {
        error();
    }
}

fn stmt_end_nosemi() {
    if peek(RBrace) {
        // return
    } else {
        stmt_tail();
    }
}

pub fn ty() {
    // TODO(scrabsha): this parses only identifier-ish type and paths

    if peek(FragmentTy) {
        bump(FragmentTy);
    } else if peek(Underscore) {
        bump(Underscore);
    } else if peek(Ident)
        || peek(FragmentIdent)
        || peek(Self_)
        || peek(SelfUpper)
        || peek(Super)
        || peek(FragmentIdent)
    {
        ty_path();
    } else if peek(LParen) {
        // TODO: rework tuple type handling - this is terribly bad.
        bump(LParen);
        ty();
        bump(RParen);
    } else {
        error();
    }
}

fn ty_no_bounds() {
    // TODO:
    ty();
}

fn ty_path() {
    if peek(ColonColon) {
        bump(ColonColon);
    }

    ty_path_segment();

    if peek(ColonColon) {
        ty_path_();
    }

    if peek(Not) {
        macro_call_tail();
    }
}

fn ty_path_() {
    if peek(ColonColon) {
        bump(ColonColon);

        ty_path_segment();
        ty_path_();
    }
}

fn ty_path_segment() {
    path_ident_segment();

    if peek(ColonColon) {
        if peek2(LessThan) {
            bump(ColonColon);
            // TODO: rename this to generic_args or sth.
            expr_angle_bracketed_generic_arguments();
        } else if peek(LParen) {
            bump(ColonColon);
            ty_path_fn();
        }
    } else if peek(LessThan) {
        // TODO: rename this to generic_args or sth.
        expr_angle_bracketed_generic_arguments();
    } else if peek(LParen) {
        ty_path_fn();
    }
}

fn ty_path_fn() {
    bump(LParen);
    if peek(RParen) {
        bump(RParen);
    } else {
        ty_path_fn_inputs();
        bump(RParen);
    }

    if peek(RightArrow) {
        bump(RightArrow);
        ty_no_bounds();
    }
}

fn ty_path_fn_inputs() {
    ty();

    if peek(Comma) {
        bump(Comma);

        if peek(RParen) {
            bump(RParen);
        } else {
            ty_path_fn_inputs_();
        }
    }
}

fn ty_path_fn_inputs_() {
    ty();

    if peek(Comma) {
        bump(Comma);

        if peek(RParen) {
            bump(RParen);
        } else {
            ty_path_fn_inputs_();
        }
    } else {
        bump(RParen);
    }
}

fn fn_args() {
    bump(LParen);
    if peek(RParen) {
    } else {
        fn_arg();
    }

    if peek(Comma) {
        bump(Comma);
    }

    if peek(RParen) {
        bump(RParen);
    } else {
        fn_args_();
    }
}

fn fn_args_() {
    fn_arg();

    if peek(Comma) {
        bump(Comma);
    }

    if peek(RParen) {
        bump(RParen);
    }
}

pub fn pat() {
    if peek(Or) {
        bump(Or);
    }

    pat_no_top_alt();

    pat_();
}

fn pat_() {
    if peek(Or) {
        bump(Or);
        pat_no_top_alt();

        pat_();
    }
}

fn pat_no_top_alt() {
    if peek(FragmentPat) || peek(Underscore) || peek(DotDot) {
        bump();
    } else if peek(Ref) || peek(Mut) {
        pat_ident();
    } else if peek(Literal) || peek(FragmentLiteral) || peek(True) || peek(False) || peek(Minus) {
        pat_literal();
        pat_maybe_range_tail();
    } else if peek(DotDotEquals) {
        bump(DotDotEquals);
        pat_literal();
    } else if peek(ColonColon) || peek(FragmentPath) {
        pat_starting_with_path();
    } else if peek(Ident) || peek(FragmentIdent) {
        if peek2(At) {
            pat_ident();
        } else {
            pat_starting_with_path();
        }
    } else if peek(And) {
        bump(And);
        if peek(And) {
            bump(And);
        }
        pat_no_top_alt();
    } else if peek(LParen) {
        pat_tuple();
    } else if peek(LBracket) {
        pat_slice();
    } else {
        error();
    }
}

fn pat_starting_with_path() {
    expr_path();

    if peek(LBrace) {
        pat_struct_tail();
    } else if peek(LParen) {
        // Tuple struct = path expression + tuple pattern
        pat_tuple();
    } else if peek(Not) {
        macro_call_tail();
        pat_maybe_range_tail();
    } else {
        pat_maybe_range_tail();
    }
}

fn pat_maybe_range_tail() {
    if peek(DotDotEquals) {
        bump(DotDotEquals);
        if peek(Ident)
            || peek(FragmentIdent)
            || peek(ColonColon)
            || peek(FragmentPath)
            || peek(LessThan)
        {
            expr_path();
        } else {
            pat_literal();
        }
    } else if peek(DotDot) {
        bump();
    }
}

fn pat_struct_tail() {
    bump(LBrace);

    if peek(DotDot) {
        bump(DotDot);
        bump(RBrace);
    } else if peek(RBrace) {
        bump(RBrace);
    } else {
        pat_struct_field();

        if peek(RBrace) {
            bump(RBrace);
        } else {
            pat_struct_tail_();
        }
    }
}

fn pat_struct_tail_() {
    if peek(DotDot) {
        bump(DotDot);
        bump(RBrace);
    } else if peek(Comma) {
        bump(Comma);

        if peek(RBrace) {
            bump(RBrace);
        } else {
            pat_struct_field();
            pat_struct_tail_();
        }
    } else {
        bump(RBrace);
    }
}

fn pat_tuple() {
    bump(LParen);

    if peek(RParen) {
        bump(RParen);
    } else {
        pat_tuple_field();

        if peek(RParen) {
            bump(RParen);
        } else {
            pat_tuple_();
        }
    }
}

fn pat_tuple_() {
    if peek(Comma) {
        bump(Comma);

        if peek(RParen) {
            bump(RParen);
        } else {
            pat_tuple_field();
            pat_tuple_();
        }
    } else {
        bump(RParen);
    }
}

fn pat_struct_field() {
    if peek(Literal) {
        // 0: <pat>
        bump(Literal);
        bump(Colon);
        pat();
    } else if peek(Ident) {
        if peek2(Colon) {
            // { field: pat }
            bump(Ident);
            bump(Colon);
            pat();
        } else {
            // { binding }
            bump(Ident);
        }
    } else if peek(Ref) || peek(Mut) {
        pat_ident();
    }
}

fn pat_tuple_field() {
    pat();
}

fn pat_slice() {
    bump(LBracket);

    if peek(RBracket) {
        bump(RBracket);
    } else {
        pat();
        pat_slice_();
    }
}

fn pat_slice_() {
    if peek(Comma) {
        bump(Comma);

        if peek(RBracket) {
            bump(RBracket);
        } else {
            pat();
            pat_slice_();
        }
    } else {
        bump(RBracket);
    }
}

fn pat_range_bound() {
    if peek(Minus) {
        pat_literal();
    } else if peek(Literal) || peek(FragmentLiteral) {
        pat_literal();
    } else if peek(Ident) || peek(FragmentIdent) || peek(FragmentPath) || peek(LessThan) {
        expr_path();
    }
}

fn pat_literal() {
    if peek(Minus) {
        bump(Minus);
    }
    if peek(Literal) || peek(FragmentLiteral) || peek(True) || peek(False) {
        bump();
    } else {
        error();
    }
}

fn pat_ident() {
    if peek(Ref) {
        bump(Ref);
    }

    if peek(Mut) {
        bump(Mut);
    }

    if peek(Ident) {
        bump(Ident);
    } else if peek(FragmentIdent) {
        bump(FragmentIdent);
    } else {
        error();
    }

    if peek(At) {
        bump(At);
        pat_no_top_alt();
    }
}

fn fn_arg() {
    pat();
    bump(Colon);
    ty();
}

pub fn expr() {
    expr_prefixed_unary_op();

    expr_atom();

    expr_after_atom();
}

fn expr_after_atom() {
    if peek(Plus)
        || peek(Minus)
        || peek(Star)
        || peek(Slash)
        || peek(Percent)
        || peek(And)
        || peek(Or)
        || peek(Caret)
        || peek(Shl)
        || peek(Shr)
    {
        // Arithmetic expressions and logical expressions
        // https://doc.rust-lang.org/reference/expressions/operator-expr.html#arithmetic-and-logical-binary-operators
        bump();
        expr();
    } else if peek(EqualsEquals)
        || peek(NotEquals)
        || peek(GreaterThan)
        || peek(LessThan)
        || peek(GreaterThanEquals)
        || peek(LessThanEquals)
    {
        // Comparison expressions:
        // https://doc.rust-lang.org/reference/expressions/operator-expr.html#comparison-operators
        bump();
        expr();
    } else if peek(OrOr) || peek(AndAnd) {
        // Lazy Boolean Expressions
        // https://spec.ferrocene.dev/expressions.html#lazy-boolean-expressions
        bump();
        expr();
    } else if peek(DotDot) || peek(DotDotEquals) {
        bump();
        expr();
    } else if peek(LParen) {
        expr_call();
        expr_after_atom();
    } else if peek(LBracket) {
        // Index expression:
        // https://doc.rust-lang.org/reference/expressions/array-expr.html#array-and-slice-indexing-expressions
        bump(LBracket);
        expr();
        bump(RBracket);
    } else if peek(Dot) {
        expr_dot_expr();
        expr_after_atom();
    }
}

fn expr_atom() {
    if peek(Return) || peek(Break) {
        expr_return_or_break();
    } else if peek(Ident)
        || peek(Self_)
        || peek(SelfUpper)
        || peek(Super)
        || peek(Crate)
        || peek(FragmentIdent)
        || peek(ColonColon)
        || peek(LessThan)
    {
        expr_path();

        if peek(Not) {
            macro_call_tail();
        }
    } else if peek(FragmentExpr) || peek(Literal) {
        bump();
    } else if peek(If) {
        expr_if();
    } else if peek(LParen) {
        expr_tuple();
    } else if peek(LBracket) {
        expr_array();
    } else if peek(LBrace) {
        block();
    } else if peek(Loop) {
        expr_loop();
    } else if peek(While) {
        expr_while();
    } else if peek(For) {
        expr_for();
    } else {
        error();
    }
}

fn expr_return_or_break() {
    if peek(Return) {
        bump(Return);
    } else if peek(Break) {
        bump(Break);
    } else {
        error();
    }

    // https://github.com/dtolnay/syn/blob/922ea2dfe9a15f3d7305a8177d1df9e2c617a953/src/expr.rs#L1339
    // TODO: lifetimes -> labeled loops
    if peek(Async)
        || peek(Break)
        || peek(Continue)
        || peek(Crate)
        || peek(False)
        || peek(For)
        || peek(Let)
        || peek(Loop)
        || peek(Match)
        || peek(Move)
        || peek(Return)
        || peek(Self_)
        || peek(SelfUpper)
        || peek(True)
        || peek(Union)
        || peek(While)
        || peek(Yield)
        || peek(Ident)
        || peek(FragmentIdent)
        || peek(LParen)
        || peek(LBracket)
        || peek(LBrace)
        || peek(Literal)
        || peek(FragmentLiteral)
        || peek(Not)
        || peek(Star)
        || peek(Or)
        || peek(And)
        || peek(DotDot)
        || peek(LessThan)
        || peek(ColonColon)
        || peek(Pound)
        || peek(FragmentExpr)
    {
        expr_atom();
    }
}

fn expr_path() {
    if peek(FragmentPath) {
        bump(FragmentPath);
    } else if peek(LessThan) {
        expr_qualified_path();
    } else if peek(Ident)
        || peek(FragmentIdent)
        || peek(Super)
        || peek(Self_)
        || peek(SelfUpper)
        || peek(Crate)
        || peek(ColonColon)
    {
        expr_path_in();
    } else {
        error();
    }
}

fn expr_qualified_path() {
    bump(LessThan);
    ty();

    if peek(As) {
        bump(As);
        ty_path();
    }

    bump(GreaterThan);
    bump(ColonColon);

    expr_path_segment();
}

fn expr_path_in() {
    if peek(ColonColon) {
        bump(ColonColon);
    }
    expr_path_segment();

    if peek(ColonColon) {
        expr_path_in_();
    }
}

fn expr_path_in_() {
    bump(ColonColon);
    expr_path_segment();

    if peek(ColonColon) {
        expr_path_in_();
    }
}

fn expr_path_segment() {
    path_ident_segment();

    if peek(ColonColon) {
        if peek2(LessThan) {
            bump(ColonColon);
            expr_angle_bracketed_generic_arguments();
        }
    }
}

fn path_ident_segment() {
    if peek(Ident)
        || peek(FragmentIdent)
        || peek(SelfUpper)
        || peek(Self_)
        || peek(Super)
        || peek(Crate)
    {
        bump();
    } else {
        error();
    }
}

fn expr_if() {
    bump(If);
    expr();
    expr_block();

    if peek(Else) {
        bump(Else);
        expr_block();
    }
}

fn expr_array() {
    bump(LBracket);

    if peek(RBracket) {
        bump(RBracket);
    } else {
        expr();
        if peek(Semicolon) {
            bump(Semicolon);
            expr();
            bump(RBracket);
        } else {
            expr_array_();
        }
    }
}

fn expr_array_() {
    if peek(Comma) {
        bump(Comma);
        if peek(RBracket) {
            bump(RBracket);
        } else {
            expr();
            expr_array_();
        }
    } else {
        bump(RBracket);
    }
}

fn expr_block() {
    block();
}

fn expr_call() {
    bump(LParen);

    if peek(RParen) {
        bump(RParen);
    } else {
        expr();
        expr_call_();
    }
}

fn expr_call_() {
    if peek(Comma) {
        bump(Comma);
        if peek(RParen) {
            bump(RParen);
        } else {
            expr();
            expr_call_();
        }
    } else {
        bump(RParen);
    }
}

fn expr_dot_expr() {
    bump(Dot);

    if peek(Await) {
        bump(Await);
    } else if peek(Ident) {
        bump(Ident);
        expr_field_or_method();
    } else if peek(FragmentIdent) {
        bump(FragmentIdent);
        expr_field_or_method();
    } else if peek(Literal) {
        // TODO: this is currently accepted because it *could* mean tuple access
        // syntax.
        bump(Literal);
        expr_field_or_method();
    } else if peek(FragmentLiteral) {
        // TODO: this is currently accepted because it *could* mean tuple access
        // syntax.
        bump(FragmentLiteral);
        expr_field_or_method();
    } else {
        error();
    }
}

fn expr_field_or_method() {
    if peek(ColonColon) {
        bump(ColonColon);
        expr_angle_bracketed_generic_arguments();
        expr_call();
    }
    if peek(LParen) {
        expr_call();
    }
}

fn expr_angle_bracketed_generic_arguments() {
    bump(LessThan);
    if peek(GreaterThan) {
        bump(GreaterThan);
    } else {
        expr_generic_argument();
        expr_angle_bracketed_generic_arguments_();
    }
}

fn expr_angle_bracketed_generic_arguments_() {
    if peek(GreaterThan) {
        bump(GreaterThan);
    } else if peek(Comma) {
        bump(Comma);
        if peek(GreaterThan) {
            bump(GreaterThan);
        } else {
            expr_generic_argument();
            expr_angle_bracketed_generic_arguments_();
        }
    }
}

fn expr_generic_argument() {
    if peek(Literal) || peek(FragmentLiteral) {
        bump();
    } else if peek(Minus) {
        // Sorry
        minus_prefixed_literal();
    } else if peek(FragmentIdent) || peek(Ident) {
        if peek2(Equals) {
            // Iterator<Item = Foo>.
            bump();
            bump(Equals);
        }
        ty();
    } else if peek(LBrace) {
        block();
    } else {
        ty();
    }
}

fn minus_prefixed_literal() {
    if peek(Minus) {
        bump(Minus);
        minus_prefixed_literal();
    } else if peek(Literal) {
        bump(Literal);
    } else if peek(FragmentLiteral) {
        bump(FragmentLiteral);
    } else {
        error();
    }
}

fn expr_prefixed_unary_op() {
    // TODO: `!` operator, ...
}

fn expr_tuple() {
    // We parse tuple expressions and grouped expressions here
    // https://doc.rust-lang.org/reference/expressions/tuple-expr.html#tuple-expressions
    // https://doc.rust-lang.org/reference/expressions/grouped-expr.html#grouped-expressions
    bump(LParen);

    if peek(RParen) {
        bump(RParen);
    } else {
        expr();
        expr_tuple_();
    }
}

fn expr_tuple_() {
    if peek(RParen) {
        bump(RParen);
    } else {
        bump(Comma);

        if peek(RParen) {
            bump(RParen);
        } else {
            expr();
            expr_tuple_();
        }
    }
}

fn expr_loop() {
    bump(Loop);
    block();
}

fn expr_while() {
    bump(While);
    // TODO: we must not allow struct expressions here
    expr();
    block();
}

fn expr_for() {
    bump(For);
    pat();
    bump(In);
    // TODO: we must not allow struct expressions here
    expr();
    block();
}

fn macro_call_tail() {
    // TODO(hypothesis): all paths are path expressions.
    //
    // This is technically incorrect because macro calls must use SimplePaths
    // (which does not include generics).
    //
    // We do it that way because we have no way to detect ahead of time that a
    // path is a SimplePath or an ExprPath, and doing a state machine that
    // transitions from SimplePath to ExprPath when a generic is reached looks
    // slightly too overkill.
    //
    // THIS MUST BE DOCUMENTED.
    bump(Not);

    token_stream_group();
}

fn token_stream_group() {
    if peek(LParen) {
        token_stream_group_paren();
    } else if peek(LBracket) {
        token_stream_group_bracket();
    } else if peek(LBrace) {
        token_stream_group_brace();
    } else {
        error();
    }
}

fn token_stream_group_or_token() {
    if peek(LParen) || peek(LBracket) || peek(LBrace) {
        token_stream_group();
    } else if peek(RParen) || peek(RBracket) || peek(RBrace) {
        error();
    } else {
        bump();
    }
}

fn token_stream_group_paren() {
    bump(LParen);
    token_stream_group_paren_();
}

fn token_stream_group_paren_() {
    if peek(RParen) {
        bump(RParen);
    } else {
        token_stream_group_or_token();
        token_stream_group_paren_();
    }
}

fn token_stream_group_bracket() {
    bump(LBracket);
    token_stream_group_bracket_();
}

fn token_stream_group_bracket_() {
    if peek(RBracket) {
        bump(RBracket);
    } else {
        token_stream_group_or_token();
        token_stream_group_bracket_();
    }
}

fn token_stream_group_brace() {
    bump(LBrace);
    token_stream_group_brace_();
}

fn token_stream_group_brace_() {
    if peek(RBrace) {
        bump(RBrace);
    } else {
        token_stream_group_or_token();
        token_stream_group_brace_();
    }
}
