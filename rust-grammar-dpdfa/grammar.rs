fn vis() {
    bump(Pub);
    if peek(LParen) {
        bump();

        if peek(Crate) {
            bump();
            bump(RParen);
        }
    }
}

pub fn item() {
    if peek(Pub) {
        vis();
    }

    if peek(Fn) {
        fn_item();
    } else {
        error();
    }
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

    fn_args();

    if peek(RightArrow) {
        bump();
        ty();
    }

    block();
}

fn block() {
    bump(LBrace);
    if peek(RBrace) {
        bump(RBrace);
    } else {
        stmt_inner();
        block_();
    }
}

fn block_() {
    if peek(RBrace) {
        bump(RBrace);
    } else if peek(Semicolon) {
        bump(Semicolon);
        if peek(RBrace) {
            bump(RBrace);
        } else {
            stmt_inner();
            block_();
        }
    }
}

fn stmt_inner() {
    if peek(Let) {
        bump(Let);
        pat();
        bump(Equals);
    }

    expr();
}

pub fn ty() {
    // TODO(scrabsha): this parses only identifier-ish type
    if peek(Ident) {
        bump();
    } else if peek(FragmentTy) {
        bump();
    } else {
        error();
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

fn pat() {
    if peek(Ident) {
        bump();
    } else if peek(FragmentIdent) {
        bump();
    } else if peek(FragmentPat) {
        bump();
    } else {
        error();
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
    } else if peek(Dot) {
        expr_dot_expr();
        expr_after_atom();
    }
}

fn expr_atom() {
    if peek(Return) || peek(Break) {
        expr_return_or_break();
    } else if peek(Ident) || peek(FragmentIdent) {
        // Maybe beginning of a path expression?
        expr_ident();
    } else if peek(FragmentExpr) || peek(Literal) {
        bump();
    } else if peek(If) {
        expr_if();
    } else if peek(LBracket) {
        expr_array();
    } else if peek(LBrace) {
        block();
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

fn expr_ident() {
    // TODO: call path() or sth
    if peek(Ident) || peek(FragmentIdent) {
        bump();
    } else {
        error();
    }

    if peek(ColonColon) {
        bump(ColonColon);
        expr_angle_bracketed_generic_arguments();
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
    if peek(Literal) {
        bump(Literal);
    } else if peek(FragmentLiteral) {
        bump(FragmentLiteral);
    } else if peek(Ident) {
        bump(Ident);
    } else if peek(FragmentIdent) {
        bump(FragmentIdent);
    } else if peek(LBrace) {
        block();
    } else if peek(Minus) {
        // Sorry
        minus_prefixed_literal();
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
