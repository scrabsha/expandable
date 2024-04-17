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
    if peek(Plus) {
        bump();
        expr();
    } else if peek(Minus) {
        bump();
        expr();
    } else if peek(Star) {
        bump();
        expr();
    } else if peek(Slash) {
        bump();
        expr();
    } else if peek(Percent) {
        bump();
        expr();
    } else if peek(Caret) {
        bump();
        expr();
    } else if peek(And) {
        bump();
        expr();
    } else if peek(Or) {
        bump();
        expr();
    } else if peek(AndAnd) {
        bump();
        expr();
    } else if peek(OrOr) {
        bump();
        expr();
    } else if peek(Shl) {
        bump();
        expr();
    } else if peek(Shr) {
        bump();
        expr();
    } else if peek(EqualsEquals) {
        bump();
        expr();
    } else if peek(NotEquals) {
        bump();
        expr();
    } else if peek(GreaterThan) {
        bump();
        expr();
    } else if peek(LessThan) {
        bump();
        expr();
    } else if peek(GreaterThanEquals) {
        bump();
        expr();
    } else if peek(LessThanEquals) {
        bump();
        expr();
    } else if peek(DotDot) {
        bump();
        expr();
    } else if peek(DotDotEquals) {
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
    if peek(Return) {
        expr_return_or_break();
    } else if peek(Break) {
        expr_return_or_break();
    } else if peek(Ident) {
        expr_ident();
    } else if peek(FragmentIdent) {
        bump();
    } else if peek(FragmentExpr) {
        bump();
    } else if peek(Literal) {
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
    if peek(Async) {
        expr_atom();
    } else if peek(Break) {
        expr_atom();
    } else if peek(Continue) {
        expr_atom();
    } else if peek(Crate) {
        expr_atom();
    } else if peek(False) {
        expr_atom();
    } else if peek(For) {
        expr_atom();
    } else if peek(Let) {
        expr_atom();
    } else if peek(Loop) {
        expr_atom();
    } else if peek(Match) {
        expr_atom();
    } else if peek(Move) {
        expr_atom();
    } else if peek(Return) {
        expr_atom();
    } else if peek(Self_) {
        expr_atom();
    } else if peek(SelfUpper) {
        expr_atom();
    } else if peek(True) {
        expr_atom();
    } else if peek(Union) {
        expr_atom();
    } else if peek(While) {
        expr_atom();
    } else if peek(Yield) {
        expr_atom();
    } else if peek(Ident) {
        expr_atom();
    } else if peek(FragmentIdent) {
        expr_atom();
    } else if peek(LParen) {
        expr_atom();
    } else if peek(LBracket) {
        expr_atom();
    } else if peek(LBrace) {
        expr_atom();
    } else if peek(Literal) {
        expr_atom();
    } else if peek(FragmentLiteral) {
        expr_atom();
    } else if peek(Not) {
        expr_atom();
    } else if peek(Star) {
        expr_atom();
    } else if peek(Or) {
        expr_atom();
    } else if peek(And) {
        expr_atom();
    } else if peek(DotDot) {
        expr_atom();
    } else if peek(LessThan) {
        expr_atom();
    } else if peek(ColonColon) {
        expr_atom();
    } else if peek(Pound) {
        expr_atom();
    } else if peek(FragmentExpr) {
        expr_atom();
    }
}

fn expr_ident() {
    // TODO: call path() or sth
    bump(Ident);
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
