use std::{
    io::{read_to_string, Write},
    process::{Child, Command, Stdio},
};

use expandable_impl::{FragmentKind, TokenDescription};
use proc_macro::{Diagnostic, Level};
use proc_macro2::Span;
use serde_json::{Deserializer, Value};

pub(crate) fn get(code: Vec<(TokenDescription, Span)>) -> Option<Diagnostic> {
    let (code, sm) = SourceMap::from_code(code);
    let rustc_error = run_rustc(code)?;
    generate_diagnostic(rustc_error, &sm)
}

struct SourceMap {
    // One token per line
    lines: Vec<(String, TokenMetadata, Span)>,
}

#[derive(Clone, Copy)]
enum TokenMetadata {
    Normal,
    FragmentBlock,
    FragmentExpr,
    FragmentIdent,
    FragmentItem,
    FragmentLifetime,
    FragmentMeta,
    FragmentPat,
    FragmentPath,
    FragmentStmt,
    FragmentTT,
    FragmentTy,
    FragmentVis,
}

impl TokenMetadata {
    fn combine(self, other: TokenMetadata) -> Option<TokenMetadata> {
        match (self, other) {
            (TokenMetadata::Normal, other) => Some(other),
            (this, TokenMetadata::Normal) => Some(this),

            _ => None,
        }
    }
}

impl SourceMap {
    fn from_code(code: Vec<(TokenDescription, Span)>) -> (String, SourceMap) {
        let lines = code
            .iter()
            .map(|(token, span)| {
                let (line, metadata) = string_of_token(token);
                (line, metadata, *span)
            })
            .collect::<Vec<_>>();

        let program = lines
            .iter()
            .flat_map(|(line, _, _)| [line.as_str(), "\n"])
            .collect::<String>();

        let program = format!("const _: () = {{\n{program}\n}};");

        let sm = SourceMap { lines };

        (program, sm)
    }

    fn resolve(&self, span: CliSpan) -> (proc_macro::Span, TokenMetadata) {
        let start_line = &self.lines[span.line_start - 2];

        let metadata = start_line.1;

        let start_span = start_line.2.unwrap();
        let start_line_len = start_line.0.chars().count();

        let span = if span.col_start > start_line_len {
            start_span.end()
        } else if span.col_end == 0 {
            start_span.start()
        } else {
            start_span
        };

        (span, metadata)
    }
}

fn string_of_token(tok: &TokenDescription) -> (String, TokenMetadata) {
    let s = match tok {
        TokenDescription::LParen => "(",
        TokenDescription::RParen => ")",
        TokenDescription::LBracket => "[",
        TokenDescription::RBracket => "]",
        TokenDescription::LBrace => "{",
        TokenDescription::RBrace => "}",
        TokenDescription::Fragment(f) => {
            return match f {
                FragmentKind::Expr => (String::from("expand + able"), TokenMetadata::FragmentExpr),
                FragmentKind::Ident => (String::from("expandable"), TokenMetadata::FragmentIdent),
                FragmentKind::Item => (
                    String::from("fn expandable() {}"),
                    TokenMetadata::FragmentItem,
                ),
                FragmentKind::Pat => (String::from("Expandable(..)"), TokenMetadata::FragmentPat),
            };
        }
        TokenDescription::Invalid => unreachable!(),
        TokenDescription::Ident => "expandable",
        TokenDescription::As => "as",
        TokenDescription::Async => "async",
        TokenDescription::Await => "await",
        TokenDescription::Break => "break",
        TokenDescription::Const => "const",
        TokenDescription::Continue => "continue",
        TokenDescription::Crate => "crate",
        TokenDescription::Dyn => "dyn",
        TokenDescription::Else => "else",
        TokenDescription::Enum => "enum",
        TokenDescription::Extern => "extern",
        TokenDescription::False => "false",
        TokenDescription::Fn => "fn",
        TokenDescription::For => "for",
        TokenDescription::If => "if",
        TokenDescription::Impl => "impl",
        TokenDescription::In => "in",
        TokenDescription::Let => "let",
        TokenDescription::Loop => "loop",
        TokenDescription::Match => "match",
        TokenDescription::Mod => "mod",
        TokenDescription::Move => "move",
        TokenDescription::Mut => "mut",
        TokenDescription::Pub => "pub",
        TokenDescription::Ref => "ref",
        TokenDescription::Return => "return",
        TokenDescription::Self_ => "self",
        TokenDescription::SelfUpper => "Self",
        TokenDescription::Static => "static",
        TokenDescription::Struct => "struct",
        TokenDescription::Super => "super",
        TokenDescription::Trait => "trait",
        TokenDescription::True => "true",
        TokenDescription::Type => "type",
        TokenDescription::Union => "union",
        TokenDescription::Unsafe => "unsafe",
        TokenDescription::Use => "use",
        TokenDescription::Where => "where",
        TokenDescription::While => "while",
        TokenDescription::Yield => "yield",
        TokenDescription::Abstract => "abstract",
        TokenDescription::Become => "become",
        TokenDescription::Box => "box",
        TokenDescription::Do => "do",
        TokenDescription::Final => "final",
        TokenDescription::Macro => "macro",
        TokenDescription::Override => "override",
        TokenDescription::Priv => "priv",
        TokenDescription::Try => "try",
        TokenDescription::Typeof => "typeof",
        TokenDescription::Unsized => "unsized",
        TokenDescription::Virtual => "virtual",
        TokenDescription::Literal => "literal",
        TokenDescription::Plus => "+",
        TokenDescription::Minus => "-",
        TokenDescription::Star => "*",
        TokenDescription::Slash => "/",
        TokenDescription::Percent => "%",
        TokenDescription::Caret => "^",
        TokenDescription::Not => "!",
        TokenDescription::And => "&",
        TokenDescription::Or => "|",
        TokenDescription::AndAnd => "&&",
        TokenDescription::OrOr => "||",
        TokenDescription::Shl => "<<",
        TokenDescription::Shr => ">>",
        TokenDescription::PlusEquals => "+=",
        TokenDescription::MinusEquals => "-=",
        TokenDescription::StarEquals => "*=",
        TokenDescription::SlashEquals => "/=",
        TokenDescription::PercentEquals => "%=",
        TokenDescription::CaretEquals => "^=",
        TokenDescription::AndEquals => "&=",
        TokenDescription::OrEquals => "|=",
        TokenDescription::ShlEquals => "<=",
        TokenDescription::ShrEquals => ">=",
        TokenDescription::Equals => "=",
        TokenDescription::EqualsEquals => "==",
        TokenDescription::NotEquals => "!=",
        TokenDescription::GreaterThan => ">",
        TokenDescription::LessThan => "<",
        TokenDescription::GreaterThanEquals => ">=",
        TokenDescription::LessThanEquals => "<=",
        TokenDescription::At => "@",
        TokenDescription::Underscore => "_",
        TokenDescription::Dot => ".",
        TokenDescription::DotDot => "..",
        TokenDescription::DotDotDot => "...",
        TokenDescription::DotDotEquals => "..=",
        TokenDescription::Comma => ",",
        TokenDescription::Semicolon => ";",
        TokenDescription::Colon => ":",
        TokenDescription::ColonColon => "::",
        TokenDescription::RightArrow => "->",
        TokenDescription::FatArrow => "=>",
        TokenDescription::Pound => "#",
        TokenDescription::Dollar => "$",
        TokenDescription::QuestionMark => "?",

        _ => panic!("Unknown token description"),
    };

    (String::from(s), TokenMetadata::Normal)
}

fn run_rustc(code: String) -> Option<RustcError> {
    let child = Command::new("rustc")
        .arg("--error-format=json")
        .arg("-Zparse-only")
        .arg("-")
        .stdin(Stdio::piped())
        .stderr(Stdio::piped())
        .spawn()
        .ok()?;

    let Child { stdin, stderr, .. } = child;
    let mut stdin = stdin.unwrap();
    let stderr = stderr.unwrap();

    stdin.write_all(code.as_bytes()).ok()?;

    drop(stdin);

    let stderr = read_to_string(stderr).unwrap();

    let mut stream = Deserializer::from_str(&stderr).into_iter::<Value>();

    let diagnostic = loop {
        let diagnostic = stream.next()?.ok()?;

        let level = diagnostic.get("level")?.as_str()?;
        if level == "error" {
            break diagnostic;
        }
    };

    let message = diagnostic.get("message")?.as_str()?.to_string();

    let primary_spans = diagnostic
        .get("spans")?
        .as_array()?
        .iter()
        .filter_map(CliSpan::of_value)
        .collect();

    let submessages = diagnostic
        .get("children")?
        .as_array()?
        .iter()
        .filter_map(|children| {
            let message = children.get("message")?.as_str()?.to_string();
            let span = children.get("spans")?.as_array()?.get(0).and_then(|span| {
                let has_replacement = span
                    .get("suggested_replacement")
                    .map(|replacement| !replacement.is_null())
                    .unwrap_or_default();

                if has_replacement {
                    CliSpan::of_value(span)
                } else {
                    None
                }
            });

            let level = DiagnosticLevel::from_value(children.get("level")?)?;

            Some((span, message, level))
        })
        .collect();

    // TODO: notes/whatever
    Some(RustcError {
        message,
        primary_spans,
        submessages,
    })
}

// fn span_is_primary(value: &Value) -> bool {
//     value
//         .get("is_primary")
//         .and_then(|value| value.as_bool())
//         .unwrap_or_default()
// }

struct RustcError {
    message: String,
    primary_spans: Vec<CliSpan>,
    submessages: Vec<(Option<CliSpan>, String, DiagnosticLevel)>,
}

enum DiagnosticLevel {
    Note,
    Help,
}

impl DiagnosticLevel {
    fn from_value(value: &Value) -> Option<DiagnosticLevel> {
        Some(match value.as_str()? {
            "note" => DiagnosticLevel::Note,
            "help" => DiagnosticLevel::Help,

            _ => return None,
        })
    }
}

/// 1-based.
#[derive(Clone, Copy, Debug, PartialEq)]
struct CliSpan {
    line_start: usize,
    col_start: usize,
    line_end: usize,
    col_end: usize,
}

impl CliSpan {
    fn of_value(value: &Value) -> Option<CliSpan> {
        let get_attr = |attr| value.get(attr).and_then(|v| v.as_u64()).map(|v| v as usize);

        let line_start = get_attr("line_start")?;
        let col_start = get_attr("column_start")?;
        let line_end = get_attr("line_end")?;
        let col_end = get_attr("column_end")?;

        Some(CliSpan {
            line_start,
            col_start,
            line_end,
            col_end,
        })
    }
}

fn generate_diagnostic(error: RustcError, sm: &SourceMap) -> Option<Diagnostic> {
    let RustcError {
        message,
        primary_spans,
        submessages,
    } = error;

    let mut diag = if primary_spans.is_empty() {
        Diagnostic::new(Level::Error, message)
    } else {
        let spans_and_metadata = primary_spans
            .into_iter()
            .map(|span| sm.resolve(span))
            .collect::<Vec<_>>();

        let spans = spans_and_metadata
            .iter()
            .map(|(s, _)| *s)
            .collect::<Vec<_>>();

        let mut metadata = TokenMetadata::Normal;
        spans_and_metadata.iter().try_for_each(|(_, metadata_)| {
            metadata
                .combine(*metadata_)
                .map(|metadata_| metadata = metadata_)
        })?;

        Diagnostic::spanned(spans, Level::Error, message)
    };

    for (span, message, kind) in submessages {
        let span = span.map(|span| sm.resolve(span));

        let new_diag = match (span, kind) {
            (None, DiagnosticLevel::Note) => diag.note(message),
            (None, DiagnosticLevel::Help) => diag.help(message),
            (Some((span, metadata)), DiagnosticLevel::Note) => diag.span_note(span, message),
            (Some((span, metadata)), DiagnosticLevel::Help) => diag.span_help(span, message),
        };

        diag = new_diag;
    }

    Some(diag)
}

fn message_fixup_from_metadata(metadata: TokenMetadata, message: impl ToString) -> String {
    todo!()
}
