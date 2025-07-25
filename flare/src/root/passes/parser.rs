use ariadne::{sources, Color, Label, Report, ReportKind};
use chumsky::{input::BorrowInput, pratt::*, prelude::*};
use ordered_float::OrderedFloat;
use std::{env, fmt, fs};


use crate::root::resource::cst;
// Tokens and lexer

#[derive(Debug, Clone, PartialEq)]
pub enum Token<'src> {
    Ident(&'src str),
    Num(f64),
    Parens(Vec<Spanned<Self>>),

    // Ops
    Eq,
    Plus,
    Asterisk,

    // Keywords
    Let,
    In,
    Fn,
    As,
    True,
    False,
}

impl fmt::Display for Token<'_> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Token::Ident(x) => write!(f, "{x}"),
            Token::Num(x) => write!(f, "{x}"),
            Token::Parens(_) => write!(f, "(...)"),
            Token::Eq => write!(f, "="),
            Token::Plus => write!(f, "+"),
            Token::Asterisk => write!(f, "*"),
            Token::Let => write!(f, "let"),
            Token::In => write!(f, "in"),
            Token::Fn => write!(f, "fn"),
            Token::As => write!(f, "as"),
            Token::True => write!(f, "true"),
            Token::False => write!(f, "false"),
        }
    }
}

pub type Spanned<T> = (T, SimpleSpan);

fn lexer<'src>(
) -> impl Parser<'src, &'src str, Vec<Spanned<Token<'src>>>, extra::Err<Rich<'src, char>>> {
    recursive(|token| {
        choice((
            // Keywords
            text::ident().map(|s| match s {
                "let" => Token::Let,
                "in" => Token::In,
                "fn" => Token::Fn,
                "true" => Token::True,
                "false" => Token::False,
                s => Token::Ident(s),
            }),
            // Operators
            just("=").to(Token::Eq),
            just("+").to(Token::Plus),
            just("*").to(Token::Asterisk),
            // Numbers
            text::int(10)
                .then(just('.').then(text::digits(10)).or_not())
                .to_slice()
                .map(|s: &str| Token::Num(s.parse().unwrap())),
            token
                .repeated()
                .collect()
                .delimited_by(just('('), just(')'))
                .labelled("token tree")
                .as_context()
                .map(Token::Parens),
        ))
        .map_with(|t, e| (t, e.span()))
        .padded()
    })
    .repeated()
    .collect()
}

// AST and parser


fn parser<'tokens, 'src: 'tokens, I, M>(
    make_input: M,
) -> impl Parser<'tokens, I, Spanned<cst::Expr>, extra::Err<Rich<'tokens, Token<'src>>>>
where
    I: BorrowInput<'tokens, Token = Token<'src>, Span = SimpleSpan>,
    // Because this function is generic over the input type, we need the caller to tell us how to create a new input,
    // `I`, from a nested token tree. This function serves that purpose.
    M: Fn(SimpleSpan, &'tokens [Spanned<Token<'src>>]) -> I + Clone + 'src,
{
    recursive(|expr| {
        let ident = select_ref! { Token::Ident(x) => *x };
        let atom = choice((
            select_ref! { Token::Num(x) => cst::Expr::Num(OrderedFloat::from(*x)) },
            just(Token::True).to(cst::Expr::Bool(true)),
            just(Token::False).to(cst::Expr::Bool(false)),
            ident.map(|arg0: &str| cst::Expr::Symbol(arg0.to_string())),
            // as x = y (in) z
            just(Token::As)
                .ignore_then(cst::Expr::Symbol(ident.to_string()))
                .then_ignore(just(Token::Eq))
                .then(expr.clone())
                // .then_ignore(just(Token::In))
                .then(expr.clone())
                .map(|((lhs, rhs), then)| cst::Expr::Assignment {
                    name: lhs,
                    value: Box::new(rhs),
                    and_in: Box::new(then),
                }),
        ));

        choice((
            atom.map_with(|expr, e| (expr, e.span())),
            // fn x y = z
            just(Token::Fn).ignore_then(
                ident.map_with(|x, e| (x, e.span())).repeated().foldr_with(
                    just(Token::Eq).ignore_then(expr.clone()),
                    |arg, body, e| {
                        (
                            Expr::Func {
                                arg: Box::new(arg),
                                body: Box::new(body),
                            },
                            e.span(),
                        )
                    },
                ),
            ),
            // ( x )
            expr.nested_in(select_ref! { Token::Parens(ts) = e => make_input(e.span(), ts) }),
        ))
        .pratt(vec![
            // Multiply
            infix(left(10), just(Token::Asterisk), |x, _, y, e| {
                (Expr::Mul(Box::new(x), Box::new(y)), e.span())
            })
            .boxed(),
            // Add
            infix(left(9), just(Token::Plus), |x, _, y, e| {
                (Expr::Add(Box::new(x), Box::new(y)), e.span())
            })
            .boxed(),
            // Calls
            infix(left(1), empty(), |x, _, y, e| {
                (
                    Expr::Apply {
                        func: Box::new(x),
                        arg: Box::new(y),
                    },
                    e.span(),
                )
            })
            .boxed(),
        ])
        .labelled("expression")
        .as_context()
    })
}

fn failure(
    msg: String,
    label: (String, SimpleSpan),
    extra_labels: impl IntoIterator<Item = (String, SimpleSpan)>,
    src: &str,
) -> ! {
    let fname = "example";
    Report::build(ReportKind::Error, (fname, label.1.into_range()))
        .with_config(ariadne::Config::new().with_index_type(ariadne::IndexType::Byte))
        .with_message(&msg)
        .with_label(
            Label::new((fname, label.1.into_range()))
                .with_message(label.0)
                .with_color(Color::Red),
        )
        .with_labels(extra_labels.into_iter().map(|label2| {
            Label::new((fname, label2.1.into_range()))
                .with_message(label2.0)
                .with_color(Color::Yellow)
        }))
        .finish()
        .print(sources([(fname, src)]))
        .unwrap();
    std::process::exit(1)
}

fn parse_failure(err: &Rich<impl fmt::Display>, src: &str) -> ! {
    failure(
        err.reason().to_string(),
        (
            err.found()
                .map(|c| c.to_string())
                .unwrap_or_else(|| "end of input".to_string()),
            *err.span(),
        ),
        err.contexts()
            .map(|(l, s)| (format!("while parsing this {l}"), *s)),
        src,
    )
}

fn make_input<'src>(
    eoi: SimpleSpan,
    toks: &'src [Spanned<Token<'src>>],
) -> impl BorrowInput<'src, Token = Token<'src>, Span = SimpleSpan> {
    toks.map(eoi, |(t, s)| (t, s))
}

pub fn parse<'src>(src: impl Into<String> + Clone) -> (cst::Expr, SimpleSpan)
    {
        let the_source = src.into();
        let tokens = lexer()
        .parse(&the_source)
        .into_result()
        .unwrap_or_else(|errs| parse_failure(&errs[0], &the_source));

    let expr: (cst::Expr, SimpleSpan) = parser(make_input)
        .parse(make_input((0..the_source.len()).into(), &tokens))
        .into_result()
        .unwrap_or_else(|errs| parse_failure(&errs[0], &the_source));
    expr.clone()
    }