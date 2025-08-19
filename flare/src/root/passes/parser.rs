use ariadne::{sources, Color, Label, Report, ReportKind};
use chumsky::input::BorrowInput;
use chumsky::pratt::*;
use chumsky::prelude::*;

use crate::root::passes::midend::environment::Quantifier;

#[derive(Debug, Clone, PartialEq)]
pub enum Token<'src> {
    Ident(&'src str),
    Num(f64),
    Strlit(&'src str),
    Comment(&'src str),
    Parens(Vec<Spanned<Self>>),

    Separator,
    Whitespace,

    Colon,
    Eq,
    Dot,
    FatArrow,
    Comma,

    Asterisk,
    Slash,
    Plus,
    Minus,

    LBracket,
    RBracket,
    LParen,
    RParen,

    Let,
    In,
    Package,
    Fn,
    Use,
    Struct,
    True,
    False,
}

impl std::fmt::Display for Token<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            Token::Ident(x) => write!(f, "{x}"),
            Token::Num(x) => write!(f, "{x}"),
            Token::Eq => write!(f, "="),
            Token::Let => write!(f, "let"),
            Token::In => write!(f, "in"),

            Token::Parens(_) => write!(f, "(...)"),

            Token::Asterisk => write!(f, "*"),
            Token::Slash => write!(f, "/"),
            Token::Plus => write!(f, "+"),
            Token::Minus => write!(f, "-"),

            Token::Fn => write!(f, "fn"),
            Token::True => write!(f, "true"),
            Token::False => write!(f, "false"),
            Token::Strlit(s) => write!(f, "\"{s}\""),
            Token::Comment(c) => write!(f, "{c}"),
            Token::Colon => write!(f, ":"),

            Token::Separator => write!(f, "newline"),
            Token::Whitespace => write!(f, "whitespace"),

            Token::Dot => write!(f, "."),
            Token::FatArrow => write!(f, "=>"),
            Token::Comma => write!(f, ","),
            Token::LBracket => write!(f, "{{"),
            Token::RBracket => write!(f, "}}"),
            Token::LParen => write!(f, "("),
            Token::RParen => write!(f, ")"),
            Token::Package => write!(f, "package"),
            Token::Use => write!(f, "use"),
            Token::Struct => write!(f, "struct"),
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum Expr {
    Ident(String),
    Number(f64),
    String(String),
    Unit,

    Mul(Box<Spanned<Self>>, Box<Spanned<Self>>),
    Div(Box<Spanned<Self>>, Box<Spanned<Self>>),
    Add(Box<Spanned<Self>>, Box<Spanned<Self>>),
    Sub(Box<Spanned<Self>>, Box<Spanned<Self>>),
    Bool(bool),

    FieldAccess(Box<Spanned<Self>>, Box<Spanned<Self>>),
    Access(Box<Spanned<Self>>),
    Call(Box<Spanned<Self>>, Box<Spanned<Self>>),
    Lambda(Box<Spanned<Self>>, Box<Spanned<Self>>),
    Struct(Vec<(Box<Spanned<Self>>, Spanned<Self>)>),
    Tuple(Vec<Spanned<Self>>),
    Let(Box<Spanned<Self>>, Box<Spanned<Self>>, Box<Spanned<Self>>),
}

impl Expr {
    pub fn get_ident(&self) -> Option<Quantifier> {
        match self {
            Expr::Ident(ref s) => Some(Quantifier::Variable(s.to_string())),
            Expr::FieldAccess(ref base, ref field) => {
                Some(base.0.get_ident()?.append(field.0.get_ident()?))
            }
            Expr::Access(ref expr) => expr.0.get_ident(),
            Expr::Call(ref func, _) => func.0.get_ident(),
            Expr::Lambda(ref arg, _) => arg.0.get_ident(),
            _ => None,
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct StructDef {
    pub name: Spanned<Expr>,
    pub fields: Vec<(Spanned<Expr>, Spanned<Expr>)>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct ImportItem {
    pub items: Vec<Vec<Spanned<Expr>>>,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Definition {
    Import(ImportItem),
    Struct(StructDef),
    Let(Spanned<Expr>, Spanned<Expr>),
}

#[derive(Debug, Clone, PartialEq)]
pub struct Package {
    pub name: Spanned<Expr>,
    pub items: Vec<Definition>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Program {
    pub packages: Vec<Package>,
}

pub type Spanned<T> = (T, SimpleSpan<usize>);

fn lexer<'src>(
) -> impl Parser<'src, &'src str, Vec<Spanned<Token<'src>>>, extra::Err<Rich<'src, char>>> {
    let ws = one_of(" \t")
                .repeated().at_least(1)
                .ignored()
                .labelled("whitespace")
                .to(Token::Whitespace);

    recursive(|token| {
        choice((
            ws, 
            text::ident().map(|s| match s {
                "let" => Token::Let,
                "in" => Token::In,
                "fn" => Token::Fn,
                "true" => Token::True,
                "false" => Token::False,
                "package" => Token::Package,
                "use" => Token::Use,
                "struct" => Token::Struct,
                "in" => Token::In,

                s => Token::Ident(s),
            }),
            // Operators
            just("=>").to(Token::FatArrow),
            just("=").to(Token::Eq),
            just("*").to(Token::Asterisk),
            just("/").to(Token::Slash),
            just("+").to(Token::Plus),
            just("-").to(Token::Minus),
            just("{").to(Token::LBracket),
            just("}").to(Token::RBracket),
            //             just("(").to(Token::LParen),
            // just(")").to(Token::RParen),
            just(".").to(Token::Dot),
            just(",").to(Token::Comma),
            just(":").to(Token::Colon),
            // Numbers
            text::int(10)
                .then(just('.').then(text::digits(10)).or_not())
                .to_slice()
                .map(|s: &str| Token::Num(s.parse().unwrap())),
            text::ident()
                .delimited_by(just('"'), just('"'))
                .labelled("string literal")
                .map(Token::Strlit),
            // Comment
            just("#")
                .then_ignore(any().and_is(just('\n').not()).repeated())
                .labelled("comment")
                .map(Token::Comment),
            token
                .repeated()
                .collect()
                .delimited_by(just('('), just(')'))
                .labelled("token tree")
                .as_context()
                .map(Token::Parens),

                            just("\n")
                .or(just("\r\n"))
                .labelled("newline")
                .to(Token::Separator),

        ))
        .map_with(|t, e| (t, e.span()))
    })
    .padded()
    .repeated()
    .collect()
}

fn parser<'tokens, 'src: 'tokens, I, M>(
    make_input: M,
) -> impl Parser<'tokens, I, Program, extra::Err<Rich<'tokens, Token<'src>, SimpleSpan>>>
where
    I: BorrowInput<'tokens, Token = Token<'src>, Span = SimpleSpan>,
    // Because this function is generic over the input type, we need the caller to tell us how to create a new input,
    // `I`, from a nested token tree. This function serves that purpose.
    M: Fn(SimpleSpan, &'tokens [Spanned<Token<'src>>]) -> I + Clone + 'src,
{
    // Basic tokens

//    let whitespace = select_ref! { Token::Whitespace => () }.ignored();

    let ident = select_ref! { Token::Ident(x) => *x }
        .map_with(|x, e| (Expr::Ident(x.to_string()), e.span()));

    // Expression parser
    let expression = recursive(|expr| {
        let rname = select_ref! { Token::Ident(x) => *x };
        let ident = rname.map_with(|x, e| (Expr::Ident(x.to_string()), e.span()));
        let atom = choice((
            select_ref! { Token::Num(x) => Expr::Number(*x) }.map_with(|x, e| (x, e.span())),
            just(Token::True).map_with(|_, e| (Expr::Bool(true), e.span())),
            just(Token::False).map_with(|_, e| (Expr::Bool(false), e.span())),
            ident.pratt(vec![infix(right(9), just(Token::Dot), |x, _, y, e| {
                (Expr::FieldAccess(Box::new(x), Box::new(y)), e.span())
            })
            .boxed()]),
            //     .then(
            //         just(Token::Dot)
            //             .ignore_then(ident.clone())
            //             .repeated()
            //             .collect::<Vec<_>>(),
            //     )
            //     .map_with(|(base, fields), e| {
            //         fields.into_iter().fold(base, |acc, field| {
            //             (Expr::FieldAccess(Box::new(acc), Box::new(field)), e.span())
            //         })
            //     }),
            ident,
            // let x = y ; z
            just(Token::Let)
                .ignore_then(ident)
                .then_ignore(just(Token::Eq))
                .then(expr.clone())
                .then_ignore(just(Token::In))
                .then(expr.clone())
                .map_with(|((lhs, rhs), then), e| {
                    (
                        Expr::Let(Box::new(lhs), Box::new(rhs), Box::new(then)),
                        e.span(),
                    )
                }),
        ))
        .boxed();

        choice((
            atom.boxed(), //.map_with(|expr, e| (expr, e.span())),
            // fn x y = z
            just(Token::Fn).ignore_then(ident.repeated().foldr_with(
                just(Token::FatArrow).ignore_then(expr.clone()),
                |arg, body, e| (Expr::Lambda(Box::new(arg), Box::new(body)), e.span()),
            )),
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
            infix(right(10), empty(), |func, _, arg, e| {
                (Expr::Call(Box::new(func), Box::new(arg)), e.span())
            })
            .boxed(),
        ])
        .labelled("expression")
        .as_context()
    });

    let definition = recursive(|_| {
        // Toplevel Let binding
        let let_binding = just(Token::Let)
            .ignore_then(ident)
            .then(ident.repeated().foldr_with(
                just(Token::Eq).ignore_then(expression.clone()),
                |arg, body, e| (Expr::Lambda(Box::new(arg), Box::new(body)), e.span()),
            ))
            .map(|(name, value)| Definition::Let(name, value));
        // Struct definition
        let struct_field = ident
            .clone()
            .then_ignore(just(Token::Colon))
            .then(ident.clone());

        let struct_def = just(Token::Struct)
            .ignore_then(ident)
            .then_ignore(just(Token::Eq))
            .then(
                struct_field
                    .separated_by(just(Token::Comma))
                    .allow_trailing()
                    .collect::<Vec<_>>(),
            )
            .map(|(name, fields)| Definition::Struct(StructDef { name, fields }));

        // Import
        let import_path = ident
            .clone()
            .separated_by(just(Token::Dot))
            .at_least(1)
            .collect::<Vec<_>>();

        let import_items = import_path
            .clone()
            .separated_by(just(Token::Comma))
            .collect::<Vec<_>>()
            .delimited_by(just(Token::LBracket), just(Token::RBracket));

        let import = just(Token::Use)
            .ignore_then(import_items)
            .map(|items| Definition::Import(ImportItem { items }));

        choice((import, let_binding, struct_def))
            
    });

    let package = just(Token::Package)
        .ignore_then(ident)
        .then_ignore(just(Token::Eq))
        .then(definition.repeated().collect::<Vec<_>>())
        .map(|(name, items)| Package {
            name: name.to_owned(),
            items,
        });
    //
    // Program
    package
        .repeated()
        .at_least(1)
        .collect::<Vec<_>>()
        .map(|packages| Program { packages })
        .then_ignore(end())
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

fn parse_failure(err: &Rich<impl std::fmt::Display>, src: &str) -> () {
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
    );
    ()
}

fn make_input<'src>(
    eoi: SimpleSpan,
    toks: &'src [Spanned<Token<'src>>],
) -> impl BorrowInput<'src, Token = Token<'src>, Span = SimpleSpan> {
    toks.map(eoi, |(t, s)| (t, s))
}

// Public API
pub fn parse(input: &str) -> Result<Program, Vec<Rich<Token>>> {
    let tokens = lexer().parse(input).into_result().unwrap_or_else(|errs| {
        parse_failure(&errs[0], input);
        std::process::exit(1)
    });

    dbg!(&tokens);

    let packg = match parser(make_input)
        .parse(make_input((0..input.len()).into(), &tokens))
        .into_result()
    {
        Ok(p) => p,
        Err(e) => {
            let _ = e.iter().for_each(|e| parse_failure(&e, input));
            std::process::exit(1);
        }
    };

    Ok(packg)
}

// #[cfg(test)]
// mod tests {
//     use super::*;

//     #[test]
//     fn test_simple_package() {
//         let input = r#"
//             package Main =
//                 use Random
//                 let x = 42
//         "#;

//         let result = parse_program(input);
//         assert!(result.is_ok(), "Parse failed: {:?}", result.err());
//     }

//     #[test]
//     fn test_struct_def() {
//         let input = r#"
//             package Main =
//                 struct User =
//                     name: String,
//                     id: Number
//         "#;

//         let result = parse_program(input);
//         assert!(result.is_ok());
//     }

//     #[test]
//     fn test_field_access() {
//         let input = r#"
//             package Main =
//                 let result = user.name
//         "#;

//         let result = parse_program(input);
//         assert!(result.is_ok());
//     }
// }

// Example usage
// pub fn example() {
//     let code = r#"
//         package Main =
//             use Random
//             struct User =
//                 name: String,
//                 id: Number
//             let main = Random.gen_string
//     "#;

//     match parse(code) {
//         Ok(program) => println!("Success! Found {} packages", program.packages.len()),
//         Err(errors) => {
//             for error in errors {
//                 println!("Error: {}", error);
//             }
//         }
//     }
// }
