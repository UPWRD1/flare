
use chumsky::input::BorrowInput;
use chumsky::pratt::*;
use chumsky::prelude::*;
use ordered_float::OrderedFloat;

use crate::root::passes::midend::environment::Quantifier;
use crate::root::resource::errors::CompResult;
use crate::root::resource::errors::CompilerErr;
use crate::root::resource::errors::DynamicErr;
use crate::root::resource::rep::Pattern;
use crate::root::resource::rep::PatternAtom;
use crate::root::resource::rep::Ty;
use crate::root::resource::rep::{Definition, Expr, ImportItem, Package, Spanned, StructDef};

/// Type representing the tokens produced by the lexer. Is private, since tokens are only used in the first stage of parsing.
#[derive(Debug, Clone, PartialEq)]
enum Token<'src> {
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
    Arrow,
    Comma,
    Pipe,

    Asterisk,
    Slash,
    Plus,
    Minus,

    LBrace,
    RBrace,
    LBracket,
    RBracket,
    LParen,
    RParen,
    Question,

    Else,
    Enum,
    Extern,
    False,
    Fn,
    If,
    In,
    Let,
    Match,
    Package,
    Pub,
    Struct,
    Then,
    True,
    Use,
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
            Token::Arrow => write!(f, "->"),
            Token::Question => write!(f, "?"),


            Token::Comma => write!(f, ","),
            Token::Pipe => write!(f, "|"),
            Token::LBrace => write!(f, "{{"),
            Token::RBrace => write!(f, "}}"),
            Token::LBracket => write!(f, "["),
            Token::RBracket => write!(f, "]"),
            Token::LParen => write!(f, "("),
            Token::RParen => write!(f, ")"),
            Token::Package => write!(f, "package"),
            Token::Use => write!(f, "use"),
            Token::Struct => write!(f, "struct"),
            Token::Else => write!(f, "else"),
            Token::Enum => write!(f, "enum"),
            Token::Extern => write!(f, "extern"),
            Token::If => write!(f, "if"),
            Token::Match => write!(f, "match"),
            Token::Pub => write!(f, "pub"),
            Token::Then => write!(f, "then"),
        }
    }
}

/// The primary lexer function.
fn lexer<'src>(
) -> impl Parser<'src, &'src str, Vec<Spanned<Token<'src>>>, extra::Err<Rich<'src, char>>> {
    let comment = just("#")
        .then_ignore(any().and_is(just('\n').not()).repeated())
        .labelled("comment")
        .padded();
    recursive(|token| {
        choice((
            text::ident().map(|s| match s {
                "else" => Token::Else,
                "enum" => Token::Enum,
                "extern" => Token::Extern,
                "false" => Token::False,
                "fn" => Token::Fn,
                "if" => Token::If,
                "in" => Token::In,
                "let" => Token::Let,
                "match" => Token::Match,
                "package" => Token::Package,
                "pub" => Token::Pub,
                "struct" => Token::Struct,
                "then" => Token::Then,
                "true" => Token::True,
                "use" => Token::Use,

                s => Token::Ident(s),
            }),
            // Operators
            just("=>").to(Token::FatArrow),
            just("->").to(Token::Arrow),
            just("=").to(Token::Eq),
            just("*").to(Token::Asterisk),
            just("/").to(Token::Slash),
            just("+").to(Token::Plus),
            just("-").to(Token::Minus),
            just("{").to(Token::LBrace),
            just("}").to(Token::RBrace),
            just("[").to(Token::LBracket),
            just("]").to(Token::RBracket),

            //             just("(").to(Token::LParen),
            // just(")").to(Token::RParen),
            just(".").to(Token::Dot),
            just(",").to(Token::Comma),
            just(":").to(Token::Colon),
            just("|").to(Token::Pipe),
            just("?").to(Token::Question),

            // Numbers
            text::int(10)
                .then(just('.').then(text::digits(10)).or_not())
                .to_slice()
                .map(|s: &str| Token::Num(s.parse().unwrap())),
            just('"')
                .ignore_then(none_of('"').repeated().to_slice())
                .then_ignore(just('"'))
                .labelled("string literal")
                .map(Token::Strlit),
            token
                .padded()
                .repeated()
                .collect()
                .delimited_by(just('('), just(')'))
                .labelled("token tree")
                .as_context()
                .map(Token::Parens),
        ))
        .map_with(|t, e| (t, e.span()))
    })
    .padded_by(comment.repeated())
    .padded()
    .repeated()
    .collect()
}

/// The main parser function.
fn parser<'tokens, 'src: 'tokens, I, M>(
    make_input: M,
) -> impl Parser<'tokens, I, Package, extra::Err<Rich<'tokens, Token<'src>, SimpleSpan>>>
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

    let ty = recursive(|ty| {
        let type_list = ty.clone().separated_by(just(Token::Comma)).collect::<Vec<_>>();
        let path = 
            // Path Access
            // This is super hacky, but it does give us a nice infix operator
        ident.clone().pratt(vec![infix(left(10), just(Token::Dot), |x, _, y, e| {
                (Expr::FieldAccess(Box::new(x), Box::new(y)), e.span())
            }).boxed()]).or(ident.clone()).memoized();
        choice((
            // User Types
            path.clone().then(type_list.clone().delimited_by(just(Token::LBracket), just(Token::RBracket)).or_not()).clone().map_with(|(name, generics), e| (Ty::User(name, generics.unwrap_or_default()), e.span())),
            // Arrow Type
            ty.clone().pratt(vec![infix(right(9), just(Token::Arrow), |x, _, y, e| {
                (Ty::Arrow(Box::new(x), Box::new(y)), e.span())
            })]),
            // Generic Type
            just(Token::Question).ignore_then(ident.clone()).map_with(|name, e| (Ty::Generic(name), e.span())),
            // Tuple
            type_list.clone().delimited_by(just(Token::LBrace), just(Token::RBrace)).map_with(|types, e| (Ty::Tuple(types), e.span())),
            
        ))
    });

    // Pattern parser.
    let pattern = recursive(|pat| {
        let path = 
            // Path Access
            // This is super hacky, but it does give us a nice infix operator
        ident.clone().pratt(vec![infix(left(10), just(Token::Dot), |x, _, y, e| {
                (Expr::FieldAccess(Box::new(x), Box::new(y)), e.span())
            }).boxed()]).or(ident.clone()).memoized();
        choice((
            pat.clone().separated_by(just(Token::Comma)).collect::<Vec<Spanned<Pattern>>>().delimited_by(just(Token::LBrace), just(Token::RBrace))
                //.map_with(|p, e| (Pattern::Tuple(p), e.span())),
                .map_with(|p, e| (Pattern::Tuple(p), e.span())),

            path.then(pat.separated_by(just(Token::Comma)).collect::<Vec<_>>().delimited_by(just(Token::LBrace), just(Token::RBrace)).or_not())
                .map_with(|(name, args), e| {
                    if let Some(args) = args {
                        (Pattern::Variant(Box::new(name), args), e.span())
                    } else {
                        (Pattern::Atom(PatternAtom::Variable(name.0.get_ident().unwrap())), e.span())
                    }
                }),
            //select_ref! { Token::Ident(x) => *x }.map_with(|x, e| (Pattern::Atom(PatternAtom::Variable(x.to_string())), e.span())),
                        // Numbers
            select_ref! { Token::Num(x) => OrderedFloat(*x) }
.map_with(|x, e| (Pattern::Atom(PatternAtom::Num(x)), e.span())),            
            // Strings
            select_ref! { Token::Strlit(x) => x.to_string() }
.map_with(|x, e| (Pattern::Atom(PatternAtom::Strlit(x.to_string())), e.span()),            
        )))

        
    });


    // Expression parser
    let expression = recursive(|expr| {
        let rname = select_ref! { Token::Ident(x) => *x };
        let ident = rname.map_with(|x, e| (Expr::Ident(x.to_string()), e.span()));
        let path = 
            // Path Access
            // This is super hacky, but it does give us a nice infix operator
        ident.clone().pratt(vec![infix(right(9), just(Token::Dot), |x, _, y, e| {
                (Expr::FieldAccess(Box::new(x), Box::new(y)), e.span())
            }).boxed()]).memoized();
        
        let atom =         recursive(|atom| {
            
            choice((
            // Numbers
            select_ref! { Token::Num(x) => Expr::Number(OrderedFloat(*x)) }
                .map_with(|x, e| (x, e.span())),
            
            // Strings
            select_ref! { Token::Strlit(x) => Expr::String(x.to_string()) }
                .map_with(|x, e| (x, e.span())),
            
            // True
            just(Token::True).map_with(|_, e| (Expr::Bool(true), e.span())),
            
            // False
            just(Token::False).map_with(|_, e| (Expr::Bool(false), e.span())),

            //path,

        
            
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

            // Plain old idents
            //ident,

            // Constructors
            path.then(atom.clone().separated_by(just(Token::Comma)).collect::<Vec<_>>().delimited_by(just(Token::LBrace), just(Token::RBrace)).or_not())
                .map_with(|(name, args), e| {
                    if let Some(args) = args {
                        (Expr::Constructor(Box::new(name), args), e.span())
                    } else {
                        name
                    }
                }).labelled("item"),
            
            atom.clone().separated_by(just(Token::Comma)).collect::<Vec<_>>().delimited_by(just(Token::LBrace), just(Token::RBrace)).map_with(|items, e| (Expr::Tuple(items), e.span())).labelled("tuple").as_context(),

            // let x = y ; z
            just(Token::Let)
                //.ignore_then(ident)
                .ignore_then(pattern.clone())
                .then_ignore(just(Token::Eq))
                .then(expr.clone())
                .then_ignore(just(Token::In))
                .then(expr.clone())
                .map_with(|((lhs, rhs), then), e| {
                    (
                        Expr::Let(Box::new((Expr::Pat(lhs.0), lhs.1)), Box::new(rhs), Box::new(then)),
                        e.span(),
                    )
                }),

            // If expression
            just(Token::If)
                .ignore_then(expr.clone())
                .then_ignore(just(Token::Then))
                .then(expr.clone())
                .then_ignore(just(Token::Else))
                .then(expr.clone())
                .map_with(|((test, then), otherwise), e| {
                    (
                        Expr::If(Box::new(test), Box::new(then), Box::new(otherwise)),
                        e.span(),
                    )
                }),

            // Match Expression
            just(Token::Match)
                .ignore_then(expr.clone())
                .then(just(Token::Pipe).ignore_then(pattern).then_ignore(just(Token::Then)).then(expr.clone()).map(|(p,e)| (p, Box::new(e))).repeated().collect::<Vec<_>>()).map_with(|(matchee, arms), e| {(Expr::Match(Box::new(matchee), arms), e.span())})
        ))})  
        .boxed().memoized();

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
            infix(left(8), just(Token::Asterisk), |x, _, y, e| {
                (Expr::Mul(Box::new(x), Box::new(y)), e.span())
            })
            .boxed(),
            // Divide
            infix(left(8), just(Token::Slash), |x, _, y, e| {
                (Expr::Div(Box::new(x), Box::new(y)), e.span())
            })
            .boxed(),
            // Add
            infix(left(7), just(Token::Plus), |x, _, y, e| {
                (Expr::Add(Box::new(x), Box::new(y)), e.span())
            })
            .boxed(),
            // Subtract
            infix(left(7), just(Token::Minus), |x, _, y, e| {
                (Expr::Sub(Box::new(x), Box::new(y)), e.span())
            })
            .boxed(),
            // Calls
            infix(right(9), empty(), |func, _, arg, e| {
                (Expr::Call(Box::new(func), Box::new(arg)), e.span())
            })
            .boxed(),
            // Field Access
            infix(left(10), just(Token::Dot), |x, _, y, e| {
                (Expr::FieldAccess(Box::new(x), Box::new(y)), e.span())
            }).boxed(),
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
            .then(ty.clone());

        let struct_def = just(Token::Struct)
            .ignore_then(ident)
            .then_ignore(just(Token::Eq))
            .then(
                struct_field
                    .separated_by(just(Token::Comma))
                    .allow_trailing()
                    .collect::<Vec<_>>(),
            )
            .map_with(|(name, fields), _| Definition::Struct(StructDef { name, fields }));

        // Import
        let import_path = expression; //ident
                                      // .clone()
                                      // .separated_by(just(Token::Dot))
                                      // .at_least(1)
                                      // .collect::<Vec<_>>();

        let import_items = import_path
            .clone()
            .separated_by(just(Token::Comma))
            .collect::<Vec<_>>()
            .delimited_by(just(Token::LBrace), just(Token::RBrace));

        let import = just(Token::Use)
            .ignore_then(import_items)
            .map(|items| Definition::Import(ImportItem { items }));

        choice((import, let_binding, struct_def))
    });

    let package = just(Token::Package)
        .ignore_then(ident)
        .then_ignore(just(Token::Eq))
        .then(just(Token::Pub).ignore_then(definition.clone().repeated().collect::<Vec<_>>()).or(definition.clone().repeated().collect::<Vec<_>>())
        )
        .map(|(name, items)| Package {
            name: name.to_owned(),
            items,
        });
    //
    // Program
    package
        // .repeated()
        // .at_least(1)
        // .collect::<Vec<_>>()
        // .map(|packages| Program { packages })
        .then_ignore(end())
}

/// Legacy helper function for the parser's error handlings
fn parse_failure(err: &Rich<impl std::fmt::Display>, src: &str) -> DynamicErr {
    DynamicErr::new(err.reason().to_string())
        .label((
            err.found()
                .map(|c| c.to_string())
                .unwrap_or_else(|| "end of input".to_string()),
            *err.span(),
        ))
        .extra_labels(
            err.contexts()
                .map(|(l, s)| (format!("while parsing this {l}"), *s))
                .collect(),
        )
        .src(src.to_string())
    // GeneralErr::builder()
    //     .msg(err.reason().to_string())
    //     .label((
    //         err.found()
    //             .map(|c| c.to_string())
    //             .unwrap_or_else(|| "end of input".to_string()),
    //         *err.span(),
    //     ))
    //     .extra_labels(
    //         err.contexts()
    //             .map(|(l, s)| (format!("while parsing this {l}"), *s))
    //             .collect(),
    //     )
    //     .src(src.to_string())
    //     .build(),
}

/// Compiler black magic function that transforms the token vector into a parsable item.
fn make_input<'src>(
    eoi: SimpleSpan,
    toks: &'src [Spanned<Token<'src>>],
) -> impl BorrowInput<'src, Token = Token<'src>, Span = SimpleSpan> {
    toks.map(eoi, |(t, s)| (t, s))
}

/// Public parsing function. Produces a parse tree from a source string.
pub fn parse(input: &str) -> CompResult<Package> {
    let tokens = match lexer().parse(input).into_result() {
        Ok(tokens) => tokens,
        Err(errs) => return Err(CompilerErr::Dynamic(parse_failure(&errs[0], input))),
    };

    //dbg!(&tokens);

    let packg = match parser(make_input)
        .parse(make_input((0..input.len()).into(), &tokens))
        .into_result()
    {
        Ok(p) => Ok(p),
        Err(e) => {
            Err(CompilerErr::Dynamic(parse_failure(
                &e.first().unwrap(),
                input,
            )))
            // let errors = e
            //     .iter()
            //     .map(|e| CompilerErr::Dynamic(parse_failure(&e, input)))
            //     .collect::<Vec<_>>();
            // Err(errors[0])
        }
    };

    Ok(packg?)
}
