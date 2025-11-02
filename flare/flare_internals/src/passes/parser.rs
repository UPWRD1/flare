use chumsky::extra::SimpleState;
use chumsky::input::{BorrowInput, SliceInput, StrInput, ValueInput};
use chumsky::pratt::{infix, left, right, Operator};
use chumsky::prelude::*;
use ordered_float::OrderedFloat;

use crate::resource::{
    errors::{CompResult, CompilerErr, DynamicErr, ErrorCollection},
    rep::{
        ast::{ComparisonOp, Definition, EnumDef, Expr, Package, Pattern, PatternAtom, StructDef},
        files::FileID,
        types::{EnumVariant, PrimitiveType, Ty},
        Spanned,
    },
};

/// Type representing the tokens produced by the lexer. Is private, since tokens are only used in the first stage of parsing.
#[derive(Debug, Clone, PartialEq)]
#[allow(dead_code)]
enum Token {
    Ident(&'static str),
    Num(f64),

    Strlit(&'static str),
    Comment(&'static str),
    Parens(Vec<Spanned<Self>>),

    ///Contains the erroneous token's src
    Error(char),

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

    ComparisonOp(ComparisonOp),

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

    TyNum,
    TyStr,
    TyBool,
    TyUnit,
}

impl std::fmt::Display for Token {
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
            Token::ComparisonOp(c) => match c {
                ComparisonOp::Eq => write!(f, "=="),
                ComparisonOp::Neq => write!(f, "!="),
                ComparisonOp::Gt => write!(f, ">"),
                ComparisonOp::Lt => write!(f, "<"),
                ComparisonOp::Gte => write!(f, ">="),
                ComparisonOp::Lte => write!(f, "<="),
            },
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

            Token::TyNum => write!(f, "num"),
            Token::TyStr => write!(f, "str"),
            Token::TyBool => write!(f, "bool"),
            Token::TyUnit => write!(f, "unit"),

            Token::Error(e) => write!(f, "Error {e}"),
        }
    }
}

/// The primary lexer function.
fn lexer<I>() -> impl Parser<
    'static,
    I,
    Vec<(Token, SimpleSpan<usize, u64>)>,
    extra::Full<Rich<'static, char, SimpleSpan<usize, ()>>, SimpleState<u64>, ()>, /*extra::Err<Rich<'src, char>>*/
>
//where I:  BorrowInput<'src, Token = char, Span = SimpleSpan<usize, ()>> + ValueInput<'src, Token = char, Span = SimpleSpan<usize, ()>> + StrInput<'src> + SliceInput<'src>,
where
    I: ValueInput<'static, Token = char, Span = SimpleSpan<usize, ()>>
        + SliceInput<'static, Slice = &'static str, Span = SimpleSpan<usize, ()>>
        + StrInput<'static, Span = SimpleSpan<usize, ()>>,
    //where I: str
{
    let comment = just("#")
        .then_ignore(any().and_is(just('\n').not()).repeated())
        .labelled("comment")
        .padded();
    let recover_strat1 = any().padded_by(comment.repeated()).padded().map_with(
        |x, e: &mut chumsky::input::MapExtra<'_, '_, I, extra::Full<_, SimpleState<u64>, ()>>| {
            (
                Token::Error(x),
                SimpleSpan::new(**e.state(), e.span().into_range()),
            )
        },
    );

    let comparison_op = choice((
        just("<").to(Token::ComparisonOp(ComparisonOp::Lt)),
        just("<=").to(Token::ComparisonOp(ComparisonOp::Lte)),
        just(">").to(Token::ComparisonOp(ComparisonOp::Gt)),
        just(">=").to(Token::ComparisonOp(ComparisonOp::Gte)),
        just("==").to(Token::ComparisonOp(ComparisonOp::Eq)),
        just("!=").to(Token::ComparisonOp(ComparisonOp::Neq)),
    ))
    .labelled("comparison operator");

    let arith_op = choice((
        just("*").to(Token::Asterisk),
        just("/").to(Token::Slash),
        just("+").to(Token::Plus),
        just("-").to(Token::Minus),
    ))
    .labelled("arithmetic operator");

    recursive( |token| {
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
                "num" => Token::TyNum,
                "str" => Token::TyStr,
                "bool" => Token::TyBool,
                "unit" => Token::TyUnit,

                s => Token::Ident(s),
            }),
            // Operators
            comparison_op,

            just("=>").to(Token::FatArrow),
            just("->").to(Token::Arrow),
            just("=").to(Token::Eq),
            just("{").to(Token::LBrace),
            just("}").to(Token::RBrace),
            just("[").to(Token::LBracket),
            just("]").to(Token::RBracket),


                        arith_op,

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
                .map(|s: &str| Token::Num(s.parse().unwrap())).labelled("number"),
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
                .labelled("nested expression")
                .as_context()
                .map(Token::Parens),
        ))
        .map_with( |t, e: &mut chumsky::input::MapExtra<'_, '_, I, extra::Full<_, SimpleState<u64>, () >>|{  (t, SimpleSpan::new(**e.state(), e.span().into_range()))})
    })
    .padded_by(comment.repeated())
    .padded()
    .recover_with(via_parser(recover_strat1))

    .repeated()
    .collect().boxed()
}

/// The main parser function.
fn parser<I, M>(
    make_input: M,
) -> impl Parser<
    'static,
    I,
    Package,
    extra::Full<Rich<'static, Token, SimpleSpan<usize, FileID>>, SimpleState<u64>, ()>,
>
//) -> impl Parser<'tokens, I, Package, extra::Err<Rich<'tokens, Token<'src>, SimpleSpan<usize, FileID>>>>
where
    I: BorrowInput<'static, Token = Token, Span = SimpleSpan<usize, FileID>>,
    // Because this function is generic over the input type, we need the caller to tell us how to create a new input,
    // `I`, from a nested token tree. This function serves that purpose.
    M: Fn(SimpleSpan<usize, FileID>, &'static [Spanned<Token>]) -> I + Clone + 'static,
{
    // Basic tokens
    let ident = select_ref! { Token::Ident(x) => *x }.map_with(|x, e| (Expr::Ident(x), e.span()));

    let ty = recursive(|ty| {
        let type_list = ty
            .clone()
            .separated_by(just(Token::Comma))
            .allow_trailing()
            .collect::<Vec<_>>();
        // Path Access
        // This is super hacky, but it does give us a nice infix operator
        let path = ident
            .pratt(vec![infix(left(10), just(Token::Dot), |x, _, y, e| {
                (
                    Expr::FieldAccess(Box::leak(Box::new(x)), Box::leak(Box::new(y))),
                    e.span(),
                )
            })
            .boxed()])
            .or(ident)
            .memoized();

        choice((
            // Primitive Types
            just(Token::TyNum).map_with(|_, e| (Ty::Primitive(PrimitiveType::Num), e.span())),
            just(Token::TyStr).map_with(|_, e| (Ty::Primitive(PrimitiveType::Str), e.span())),
            just(Token::TyBool).map_with(|_, e| (Ty::Primitive(PrimitiveType::Bool), e.span())),
            just(Token::TyUnit).map_with(|_, e| (Ty::Primitive(PrimitiveType::Unit), e.span())),
            // User Types
            path.clone()
                .then(
                    type_list
                        .clone()
                        .delimited_by(just(Token::LBracket), just(Token::RBracket))
                        .or_not(),
                )
                .clone()
                .map_with(|(name, generics), e| {
                    (
                        Ty::User(name, generics.unwrap_or_default().leak()),
                        e.span(),
                    )
                }),
            // Generic Type
            just(Token::Question)
                .ignore_then(ident)
                .map_with(|name, e| (Ty::Generic(name), e.span())),
            // Tuple
            type_list
                .clone()
                .delimited_by(just(Token::LBrace), just(Token::RBrace))
                .map_with(|types, e| (Ty::Tuple(types.leak()), e.span())),
        ))
        .pratt(vec![infix(right(9), just(Token::Arrow), |x, _, y, e| {
            (
                Ty::Arrow(Box::leak(Box::new(x)), Box::leak(Box::new(y))),
                e.span(),
            )
        })])
    });

    // Pattern parser.
    let pattern = recursive(|pat| {
        // Path Access
        // This is super hacky, but it does give us a nice infix operator

        let path = ident
            .pratt(vec![infix(left(10), just(Token::Dot), |x, _, y, e| {
                (
                    Expr::FieldAccess(Box::leak(Box::new(x)), Box::leak(Box::new(y))),
                    e.span(),
                )
            })
            .boxed()])
            .or(ident)
            .memoized();
        choice((
            pat.clone()
                .separated_by(just(Token::Comma))
                .collect::<Vec<Spanned<Pattern>>>()
                .delimited_by(just(Token::LBrace), just(Token::RBrace))
                //.map_with(|p, e| (Pattern::Tuple(p), e.span())),
                .map_with(|p, e| (Pattern::Tuple(p.leak()), e.span())),
            path.then(
                pat.separated_by(just(Token::Comma))
                    .collect::<Vec<_>>()
                    .delimited_by(just(Token::LBrace), just(Token::RBrace))
                    .or_not(),
            )
            .map_with(|(name, args), e| {
                if let Some(args) = args {
                    (
                        Pattern::Variant(Box::leak(Box::new(name)), args.leak()),
                        e.span(),
                    )
                } else {
                    (
                        Pattern::Atom(PatternAtom::Variable(name.0.get_ident().unwrap())),
                        e.span(),
                    )
                }
            }),
            //select_ref! { Token::Ident(x) => *x }.map_with(|x, e| (Pattern::Atom(PatternAtom::Variable(x.to_string())), e.span())),
            // Numbers
            select_ref! { Token::Num(x) => OrderedFloat(*x) }
                .map_with(|x, e| (Pattern::Atom(PatternAtom::Num(x)), e.span())),
            // Strings
            select_ref! { Token::Strlit(x) => (*x).to_string() }
                .map_with(|x, e| (Pattern::Atom(PatternAtom::Strlit(x.leak())), e.span())),
            ty.clone().map_with(|x, e| {
                (
                    Pattern::Atom(PatternAtom::Type(Box::leak(Box::new(x)))),
                    e.span(),
                )
            }),
        ))
    });

    // Expression parser
    let expression = recursive(|expr| {
        let rname = select_ref! { Token::Ident(x) => *x };
        let ident = rname.map_with(|x, e| (Expr::Ident(x), e.span()));
        // Path Access
        // This is super hacky, but it does give us a nice infix operator
        let path = ident
            .pratt(vec![infix(right(9), just(Token::Dot), |x, _, y, e| {
                (
                    Expr::FieldAccess(Box::leak(Box::new(x)), Box::leak(Box::new(y))),
                    e.span(),
                )
            })
            .boxed()])
            .memoized();

        let atom = recursive(|atom| {
            choice((
                // Numbers
                select_ref! { Token::Num(x) => Expr::Number(OrderedFloat(*x)) }
                    .map_with(|x, e| (x, e.span())),
                // Strings
                select_ref! { Token::Strlit(x) => Expr::String(x) }.map_with(|x, e| (x, e.span())),
                // True
                just(Token::True).map_with(|_, e| (Expr::Bool(true), e.span())),
                // False
                just(Token::False).map_with(|_, e| (Expr::Bool(false), e.span())),
                // Plain old idents
                path.clone()
                    .then(
                        atom.clone()
                            .separated_by(just(Token::Comma))
                            .collect::<Vec<_>>()
                            .delimited_by(just(Token::LBrace), just(Token::RBrace)), //.or_not(),
                    )
                    .map_with(|(name, args), e| {
                        if !args.is_empty() {
                            (
                                Expr::Constructor(Box::leak(Box::new(name)), args.leak()),
                                e.span(),
                            )
                        } else {
                            name
                        }
                    })
                    .labelled("enum constructor")
                    .as_context(),
                path.clone()
                    .then(
                        ident
                            .then_ignore(just(Token::Eq))
                            .then(expr.clone())
                            .separated_by(just(Token::Comma))
                            .collect::<Vec<_>>()
                            .delimited_by(just(Token::LBrace), just(Token::RBrace))
                            .or_not(),
                    )
                    .map_with(|(name, args), e| {
                        if let Some(args) = args {
                            (
                                Expr::FieldedConstructor(Box::leak(Box::new(name)), args.leak()),
                                e.span(),
                            )
                        } else {
                            name
                        }
                    })
                    .labelled("struct constructor")
                    .as_context(),
                // Enum Constructors
                expr.clone()
                    .separated_by(just(Token::Comma))
                    .collect::<Vec<_>>()
                    .delimited_by(just(Token::LBrace), just(Token::RBrace))
                    .map_with(|items, e| (Expr::Tuple(items.leak()), e.span()))
                    .labelled("tuple")
                    .as_context(),
                // let x = y in z
                just(Token::Let)
                    //.ignore_then(ident)
                    .ignore_then(pattern.clone())
                    .then_ignore(just(Token::Eq))
                    .then(expr.clone())
                    .then_ignore(just(Token::In))
                    .then(expr.clone())
                    .map_with(|((lhs, rhs), then), e| {
                        (
                            Expr::Let(
                                Box::leak(Box::new((Expr::Pat(lhs), lhs.1))),
                                Box::leak(Box::new(rhs)),
                                Box::leak(Box::new(then)),
                            ),
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
                            Expr::If(
                                Box::leak(Box::new(test)),
                                Box::leak(Box::new(then)),
                                Box::leak(Box::new(otherwise)),
                            ),
                            e.span(),
                        )
                    })
                    .labelled("if expression")
                    .as_context(),
                // Match Expression
                just(Token::Match)
                    .ignore_then(expr.clone())
                    .then(
                        just(Token::Pipe)
                            .ignore_then(pattern.clone())
                            .then_ignore(just(Token::Then))
                            .then(expr.clone())
                            .map(|(p, e)| (p, e))
                            .separated_by(just(Token::Comma))
                            .allow_trailing()
                            //                            .allow_trailing()
                            //.repeated()
                            .collect::<Vec<_>>(),
                    )
                    .map_with(|(matchee, arms), e| {
                        (
                            Expr::Match(Box::leak(Box::new(matchee)), arms.leak()),
                            e.span(),
                        )
                    }),
            ))
        })
        .boxed();
        //.memoized();

        choice((
            atom,
            // fn x y = z
            just(Token::Fn).ignore_then(ident.repeated().foldr_with(
                just(Token::FatArrow).ignore_then(expr.clone()),
                |arg, body, e| {
                    (
                        Expr::Lambda(Box::leak(Box::new(arg)), Box::leak(Box::new(body))),
                        e.span(),
                    )
                },
            )),
            // ( x )
            expr.nested_in(select_ref! { Token::Parens(ts) = e => make_input(e.span(), ts) }),
        ))
        .pratt(vec![
            // Multiply
            infix(left(8), just(Token::Asterisk), |x, _, y, e| {
                (
                    Expr::Mul(Box::leak(Box::new(x)), Box::leak(Box::new(y))),
                    e.span(),
                )
            })
            .boxed(),
            // Divide
            infix(left(8), just(Token::Slash), |x, _, y, e| {
                (
                    Expr::Div(Box::leak(Box::new(x)), Box::leak(Box::new(y))),
                    e.span(),
                )
            })
            .boxed(),
            // Add
            infix(left(7), just(Token::Plus), |x, _, y, e| {
                (
                    Expr::Add(Box::leak(Box::new(x)), Box::leak(Box::new(y))),
                    e.span(),
                )
            })
            .boxed(),
            // Subtract
            infix(left(7), just(Token::Minus), |x, _, y, e| {
                (
                    Expr::Sub(Box::leak(Box::new(x)), Box::leak(Box::new(y))),
                    e.span(),
                )
            })
            .boxed(),
            infix(
                left(5),
                select_ref! { Token::ComparisonOp(c) => *c },
                |left, op, right, e| {
                    (
                        Expr::Comparison(Box::leak(Box::new(left)), op, Box::leak(Box::new(right))),
                        e.span(),
                    )
                },
            )
            .boxed(),
            // Calls
            infix(left(9), empty(), |func, (), arg, e| {
                (
                    Expr::Call(Box::leak(Box::new(func)), Box::leak(Box::new(arg))),
                    e.span(),
                )
            })
            .boxed(),
            // Field Access
            infix(left(10), just(Token::Dot), |x, _, y, e| {
                (
                    Expr::FieldAccess(Box::leak(Box::new(x)), Box::leak(Box::new(y))),
                    e.span(),
                )
            })
            .boxed(),
        ])
        // .map(|x| Box::leak(Box::new(x)))
        .boxed()
        .memoized()
        .labelled("expression")
        .as_context()
    });

    let definition = recursive(move |_| {
        // Toplevel Let binding
        // let let_binding = just(Token::Let)
        //     .ignore_then(ident)
        //     .then(just(Token::Colon).ignore_then(ty.clone()).or_not())
        //     .then(ident.repeated().foldr_with(
        //         just(Token::Eq).ignore_then(expression.clone()),
        //         |arg, body, e| (Expr::Lambda(Box::leak(arg), Box::leak(body)), e.span()),
        //     ))
        //     .map(|((name, ty), value)| Definition::Let(name, value, ty))
        //     .labelled("let-declaration")
        //     .as_context();
        let let_binding = just(Token::Let)
            .ignore_then(ident)
            .then(ident.repeated().collect::<Vec<_>>())
            .then(just(Token::Colon).ignore_then(ty.clone()).or_not())
            .then(just(Token::Eq).ignore_then(expression.clone()))
            .map_with(|(((name, args), ty), body), e| {
                let value = args.into_iter().rev().fold(body, |acc, arg| {
                    (
                        Expr::Lambda(Box::leak(Box::new(arg)), Box::leak(Box::new(acc))),
                        e.span(),
                    )
                });
                Definition::Let(name, value, ty)
            })
            .labelled("let-declaration")
            .as_context();

        // Struct definition
        let struct_field = ident.then_ignore(just(Token::Colon)).then(ty.clone());

        let struct_def = just(Token::Struct)
            .ignore_then(ty.clone())
            //.then(ident.separated_by(just(Token::Comma)).delimited_by(just(Token::LBracket), just(Token::RBracket)).collect::<Vec<_>>())
            .then_ignore(just(Token::Eq))
            .then(
                struct_field
                    .separated_by(just(Token::Comma))
                    .allow_trailing()
                    .collect::<Vec<_>>(),
            )
            .map_with(|(the_ty, fields), _| Definition::Struct(StructDef { the_ty, fields }));
        let enum_variant = choice((
            ident
                .then(
                    ty.clone()
                        .separated_by(just(Token::Comma))
                        .allow_trailing()
                        .collect::<Vec<_>>()
                        .delimited_by(just(Token::LBrace), just(Token::RBrace)),
                )
                .map_with(|(name, types), e| {
                    (
                        EnumVariant {
                            name,
                            types: types.leak(),
                        },
                        e.span(),
                    )
                }),
            ident.map_with(|x, e| {
                (
                    EnumVariant {
                        name: x,
                        types: vec![].leak(),
                    },
                    e.span(),
                )
            }),
        ));

        // Enum defintions
        let enum_def = just(Token::Enum)
            .ignore_then(ty.clone())
            .then_ignore(just(Token::Eq))
            .then(
                enum_variant
                    .clone()
                    .separated_by(just(Token::Comma))
                    .allow_trailing()
                    .collect::<Vec<Spanned<EnumVariant>>>(),
            )
            .map_with(|(the_ty, variants), _| Definition::Enum(EnumDef { the_ty, variants }));

        // Import
        let import_path = expression; //ident
                                      // .clone()
                                      // .separated_by(just(Token::Dot))
                                      // .at_least(1)
                                      // .collect::<Vec<_>>();

        let import = just(Token::Use)
            .ignore_then(import_path)
            .map(Definition::Import);

        let extern_def = just(Token::Extern)
            .ignore_then(ident)
            .then_ignore(just(Token::Colon))
            .then(ty.clone())
            .map(|(name, ty)| Definition::Extern(name, ty));

        choice((import, let_binding, struct_def, extern_def, enum_def))
    });

    let package = just(Token::Package)
        .ignore_then(ident)
        .then_ignore(just(Token::Eq))
        .then(
            just(Token::Pub)
                .ignore_then(definition.clone().repeated().collect::<Vec<_>>())
                .or(definition.clone().repeated().collect::<Vec<_>>()),
        )
        .map(|(name, items)| Package {
            name: name.to_owned(),
            items,
        })
        .labelled("package")
        .as_context();
    //
    // Program
    package
        // .repeated()
        // .at_least(1)
        // .collect::<Vec<_>>()
        // .map(|packages| Program { packages })
        .then_ignore(end())
}

/// Trait that extends `SimpleSpan` to permit adding `FileID` information
pub trait AnnotateRange {
    fn annotate(&self, id: FileID) -> SimpleSpan<usize, u64>;
}

impl AnnotateRange for SimpleSpan<usize, ()> {
    fn annotate(&self, id: FileID) -> SimpleSpan<usize, u64> {
        SimpleSpan::new(id, self.into_range())
    }
}

impl AnnotateRange for SimpleSpan<usize, u64> {
    fn annotate(&self, id: FileID) -> SimpleSpan<usize, u64> {
        SimpleSpan::new(id, self.into_range())
    }
}

/// Legacy helper function for the parser's error handlings
fn parse_failure(
    err: &Rich<'_, impl std::fmt::Display, impl AnnotateRange>,
    fid: FileID,
) -> DynamicErr {
    DynamicErr::new(err.reason().to_string())
        .label((
            err.found().map_or_else(
                || "end of input".to_string(),
                |c| format!("Unexpected '{c}'"),
            ),
            err.span().annotate(fid),
            //err.span()
        ))
        .extra_labels(
            err.contexts()
                .map(|(l, s)| (format!("while parsing this {l}"), s.annotate(fid)))
                .collect(),
        )
}

/// Compiler black magic function that transforms the token vector into a parsable item.
fn make_input(
    eoi: SimpleSpan<usize, FileID>,
    toks: &'static [(Token, SimpleSpan<usize, FileID>)],
) -> impl BorrowInput<'static, Token = Token, Span = SimpleSpan<usize, FileID>> {
    //toks.map(eoi, |(t, s)| (t, s))
    toks.map(eoi, |(t, s)| (t, s))
}

/// Public parsing function. Produces a parse tree from a source string.
pub fn parse(input: &'static str, fid: FileID) -> CompResult<Package> {
    let tokens: Vec<(Token, SimpleSpan<usize, u64>)> = match lexer()
        .parse_with_state(input, &mut SimpleState::from(0))
        .into_result()
    {
        Ok(tokens) => tokens,
        Err(errs) => {
            return Err(ErrorCollection::new(
                errs.into_iter()
                    .map(|x| parse_failure(&x, fid).into())
                    .collect(),
            )
            .into());
            // return Err(CompilerErrKind::Dynamic(parse_failure(&errs[0], fid)).into())
        }
    };

    //dbg!(&tokens);

    let packg: Result<Package, CompilerErr> = match parser(make_input)
        .parse_with_state(
            make_input(SimpleSpan::new(fid, 0..input.len()), tokens.leak()),
            &mut SimpleState::from(0),
        )
        .into_result()
    {
        Ok(p) => Ok(p),
        Err(e) => {
            let errs = ErrorCollection::new(
                e.iter()
                    .map(|e| {
                        parse_failure(
                            e, //SimpleSpan::new(fid, e.first().unwrap().span()),
                            fid,
                        )
                        .into()
                    })
                    .collect::<Vec<CompilerErr>>(),
            );
            Err(errs.into())
            // let errors = e
            //     .iter()
            //     .map(|e| CompilerErr::Dynamic(parse_failure(&e, input)))
            //     .collect::<Vec<_>>();
            // Err(errors[0])
        }
    };

    packg
}
