use crate::{
    resource::{
        errors::{CompResult, CompilerErr, DynamicErr, ErrorCollection},
        rep::{
            ast::{
                ComparisonOp, Definition, EnumDef, Expr, Package, Pattern, PatternAtom, StructDef,
            },
            files::FileID,
            types::{EnumVariant, PrimitiveType, Ty},
            Spanned,
        },
    },
    Context,
};
use chumsky::input::{BorrowInput, MapExtra, SliceInput, StrInput, ValueInput};
use chumsky::pratt::{infix, left, right, Operator};
use chumsky::prelude::*;
use internment::Intern;
// use lasso::{Interner, Rodeo};
use ordered_float::OrderedFloat;

/// Type representing the tokens produced by the lexer. Is private, since tokens are only used in the first stage of parsing.
#[derive(Debug, Clone, PartialEq, Copy)]
#[allow(dead_code)]
enum Token {
    Ident(&'static str),
    // Ident(),
    // Ident(Spur),
    Num(f64),

    Strlit(&'static str),
    // Strlit(Intern<String>),
    Comment(&'static str),
    // Comment(Intern<String>),
    Parens(&'static [Spanned<Self>]),

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

    Def,
    Else,
    Enum,
    Extern,
    False,
    Fn,
    If,
    Impl,
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
    Myself,
    Metatype,
}

impl std::fmt::Display for Token {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            Self::Ident(x) => write!(f, "{x:?}"),
            Self::Num(x) => write!(f, "{x}"),
            Self::Eq => write!(f, "="),
            Self::Let => write!(f, "let"),
            Self::In => write!(f, "in"),
            Self::Parens(_) => write!(f, "(...)"),
            Self::Asterisk => write!(f, "*"),
            Self::Slash => write!(f, "/"),
            Self::Plus => write!(f, "+"),
            Self::Minus => write!(f, "-"),
            Self::ComparisonOp(c) => match c {
                ComparisonOp::Eq => write!(f, "=="),
                ComparisonOp::Neq => write!(f, "!="),
                ComparisonOp::Gt => write!(f, ">"),
                ComparisonOp::Lt => write!(f, "<"),
                ComparisonOp::Gte => write!(f, ">="),
                ComparisonOp::Lte => write!(f, "<="),
            },
            Self::Fn => write!(f, "fn"),
            Self::True => write!(f, "true"),
            Self::False => write!(f, "false"),
            Self::Strlit(s) => write!(f, "\"{s}\""),
            Self::Comment(c) => write!(f, "{c}"),
            Self::Colon => write!(f, ":"),
            Self::Separator => write!(f, "newline"),
            Self::Whitespace => write!(f, "whitespace"),
            Self::Dot => write!(f, "."),
            Self::FatArrow => write!(f, "=>"),
            Self::Arrow => write!(f, "->"),
            Self::Question => write!(f, "?"),

            Self::Comma => write!(f, ","),
            Self::Pipe => write!(f, "|"),
            Self::LBrace => write!(f, "{{"),
            Self::RBrace => write!(f, "}}"),
            Self::LBracket => write!(f, "["),
            Self::RBracket => write!(f, "]"),
            Self::LParen => write!(f, "("),
            Self::RParen => write!(f, ")"),
            Self::Package => write!(f, "package"),
            Self::Use => write!(f, "use"),
            Self::Struct => write!(f, "struct"),
            Self::Else => write!(f, "else"),
            Self::Enum => write!(f, "enum"),
            Self::Extern => write!(f, "extern"),
            Self::Impl => write!(f, "impl"),
            Self::If => write!(f, "if"),
            Self::Match => write!(f, "match"),
            Self::Pub => write!(f, "pub"),
            Self::Then => {
                write!(f, "then")
            }
            Self::Def => write!(f, "def"),

            Self::TyNum => write!(f, "num"),
            Self::TyStr => write!(f, "str"),
            Self::TyBool => write!(f, "bool"),
            Self::TyUnit => write!(f, "unit"),
            Self::Myself => write!(f, "self"),
            Self::Metatype => write!(f, "Self"),

            Self::Error(e) => write!(f, "Error {e}"),
        }
    }
}

/// The primary lexer function.
fn lexer<I>(
    id: FileID,
    // rodeo: &mut Rodeo,
) -> impl Parser<
    'static,
    I,
    Vec<(Token, SimpleSpan<usize, u64>)>,
    extra::Err<Rich<'static, char, SimpleSpan<usize, ()>>>, /*extra::Err<Rich<'src, char>>*/
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
        move |x, e: &mut chumsky::input::MapExtra<'_, '_, I, extra::Err<_>>| {
            (
                Token::Error(x),
                // SimpleSpan::new(**e.state(), e.span().into_range()),
                SimpleSpan::new(id, e.span().into_range()),
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

    recursive(move |token| {
        choice((
            text::ident().map(|s| match s {
                "def" => Token::Def,
                "else" => Token::Else,
                "enum" => Token::Enum,
                "extern" => Token::Extern,
                "false" => Token::False,
                "fn" => Token::Fn,
                "impl" => Token::Impl,
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
                "self" => Token::Myself,
                "Self" => Token::Metatype,
                "str" => Token::TyStr,
                "bool" => Token::TyBool,
                "unit" => Token::TyUnit,

                // s => Token::Ident(rodeo.get_or_intern_static(s)),
                // s => Token::Ident(e.state().get_or_intern(s)),
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
                .map(|s: &str| Token::Num(s.parse().unwrap()))
                .labelled("number"),
            just('"')
                .ignore_then(none_of('"').repeated().to_slice())
                .then_ignore(just('"'))
                .labelled("string literal")
                .map(Token::Strlit),
            token
                .padded()
                .repeated()
                .collect::<Vec<_>>()
                .delimited_by(just('('), just(')'))
                .labelled("nested expression")
                .as_context()
                .map_with(|x, _| Token::Parens(x.leak())),
        ))
        .map_with(move |t, e: &mut MapExtra<'_, '_, I, extra::Err<_>>| {
            (t, SimpleSpan::new(id, e.span().into_range()))
        })
    })
    .padded_by(comment.repeated())
    .padded()
    .recover_with(via_parser(recover_strat1))
    .repeated()
    .collect::<Vec<_>>()
    .boxed()
}

/// The main parser function.
fn parser<I, M>(
    make_input: &'static M,
    // id: FileID,
    // rodeo: &mut Rodeo,
) -> impl Parser<
    'static,
    I,
    Package,
    extra::Full<Rich<'static, Token, SimpleSpan<usize, FileID>>, (), ()>,
    // extra::Full<Rich<'src, Token<'src>, SimpleSpan<usize, FileID>>, RodeoState, ()>,
>
//) -> impl Parser<'tokens, I, Package, extra::Err<Rich<'tokens, Token<'src>, SimpleSpan<usize, FileID>>>>
where
    I: BorrowInput<'static, Token = Token, Span = SimpleSpan<usize, FileID>> + ValueInput<'static>,
    // Because this function is generic over the input type, we need the caller to tell us how to create a new input,
    // `I`, from a nested token tree. This function serves that purpose.
    M: Fn(SimpleSpan<usize, FileID>, &'static [Spanned<Token>]) -> I + 'static,
{
    // Basic tokens
    let ident =
        select_ref! { Token::Ident(x) => *x }.map_with(|x, e: &mut MapExtra<'_, '_, _, _>| {
            // |x, e| {
            (Expr::Ident(Intern::from_ref(x)), e.span())
            // (Expr::Ident(rodeo.get_or_intern_static(x)), e.span())
        });

    // let ty = ty_parser( ident.boxed()).boxed();

    let ty = recursive(|ty| {
        let type_list = ty
            .clone()
            .separated_by(just(Token::Comma))
            .allow_trailing()
            .collect::<Vec<_>>();

        let grouping = Parser::nested_in::<
            _,
            I,
            extra::Full<Rich<'static, Token, SimpleSpan<usize, FileID>>, (), ()>,
        >(
            ty.clone(),
            select_ref! { Token::Parens(ts) = e => make_input(e.span(), ts) },
        );
        // Path Access
        // This is super hacky, but it does give us a nice infix operator
        let path = ident
            .pratt(vec![infix(left(10), just(Token::Dot), |x, _, y, e| {
                (
                    Expr::FieldAccess(Intern::from(x), Intern::from(y)),
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
            just(Token::Metatype).map_with(|_, e| (Ty::Myself, e.span())),
            just(Token::Myself).map_with(|_, e| (Ty::Myself, e.span())),
            // User Types
            path.then(
                type_list
                    .clone()
                    .delimited_by(just(Token::LBracket), just(Token::RBracket))
                    .or_not(),
            )
            // .clone()
            .map_with(|(name, generics), e| {
                (
                    Ty::User(
                        (*name.0.get_ident(name.1).unwrap(), name.1),
                        Intern::from(generics.unwrap_or_default()),
                    ),
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
                .map_with(|types, e| (Ty::Tuple(Intern::from(types)), e.span())),
            grouping, // ty.nested_in(select_ref! { Token::Parens(ts) = e => make_input(e.span(), ts) }),
        ))
        .pratt(vec![infix(right(9), just(Token::Arrow), |x, _, y, e| {
            (Ty::Arrow(Intern::from(x), Intern::from(y)), e.span())
        })])
        .map_with(|x, e| x)
    });

    // Pattern parser.
    // let pattern  =         pattern_parser(Box::leak(Box::new(ident)), Box::leak(Box::new(ty.clone()))).boxed()
    // ;

    let pattern = recursive(|pat| {
        // Path Access
        // This is super hacky, but it does give us a nice infix operator
        // let ty = ty_parser(ident).lazy().boxed();
        let path = ident
            .pratt(vec![infix(left(10), just(Token::Dot), |x, _, y, e| {
                (
                    Expr::FieldAccess(Intern::from(x), Intern::from(y)),
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
                .map_with(|p, e| (Pattern::Tuple(Intern::from(p)), e.span())),
            path.then(
                pat.separated_by(just(Token::Comma))
                    .collect::<Vec<_>>()
                    .delimited_by(just(Token::LBrace), just(Token::RBrace))
                    .or_not(),
            )
            .map_with(|(name, args), e| {
                if let Some(args) = args {
                    (
                        Pattern::Variant(Intern::from(name), Intern::from(args)),
                        e.span(),
                    )
                } else {
                    (
                        Pattern::Atom(PatternAtom::Variable(*name.0.get_ident(name.1).unwrap())),
                        e.span(),
                    )
                }
            }),
            //select_ref! { Token::Ident(x) => *x }.map_with(|x, e| (Pattern::Atom(PatternAtom::Variable(x.to_string())), e.span())),
            // Numbers
            select_ref! { Token::Num(x) => OrderedFloat(*x) }
                .map_with(|x, e| (Pattern::Atom(PatternAtom::Num(x)), e.span())),
            // Strings
            select_ref! { Token::Strlit(x) => *x }.map_with(|x, e: &mut MapExtra<'_, '_, _, _>| {
                (
                    Pattern::Atom(PatternAtom::Strlit(Intern::from_ref(x))),
                    e.span(),
                )
            }),
            ty.clone()
                .map_with(|x, e| (Pattern::Atom(PatternAtom::Type(Intern::from(x))), e.span())),
        ))
    });
    // Expression parser
    let expression = recursive(|expr| {
        let rname = select_ref! { Token::Ident(x) => *x };
        let ident = rname.map_with(|x, e: &mut MapExtra<'_, '_, _, _>| {
            (Expr::Ident(Intern::from_ref(x)), e.span())
        });
        // Path Access
        // This is super hacky, but it does give us a nice infix operator
        let path = ident
            .pratt(vec![infix(right(9), just(Token::Dot), |x, _, y, e| {
                (
                    Expr::FieldAccess(Intern::from(x), Intern::from(y)),
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
                select_ref! { Token::Strlit(x) => *x }.map_with(
                    |x, e: &mut MapExtra<'_, '_, _, _>| {
                        (Expr::String(Intern::from_ref(x)), e.span())
                    },
                ),
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
                                Expr::Constructor(Intern::from(name), Intern::from(args)),
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
                                Expr::FieldedConstructor(Intern::from(name), Intern::from(args)),
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
                    .allow_trailing()
                    .collect::<Vec<_>>()
                    .delimited_by(just(Token::LBrace), just(Token::RBrace))
                    .map_with(|items, e| (Expr::Tuple(Intern::from(items)), e.span()))
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
                                Intern::from((Expr::Pat(lhs), lhs.1)),
                                Intern::from(rhs),
                                Intern::from(then),
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
                                Intern::from(test),
                                Intern::from(then),
                                Intern::from(otherwise),
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
                            .ignore_then(pattern)
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
                            Expr::Match(Intern::from(matchee), Intern::from(arms)),
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
                        Expr::Lambda(Intern::from(arg), Intern::from(body)),
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
                (Expr::Mul(Intern::from(x), Intern::from(y)), e.span())
            })
            .boxed(),
            // Divide
            infix(left(8), just(Token::Slash), |x, _, y, e| {
                (Expr::Div(Intern::from(x), Intern::from(y)), e.span())
            })
            .boxed(),
            // Add
            infix(left(7), just(Token::Plus), |x, _, y, e| {
                (Expr::Add(Intern::from(x), Intern::from(y)), e.span())
            })
            .boxed(),
            // Subtract
            infix(left(7), just(Token::Minus), |x, _, y, e| {
                (Expr::Sub(Intern::from(x), Intern::from(y)), e.span())
            })
            .boxed(),
            infix(
                left(5),
                select_ref! { Token::ComparisonOp(c) => *c },
                |left, op, right, e| {
                    (
                        Expr::Comparison(Intern::from(left), op, Intern::from(right)),
                        e.span(),
                    )
                },
            )
            .boxed(),
            // Calls
            infix(left(9), empty(), |func, (), arg, e| {
                (Expr::Call(Intern::from(func), Intern::from(arg)), e.span())
            })
            .boxed(),
            infix(left(8), just(Token::Arrow), |obj, _, f, e| {
                (
                    Expr::MethodAccess(Intern::from(obj), Intern::from(f)),
                    e.span(),
                )
            })
            .boxed(),
            // Field Access
            infix(left(10), just(Token::Dot), |x, _, y, e| {
                (
                    Expr::FieldAccess(Intern::from(x), Intern::from(y)),
                    e.span(),
                )
            })
            .boxed(),
        ])
        // .map(|x| (x))
        .boxed()
        .memoized()
        .labelled("expression")
        .as_context()
    });

    let definition = recursive(|_| {
        // Toplevel Let binding
        let let_binding = just(Token::Let)
            .ignore_then(ident)
            .then(ident.repeated().collect::<Vec<_>>())
            .then(just(Token::Colon).ignore_then(ty.clone()).or_not())
            .then(just(Token::Eq).ignore_then(expression.clone()))
            .map_with(|(((name, args), ty), body), e| {
                let value = args.into_iter().rev().fold(body, |acc, arg| {
                    (Expr::Lambda(Intern::from(arg), Intern::from(acc)), e.span())
                });
                Definition::Let(name, value, ty)
            })
            .labelled("let-definition")
            .as_context();

        // let def_binding = just(Token::Def)
        //     .ignore_then(ident)
        //     .then_ignore(just(Token::Myself))
        //     .then(ident.repeated().collect::<Vec<_>>())
        //     .then(just(Token::Colon).ignore_then(ty))
        //     .then(just(Token::Eq).ignore_then(expression.clone()))
        //     // .then(expression.clone())
        //     .map_with(|(((name, args), ty), body), e| {
        //         let value: (Expr, SimpleSpan<usize, u64>) =
        //             args.into_iter().rev().fold(body, |acc, arg| {
        //                 (
        //                     Expr::Lambda(Box::leak(Box::new(arg)), Box::leak(Box::new(acc))),
        //                     e.span(),
        //                 )
        //             });
        //         (name, value, ty)
        //     })
        //     .labelled("def-definition")
        //     .as_context();

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
                            parent_name: None,
                            name: (*name.0.get_ident(name.1).unwrap(), name.1),
                            types: Intern::from_ref(types.as_slice()),
                        },
                        e.span(),
                    )
                }),
            ident.map_with(|name, e| {
                (
                    EnumVariant {
                        parent_name: None,
                        name: (*name.0.get_ident(name.1).unwrap(), name.1),
                        types: Intern::from_ref(&[][..]),
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

        // let impl_group = just(Token::Impl)
        //     .ignore_then(ty)
        //     .then_ignore(just(Token::Eq))
        //     .then(def_binding.repeated().collect::<Vec<_>>())
        //     .map_with(|(the_ty, methods), _| Definition::ImplDef(ImplDef { the_ty, methods }));

        choice((
            import,
            let_binding,
            struct_def,
            extern_def,
            enum_def,
            // impl_group,
        ))
    });

    let package = just(Token::Package)
        .ignore_then(ident)
        .then_ignore(just(Token::Eq))
        .then(
            just(Token::Pub)
                .ignore_then(definition.clone().repeated().collect::<Vec<_>>())
                .or(definition.clone().repeated().collect::<Vec<_>>()),
        )
        .map_with(|(name, items), _| Package {
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

fn ty_parser<I>(
    ident: Boxed<
        'static,
        'static,
        I,
        Spanned<Expr>,
        extra::Full<Rich<'static, Token, SimpleSpan<usize, FileID>>, (), ()>,
    >,
) -> impl Parser<
    'static,
    I,
    (Ty, SimpleSpan<usize, u64>),
    extra::Full<Rich<'static, Token, SimpleSpan<usize, FileID>>, (), ()>,
>
where
    I: BorrowInput<'static, Token = Token, Span = SimpleSpan<usize, FileID>> + ValueInput<'static>,
{
    // let ident = select_ref! { Token::Ident(x) => *x }.map_with(|x, e| (Expr::Ident(x), e.span()));

    // let ident_or_self = ident
    //     .or(just(Token::Myself).map_with(|_, e| (Expr::Myself, e.span())))
    //     .boxed()
    //     .memoized();

    let ty = recursive(|ty| {
        let type_list = ty
            .clone()
            .separated_by(just(Token::Comma))
            .allow_trailing()
            .collect::<Vec<_>>();
        // Path Access
        // This is super hacky, but it does give us a nice infix operator
        let path = ident
            .clone()
            .pratt(vec![infix(left(10), just(Token::Dot), |x, _, y, e| {
                (
                    Expr::FieldAccess(Intern::from(x), Intern::from(y)),
                    e.span(),
                )
            })
            .boxed()])
            .or(ident.clone())
            .memoized();

        choice((
            // Primitive Types
            just(Token::TyNum).map_with(|_, e| (Ty::Primitive(PrimitiveType::Num), e.span())),
            just(Token::TyStr).map_with(|_, e| (Ty::Primitive(PrimitiveType::Str), e.span())),
            just(Token::TyBool).map_with(|_, e| (Ty::Primitive(PrimitiveType::Bool), e.span())),
            just(Token::TyUnit).map_with(|_, e| (Ty::Primitive(PrimitiveType::Unit), e.span())),
            just(Token::Metatype).map_with(|_, e| (Ty::Myself, e.span())),
            just(Token::Myself).map_with(|_, e| (Ty::Myself, e.span())),
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
                        Ty::User(
                            (*name.0.get_ident(name.1).unwrap(), name.1),
                            Intern::from(generics.unwrap_or_default()),
                        ),
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
                .map_with(|types, e| (Ty::Tuple(Intern::from(types)), e.span())),
        ))
        .pratt(vec![infix(right(9), just(Token::Arrow), |x, _, y, e| {
            (Ty::Arrow(Intern::from(x), Intern::from(y)), e.span())
        })])
    });
    ty
}

fn pattern_parser<'src, I>(
    ident: &'src impl Parser<
        'src,
        I,
        Spanned<Expr>,
        extra::Full<Rich<'src, Token, SimpleSpan<usize, FileID>>, (), ()>,
    >,
    ty: Boxed<
        'src,
        'src,
        I,
        Spanned<Ty>,
        extra::Full<Rich<'src, Token, SimpleSpan<usize, FileID>>, (), ()>,
    >,
) -> impl Parser<
    'src,
    I,
    (Pattern, SimpleSpan<usize, u64>),
    extra::Full<Rich<'src, Token, SimpleSpan<usize, FileID>>, (), ()>,
>
where
    I: BorrowInput<'src, Token = Token, Span = SimpleSpan<usize, FileID>> + ValueInput<'src>,
{
    let pattern = recursive(|pat| {
        // Path Access
        // This is super hacky, but it does give us a nice infix operator
        // let ty = ty_parser(ident).lazy().boxed();
        let path = ident
            .pratt(vec![infix(left(10), just(Token::Dot), |x, _, y, e| {
                (
                    Expr::FieldAccess(Intern::from(x), Intern::from(y)),
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
                .map_with(|p, e| (Pattern::Tuple(Intern::from(p)), e.span())),
            path.then(
                pat.separated_by(just(Token::Comma))
                    .collect::<Vec<_>>()
                    .delimited_by(just(Token::LBrace), just(Token::RBrace))
                    .or_not(),
            )
            .map_with(|(name, args), e| {
                if let Some(args) = args {
                    (
                        Pattern::Variant(Intern::from(name), Intern::from(args)),
                        e.span(),
                    )
                } else {
                    (
                        Pattern::Atom(PatternAtom::Variable(*name.0.get_ident(name.1).unwrap())),
                        e.span(),
                    )
                }
            }),
            // Numbers
            select_ref! { Token::Num(x) => OrderedFloat(*x) }
                .map_with(|x, e| (Pattern::Atom(PatternAtom::Num(x)), e.span())),
            // Strings
            select_ref! { Token::Strlit(x) => *x }.map_with(|x, e: &mut MapExtra<'_, '_, _, _>| {
                (
                    Pattern::Atom(PatternAtom::Strlit(Intern::from_ref(x))),
                    e.span(),
                )
            }),
            ty.clone()
                .map_with(|x, e| (Pattern::Atom(PatternAtom::Type(Intern::from(x))), e.span())),
        ))
    });
    pattern
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
        Self::new(id, self.into_range())
    }
}

/// Legacy helper function for the parser's error handlings
fn parse_failure(
    err: &Rich<'_, impl std::fmt::Display, impl AnnotateRange>,
    fid: FileID,
) -> DynamicErr {
    DynamicErr::new(err.reason().to_string())
        .label(
            err.found().map_or_else(
                || "end of input".to_string(),
                |c| format!("Unexpected '{c}'"),
            ),
            err.span().annotate(fid),
        )
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
) -> impl BorrowInput<'static, Token = Token, Span = SimpleSpan<usize, FileID>> + ValueInput<'static>
{
    toks.map(eoi, |(t, s)| (t, s))
}

/// Public parsing function. Produces a parse tree from a source string.
pub fn parse(ctx: &mut Context, fid: FileID) -> CompResult<Package> {
    let input = ctx.filectx.get(&fid).unwrap().src_text;
    let tokens: Vec<(Token, SimpleSpan<usize, u64>)> = match lexer(fid).parse(input).into_result() {
        Ok(tokens) => tokens,
        Err(errs) => {
            return Err(ErrorCollection::new(
                errs.into_iter()
                    .map(|x| parse_failure(&x, fid).into())
                    .collect(),
            )
            .into());
        }
    };

    //dbg!(&tokens);

    let packg: Result<Package, CompilerErr> = match parser(&make_input)
        .parse(make_input(
            SimpleSpan::new(fid, 0..input.len()),
            tokens.leak(),
        ))
        .into_result()
    {
        Ok(p) => Ok(p),
        Err(e) => {
            let errs = ErrorCollection::new(
                e.iter()
                    .map(|e| parse_failure(e, fid).into())
                    .collect::<Vec<CompilerErr>>(),
            );
            Err(errs.into())
        }
    };
    packg
}

// #[cfg(test)]
// mod tests {
//     use chumsky::input::MapExtra;

//     use super::*;

//     fn make_tokens(src: &'static str) -> CompResult<Vec<Spanned<Token>>> {
//         match lexer(0)
//             .parse(src )
//             .into_result()
//         {
//             Ok(tokens) => Ok(tokens),
//             Err(errs) => {
//                 Err(ErrorCollection::new(
//                     errs.into_iter()
//                         .map(|x| parse_failure(&x, 0).into())
//                         .collect(),
//                 )
//                 .into())
//                 // return Err(CompilerErrKind::Dynamic(parse_failure(&errs[0], fid)).into())
//             }
//         }
//     }
//     /// Given a source str parse a type from it
//     fn type_test(src: &'static str, rodeo: Rodeo) -> (Ty, Rodeo) {
//         let mut state = SimpleState::from(rodeo);
//         let ident =
//             select_ref! { Token::Ident(x) => *x }.map_with(|x, e: &mut MapExtra<'_, '_, _, extra::Full<_, SimpleState<Rodeo>, _>> | (Expr::Ident(e.state().get_or_intern(x)), e.span()));
//         let tokens = make_tokens(src);

//         match ty_parser(ident.boxed())
//             .parse_with_state(
//                 make_input(SimpleSpan::new(0, 0..src.len()), tokens.unwrap().leak()),
//                 &mut state,
//             )
//             .into_result()
//         {
//             Ok((t, _)) => (t, state.0),
//             Err(_) => unreachable!(),
//         }
//     }

//     macro_rules! parser_test {
//     ($test_fn:expr, [ $( ($input:expr, $pattern:pat $(if $guard:expr)?) ),* $(,)? ]) => {
//         $(
//             {
//                 let rodeo = Rodeo::new();
//                 let res = $test_fn($input);
//                 assert!(matches!(res, $pattern $(if $guard)?),
//                     "Failed for input: {}. \nGot: {:?}", $input,  res);
//             }
//         )*
//     };
// }

//     #[test]
//     #[rustfmt::skip::macros(parser_test)]
//     fn test_ty_parser() {
//         parser_test!(type_test, [
//             ("num", Ty::Primitive(PrimitiveType::Num)),
//             ("?T", Ty::Generic(_)),
//             ("User",
//                 Ty::User(
//                     ("User", _),
//                     args
//                 ) if args.is_empty()
//             ),
//             ("User[num]",
//                 Ty::User(
//                     ("User", _),
//                     args
//                 ) if args.len() == 1),
//             ("User[?T]",
//                 Ty::User(
//                     ("User", _),
//                     [
//                         (
//                             Ty::Generic(
//                                 (Expr::Ident("T"),_)
//                             ),
//                         _)
//                     ]
//                 )
//             ),
//             ("num -> num",
//                 Ty::Arrow(
//                     (Ty::Primitive(_), _),
//                     (Ty::Primitive(_), _)
//                 )
//             ),
//             ("num -> num -> num",
//                 Ty::Arrow(
//                     (Ty::Primitive(_), _),
//                     (
//                         Ty::Arrow(
//                             (Ty::Primitive(_), _),
//                             (Ty::Primitive(_), _)
//                         ),
//                     _)
//                 )
//             ),

//             ("self -> num -> num",
//                 Ty::Arrow(
//                     (Ty::Myself, _),
//                     (
//                         Ty::Arrow(
//                             (Ty::Primitive(_), _),
//                             (Ty::Primitive(_), _)
//                         ),
//                     _)
//                 )
//             ),

//             ("self -> num -> num -> num",
//                 Ty::Arrow(
//                     (Ty::Myself, _),

//                         (Ty::Arrow(
//                             (Ty::Primitive(_), _),
//                             (Ty::Arrow(
//                                 (Ty::Primitive(_), _),
//                                 (Ty::Primitive(_), _),
//                             ), _)
//                         ),
//                     _)
//                 )
//             ),
//         ]);
//     }
// }
