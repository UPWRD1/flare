use crate::{
    resource::{
        errors::{CompResult, CompilerErr, DynamicErr, ErrorCollection},
        rep::{
            common::Spanned,
            frontend::{ast::{
                self, BinOp,  Direction, Label,   Untyped,  
            }, cst::{CstExpr, Definition, ItemDefinition, MatchArm, Package, Pattern, UntypedCst}, csttypes::{CstClosedRow, CstType}, files::{FileID, FileSource}}
        },
    },
};
use chumsky::input::{BorrowInput, MapExtra, SliceInput, StrInput, ValueInput};
use chumsky::pratt::{Operator, infix, left, right};
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
    Num(f32),

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
    DoubleColon,
    Sandwich(&'static str),
    Eq,
    Dot,
    DoubleDot,
    TripleDot,
    FatArrow,
    Arrow,
    Comma,
    Pipe,

    Asterisk,
    Slash,
    Plus,
    Minus,

    ComparisonOp(BinOp),
    Ampersand,
    At,

    LBrace,
    RBrace,
    LBracket,
    RBracket,
    LParen,
    RParen,
    Question,

    And,
    Def,
    Else,
    // Enum,
    Extern,
    False,
    For,
    Fn,
    If,
    Impl,
    In,
    Let,
    Match,
    Package,
    Prop,
    Pub,
    Or,
    // Struct,
    Then,
    True,
    Type,
    Use,

    TyNum,
    TyStr,
    TyBool,
    TyUnit,
    Myself,
    Metatype,
    TyInt,
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
            Self::ComparisonOp(c) => write!(f, "{c}"),
            Self::Ampersand => write!(f, "&"),
            Self::And => write!(f, "and"),
            Self::Or => write!(f, "or"),
            Self::At => write!(f, "@"),
            Self::Fn => write!(f, "fn"),
            Self::True => write!(f, "true"),
            Self::False => write!(f, "false"),
            Self::Strlit(s) => write!(f, "\"{s}\""),
            Self::Comment(c) => write!(f, "{c}"),
            Self::Colon => write!(f, ":"),
            Self::DoubleColon => write!(f, "::"),
            Self::Sandwich(s) => write!(f, ":{s}:"),
            Self::Separator => write!(f, "newline"),
            Self::Whitespace => write!(f, "whitespace"),
            Self::Dot => write!(f, "."),
            Self::DoubleDot => write!(f, ".."),
            Self::TripleDot => write!(f, "..."),
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
            Self::Else => write!(f, "else"),
            Self::For => write!(f, "for"),
            Self::Extern => write!(f, "extern"),
            Self::Impl => write!(f, "impl"),
            Self::If => write!(f, "if"),
            Self::Match => write!(f, "match"),
            Self::Prop => write!(f, "prop"),
            Self::Pub => write!(f, "pub"),
            Self::Then => write!(f, "then"),
            Self::Type => write!(f, "type"),
            Self::Def => write!(f, "def"),
            Self::TyInt => write!(f, "int"),
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
) -> impl Parser<
    'static,
    I,
    Vec<Spanned<Token>>,
    extra::Err<Rich<'static, char, SimpleSpan<usize, ()>>>, >
where
    I: ValueInput<'static, Token = char, Span = SimpleSpan<usize, ()>>
        + SliceInput<'static, Slice = &'static str, Span = SimpleSpan<usize, ()>>
        + StrInput<'static, Span = SimpleSpan<usize, ()>>,
    
{
    let comment = just("#")
        .then_ignore(any().and_is(just('\n').not()).repeated())
        .labelled("comment")
        .padded();
    let recover_strat1 = any().padded_by(comment.repeated()).padded().map_with(
        move |x, e: &mut chumsky::input::MapExtra<'_, '_, I, extra::Err<_>>| {
            Spanned(
                Token::Error(x),
                // SimpleSpan::new(**e.state(), e.span().into_range()),
                SimpleSpan::new(id, e.span().into_range()),
            )
        },
    );

    let comparison_op = choice((
        just("<").to(Token::ComparisonOp(BinOp::Lt)),
        just("<=").to(Token::ComparisonOp(BinOp::Lte)),
        just(">").to(Token::ComparisonOp(BinOp::Gt)),
        just(">=").to(Token::ComparisonOp(BinOp::Gte)),
        just("==").to(Token::ComparisonOp(BinOp::Eq)),
        just("!=").to(Token::ComparisonOp(BinOp::Neq)),
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
                "and" => Token::And,
                "def" => Token::Def,
                "else" => Token::Else,
                "extern" => Token::Extern,
                "false" => Token::False,
                "fn" => Token::Fn,
                "if" => Token::If,
                "impl" => Token::Impl,
                "in" => Token::In,
                "int" => Token::TyInt,
                "let" => Token::Let,
                "match" => Token::Match,
                "or" => Token::Or,
                "package" => Token::Package,
                "prop" => Token::Prop,
                "pub" => Token::Pub,
                "then" => Token::Then,
                "type" => Token::Type,
                "true" => Token::True,
                "use" => Token::Use,
                "num" => Token::TyNum,
                "self" => Token::Myself,
                "Self" => Token::Metatype,
                "str" => Token::TyStr,
                "bool" => Token::TyBool,
                "unit" => Token::TyUnit,
                s => Token::Ident(s),
            }),
            // Operators
            comparison_op,
            just("&").to(Token::Ampersand),
            just("@").to(Token::At),
            just("=>").to(Token::FatArrow),
            just("->").to(Token::Arrow),
            just("=").to(Token::Eq),
            just("{").to(Token::LBrace),
            just("}").to(Token::RBrace),
            just("[").to(Token::LBracket),
            just("]").to(Token::RBracket),
            arith_op,
            just("...").to(Token::TripleDot),
            just("..").to(Token::DoubleDot),
            just(".").to(Token::Dot),
            just(",").to(Token::Comma),
            just("|").to(Token::Pipe),
            just("?").to(Token::Question),
            just("::").to(Token::DoubleColon),
            just(":").to(Token::Colon),
            
            // Numbers
            text::int(10)
                .then(just('.').then(text::digits(10)).or_not())
                .to_slice()
                .try_map(|s: &str, span| {
                    Ok(Token::Num(s.parse().map_err(|_| {
                        Rich::custom(span, "Could not parse number")
                    })?))
                })
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
            Spanned(t, SimpleSpan::new(id, e.span().into_range()))
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
) -> impl Parser<
    'static,
    I,
    Vec<Package<UntypedCst>>,
    extra::Err<Rich<'static, Token, SimpleSpan<usize, FileID>>>,
  >
where
    I: BorrowInput<'static, Token = Token, Span = SimpleSpan<usize, FileID>> + ValueInput<'static>,
    // Because this function is generic over the input type, we need the caller to tell us how to create a new input,
    // `I`, from a nested token tree. This function serves that purpose.
    M: Fn(SimpleSpan<usize, FileID>, &'static [Spanned<Token>]) -> I + 'static,
{
    let rname = select_ref! { Token::Ident(x) => *x };
    let ident = rname.map_with(|x, e: &mut MapExtra<'_, '_, _, _>| {
        Spanned(
            Intern::from(CstExpr::Ident(Untyped(Spanned(Intern::from_ref(x), e.span())))),
            e.span(),
        )
    });

    let raw_ident =
        rname.map_with(|x, e: &mut MapExtra<'_, '_, _, _>| Spanned(Intern::from_ref(x), e.span()));

    let ty = ty_parser(make_input).boxed();

   let pattern = destructure_pattern_parser(ident.boxed(), ty.clone()).boxed();    // Expression parser
    let expression = recursive(|expr| {
                // Path Access
        // This is super hacky, but it does give us a nice infix operator
        let _ = ident.pratt(vec![
            infix(
                right(9),
                just(Token::Dot),
                |x,
                 _,
                 y,
                 e: &mut MapExtra<
                    '_,
                    '_,
                    I,
                    extra::Err<Rich<'static, Token, SimpleSpan<usize, FileID>>>,
                >| {
                    Spanned(Intern::from(CstExpr::<Untyped>::FieldAccess(x, y)), e.span())
                },
            )
            .boxed(),
        ]);

        let tuple = expr
            .clone()
            .separated_by(just(Token::Comma))
            .allow_trailing()
            .at_least(1)
            .collect::<Vec<_>>()
            .delimited_by(just(Token::LBrace), just(Token::RBrace))
            .map_with(|args, _| unsafe {
                args.into_iter()
                    .enumerate()
                    .map(|(i, arg): (_, Spanned<Intern<CstExpr<Untyped>>>)| {
                        Spanned(
                            CstExpr::Label(Label(Spanned(i.to_string().into(), arg.1)), arg).into(),
                            arg.1,
                        )
                    })
                    .reduce(|l, r| Spanned(CstExpr::Concat(l, r).into(), l.1.union(r.1)))
                    .unwrap_unchecked()
            })
            .labelled("tuple")
            .as_context();

        let table = raw_ident
            .then_ignore(just(Token::Eq))
            .then(expr.clone())
            .separated_by(just(Token::Comma))
            .allow_trailing()
            .at_least(1)
            .collect::<Vec<_>>()
            .delimited_by(just(Token::LBrace), just(Token::RBrace))
            .map_with(|args, _e| {
                if args.is_empty() {
                    unreachable!()
                } else {
                    unsafe {
                        args.into_iter()
                            .map(|(field_name, arg)| -> Spanned<Intern<CstExpr<Untyped>>> {
                                Spanned(
                                    CstExpr::Label(Label(Spanned(field_name.0, arg.1)), arg).into(),
                                    field_name.1.union(arg.1),
                                )
                            })
                            .reduce(|l, r| Spanned(CstExpr::Concat(l, r).into(), l.1.union(r.1)))
                            .unwrap_unchecked()
                    }
                }
            })
            .labelled("table")
            .as_context();
        let sum = raw_ident
            
            .then(expr.clone().or_not())
            .delimited_by(just(Token::Pipe), just(Token::Pipe))
            .map_with(|(name, val), e| {
               Spanned(
                    CstExpr::Inject(
                        Direction::Right,
                        name.convert(
                            CstExpr::Label(
                                Label(name),
                                val.unwrap_or_else(||Spanned(CstExpr::Unit.into(), e.span()))
                            )
                        )
                    ).into(),
                    e.span()
                )
              }).labelled("sum").as_context();

        let atom = recursive(|_atom| {
            choice((
                // Numbers
                select_ref! { Token::Num(x) => CstExpr::Number(OrderedFloat(*x)) }
                    .map_with(|x, e| Spanned(Intern::from(x), e.span())),
                // Strings
                select_ref! { Token::Strlit(x) => *x }.map_with(
                    |x, e: &mut MapExtra<'_, '_, _, _>| {
                        Spanned(
                            Intern::from(CstExpr::String(Spanned(Intern::from_ref(x), e.span()))),
                            e.span(),
                        )
                    },
                ),
                // True
                just(Token::True)
                    .map_with(|_, e| Spanned(Intern::from(CstExpr::Bool(true)), e.span())),
                // False
                just(Token::False)
                    .map_with(|_, e| Spanned(Intern::from(CstExpr::Bool(false)), e.span())),

// Unit
                just(Token::TyUnit)
                    .map_with(|_, e| Spanned(Intern::from(CstExpr::Unit), e.span())),
                // Tuple Constructors
                tuple,
                table,
                sum,
                               ident,
                just(Token::At)
                    .ignore_then(raw_ident)
                    .map_with(|id, e| Spanned(Intern::from(CstExpr::Particle(id)), e.span())),
                // let x = y in z
                just(Token::Let)
                    .ignore_then(raw_ident)
                    .then_ignore(just(Token::Eq))
                    .then(expr.clone())
                    .then_ignore(just(Token::In))
                    .then(expr.clone())
                    .map_with(|((lhs, rhs), then), e| {
                        Spanned(Intern::from(CstExpr::Let(Untyped(lhs), rhs, then)), e.span())
                    }),
                // If expression
                just(Token::If)
                    .ignore_then(expr.clone())
                    .then_ignore(just(Token::Then))
                    .then(expr.clone())
                    .then_ignore(just(Token::Else))
                    .then(expr.clone())
                    .map_with(|((test, then), otherwise), e| {
                        Spanned(Intern::from(CstExpr::If(test, then, otherwise)), e.span())
                    })
                    .labelled("if expression")
                    .as_context(),
                // Match Expression
                just(Token::Match)
                    .ignore_then(expr.clone()).then_ignore(just(Token::In))
                    .then(
                        
                    pattern
                        .then_ignore(just(Token::Then))
                        .then(expr.clone())
.map(|(pat, body)| {MatchArm{pat, body}})                            .separated_by(just(Token::Comma))
                            // .repeated()
                            .allow_trailing()
                            .at_least(1)
                            .collect::<Vec<_>>(),
                    )
                    .map_with(|(matchee, arms), e| {
                        Spanned(
                            Intern::from(CstExpr::Match(matchee, arms.leak())),
                            
                            e.span(),
                        )
                    }),
            ))
        })
        .boxed();
        //.memoized();

        choice((
            atom,
            // fn x y => z
            just(Token::Fn).ignore_then(raw_ident.repeated().foldr_with(
                just(Token::FatArrow).ignore_then(expr.clone()),
                |arg, body, e| {
                    Spanned(
                        Intern::from(CstExpr::Lambda(Untyped(arg), body)),
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
                Spanned(Intern::from(CstExpr::Mul(x, y)), e.span())
            })
            .boxed(),
            // Divide
            infix(left(8), just(Token::Slash), |x, _, y, e| {
                Spanned(Intern::from(CstExpr::Div(x, y)), e.span())
            })
            .boxed(),
            // Add
            infix(left(7), just(Token::Plus), |x, _, y, e| {
                Spanned(Intern::from(CstExpr::Add(x, y)), e.span())
            })
            .boxed(),
            // Subtract
            infix(left(7), just(Token::Minus), |x, _, y, e| {
                Spanned(Intern::from(CstExpr::Sub(x, y)), e.span())
            })
            .boxed(),
            infix(
                left(5),
                select_ref! { Token::ComparisonOp(c) => *c },
                |left, op, right, e| {
                    Spanned(Intern::from(CstExpr::Comparison(left, op, right)), e.span())
                },
            )
            .boxed(),

            infix(
                left(5),
                just(Token::And),
                |left, _, right, e| {
                    Spanned(Intern::from(CstExpr::Comparison(left, BinOp::And, right)), e.span())
                    
                },
            )
            .boxed(),

            infix(
                left(5),
                just(Token::Or),
                |left, _, right, e| {
                    Spanned(Intern::from(CstExpr::Comparison(left, BinOp::Or, right)), e.span())
                    
                },
            )
            .boxed(),
            
            // Calls
            infix(left(9), empty(), |func, (), arg, e| {
                Spanned(Intern::from(CstExpr::Call(func, arg)), e.span())
            })
            .boxed(),
            infix(left(8), just(Token::DoubleColon), |obj, _, method, e| {
                Spanned(Intern::from(CstExpr::MethodAccess{obj, prop: None, method}), e.span())
            })
            .boxed(),
            infix(left(8), select! {Token::Sandwich(p) = e => Some(Spanned(String::from(p).into(), e.span()))}, |obj, prop, method, e| {
                Spanned(Intern::from(CstExpr::MethodAccess{obj, prop, method}), e.span())
            })
            .boxed(),            // Field Access
            infix(left(10), just(Token::Dot), |x, _, y, e| {
                Spanned(Intern::from(CstExpr::FieldAccess(x, y)), e.span())
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
            .ignore_then(raw_ident)
            .then(raw_ident.repeated().collect::<Vec<_>>())
            .then_ignore(just(Token::Colon))
            .then(ty.clone())
            .then_ignore(just(Token::Eq))
            .then(expression.clone())
            .try_map_with(|(((name, args), ty), body), e| {
              let value = args
                    .iter()
                    .rev()
                    .fold(body, |acc, arg| {
                        Spanned(
                            CstExpr::Lambda(Untyped(*arg), acc).into(),
                            e.span(),
                        )
                    });
                Ok(Definition::Let(name, value, ty))
             
            })
            .labelled("let-definition")
            .as_context();

        let type_def = just(Token::Type)
            .ignore_then(raw_ident)
            .then(just(Token::Question)
                .ignore_then(raw_ident)
                .separated_by(just(Token::Comma))
                .at_least(1).collect::<Vec<_>>().delimited_by(just(Token::LBracket), just(Token::RBracket)).or_not()).then_ignore(just(Token::Eq)).then(ty.clone()).map(|((name, generics), ty)| {
            let generics = generics.unwrap_or_default().into_iter().map(|x| x.convert(CstType::Generic(x))).collect::<Vec<_>>().leak();
            Definition::Type(name, generics, ty)
        });
        
        // Import
        let import_path = expression;       
        let import = just(Token::Use)
            .ignore_then(import_path)
            .map(Definition::Import);

        let extern_def = just(Token::Extern)
            .ignore_then(raw_ident).then(raw_ident.repeated().collect::<Vec<_>>())
            .then_ignore(just(Token::Colon))
            .then(ty)
            .map(|((name, args), ty)| Definition::Extern(name, args.into_iter().map(Untyped).collect::<Vec<_>>().leak(), ty));

        choice((
            import,
            let_binding,
            type_def,
            extern_def,
))
    });

    let package = just(Token::Package)
        .ignore_then(raw_ident)
        .then_ignore(just(Token::Eq))
        .then(
            just(Token::Pub)
                .or_not()
                .then(definition.clone()
        ).repeated().collect::<Vec<_>>()                ,
        )
        .map_with(|(name, items), _| {
            let items = items.into_iter().map(|(is_pub, def)| {
                ItemDefinition {
                    def,
                    is_pub: is_pub.is_some()
                }
                
            }).collect();
            Package {
            name ,
            items,
        }})
        .labelled("package")
        .as_context();
    
    // Program
    package
        .repeated()
        .at_least(1)
        .collect::<Vec<_>>()
        .then_ignore(end())
}

fn ty_parser<'src, I, M>(
    make_input: &'src M,
) -> impl Parser<'src, I, Spanned<Intern<CstType>>, extra::Err<Rich<'src, Token, SimpleSpan<usize, FileID>>>>
where
    I: BorrowInput<'src, Token = Token, Span = SimpleSpan<usize, FileID>> + ValueInput<'src>,
    M: Fn(SimpleSpan<usize, FileID>, &'src [Spanned<Token>]) -> I + 'src,
{
    let rname = select_ref! { Token::Ident(x) => *x };

    let raw_ident =
        rname.map_with(|x, e: &mut MapExtra<'_, '_, _, _>| Spanned(Intern::from_ref(x), e.span()));

    recursive(
        |ty: Recursive<dyn Parser<'_, I, Spanned<Intern<CstType>>, _>>| {
            let type_list = ty
                .clone()
                .separated_by(just(Token::Comma))
                .allow_trailing()
                .collect::<Vec<_>>();

            let tuple = type_list
                .clone()
                .delimited_by(just(Token::LBrace), just(Token::RBrace))
                .map_with(|types, e| {
                    let ty = CstType::Prod(CstClosedRow {
                        fields: types
                            .iter()
                            .enumerate()
                            .map(|(i, x)| Label(Spanned(i.to_string().into(), x.1)))
                            .collect::<Vec<_>>()
                            .leak(),
                        values: types.leak(),
                    });
                    Spanned(Intern::from(ty), e.span())
                });

            let table = raw_ident
                .then_ignore(just(Token::Colon))
                .then(ty.clone())
                .separated_by(just(Token::Comma))
                .allow_trailing()
                .collect::<Vec<_>>()
                .then(just(Token::TripleDot).map_with(|_, e| e.span()).or_not())
                
                .delimited_by(just(Token::LBrace), just(Token::RBrace))
                .map_with(|(types, is_open), e| {
                    let (fields, values): (
                        Vec<Spanned<Intern<String>>>,
                        Vec<Spanned<Intern<CstType>>>,
                    ) = types.into_iter().unzip();
                    if let Some(id) = is_open {
                        let l_ty = CstType::Prod(CstClosedRow {
                            fields: fields
                                .iter()
                                .map(|name| Label(*name))
                                .collect::<Vec<_>>()
                                .leak(),
                            values: values.leak(),
                        });

                        todo!()
                        // let l_ty = Spanned(Intern::from(l_ty), e.span());
                        // let ty = CstType::Subtable(l_ty, id);
                        // Spanned(Intern::from(ty), e.span())
                    } else {
                        let ty = CstType::Prod(CstClosedRow {
                            fields: fields
                                .iter()
                                .map(|name| Label(*name))
                                .collect::<Vec<_>>()
                                .leak(),
                            values: values.leak(),
                        });
                        Spanned(Intern::from(ty), e.span())
                    }
                });

            let sum = raw_ident.then(ty.clone().or_not())
                .separated_by(just(Token::Comma))
                .at_least(1)
                .collect::<Vec<_>>()
                .delimited_by(just(Token::Pipe), just(Token::Pipe))
                .map_with(|types, e| {

                    
                    let (fields, values): (
                        Vec<Spanned<Intern<String>>>,
                        Vec<Option<Spanned<Intern<CstType>>>>,
                    ) = types.into_iter().unzip();
                        let ty = CstType::Sum(CstClosedRow {
                            fields: fields
                                .iter()
                                .map(|name| Label(*name))
                                .collect::<Vec<_>>()
                                .leak(),
                            values: values.iter().enumerate().map(|(i, x)| x.unwrap_or(fields[i].convert(CstType::Unit))).collect::<Vec<_>>().leak(),
                        });
                        // dbg!(ty);
                        Spanned(Intern::from(ty), e.span())
                    
                });
            
            
            
            let grouping = Parser::nested_in(
                ty.clone(),
                select_ref! { Token::Parens(ts) = e => make_input(e.span(), ts) },
            );

            // Atomic types (no arrows at this level)
            let atom = choice((
                // Primitive Types
                just(Token::TyNum).map_with(|_, e| Spanned(Intern::from(CstType::Num), e.span())),
                just(Token::TyStr).map_with(|_, e| Spanned(Intern::from(CstType::String), e.span())),
                just(Token::TyBool).map_with(|_, e| Spanned(Intern::from(CstType::Bool), e.span())),
                just(Token::TyUnit).map_with(|_, e| Spanned(Intern::from(CstType::Unit), e.span())),


raw_ident
                    .then(
                        type_list
                            .clone()
                            .delimited_by(just(Token::LBracket), just(Token::RBracket))
                            .or_not(),
                    )
                    .clone()
                    .map_with(|(name, generics), e| {
                        Spanned(Intern::from(CstType::User(name, generics.unwrap_or_default().leak())), e.span())
                    }),
                just(Token::At)
                    .ignore_then(raw_ident)
                    .map_with(|name, e| Spanned(Intern::from(CstType::Particle(name)), e.span())),
                just(Token::Question)
                    .ignore_then(raw_ident)
                    .map_with(|name, e| Spanned(Intern::from(CstType::Generic(name)), e.span())),
                                
                tuple,
                table,
                sum,
                grouping,
            ));

            // Apply pratt parser for arrows at this level
            atom.pratt(vec![infix(
                right(9),
                just(Token::Arrow),
                |x: Spanned<Intern<CstType>>, _, y, e| Spanned(Intern::from(CstType::Func(x, y)), e.span()),
            )])
        },
    )
    .boxed()
}

fn destructure_pattern_parser<'src, I>(
    _ident: Boxed<
        'src,
        'src,
        I,
        Spanned<Intern<CstExpr<Untyped>>>,
        extra::Full<Rich<'src, Token, SimpleSpan<usize, FileID>>, (), ()>,
    >,
    _ty: Boxed<
        'src,
        'src,
        I,
        Spanned<Intern<CstType>>,
        extra::Full<Rich<'src, Token, SimpleSpan<usize, FileID>>, (), ()>,
    >,
) -> impl Parser<'src, I, Spanned<Intern<Pattern<Untyped>>>, extra::Full<Rich<'src, Token, SimpleSpan<usize, FileID>>, (), ()>,
> where I: BorrowInput<'src, Token = Token, Span = SimpleSpan<usize,FileID>> + ValueInput<'src> {

let rnum = select_ref! { Token::Num(n) => *n }.map_with(|v, e| Spanned(Pattern::Number(OrderedFloat::from(v as f64)).into(), e.span()));
let rname = select_ref! { Token::Ident(x) => *x };

    let raw_ident =
        rname.map_with(|x, e: &mut MapExtra<'_, '_, _, _>| Spanned(Intern::from_ref(x), e.span()));

    recursive(|pat| {
        choice((
just(Token::LBrace)
                .ignore_then(
pat.clone()
    
                        .separated_by(just(Token::Comma)).at_least(1).collect::<Vec<_>>()               )
                .then_ignore(just(Token::RBrace))
                .map_with(|fields, e|{
                    let fields = fields
                        .leak();
                Spanned(Pattern::Tuple(fields).into(), e.span())
            }),
        just(Token::Pipe)
                .ignore_then(choice((
                    raw_ident.then(pat.clone())
                        .map(|(label, term)| 
                            Pattern::Variant(ast::Label(label), term)
                        ),
                    
                    raw_ident.map(|label| {
                        Pattern::Variant(ast::Label(label), label.convert(Pattern::Any))
                    
                    })
                        
                )))
                .then_ignore(just(Token::Pipe))
                .map_with(|term, e| Spanned(term.into(), e.span())), 

just(Token::LBrace)
                .ignore_then(
raw_ident.then_ignore(just(Token::Eq)).then(pat.clone())
                        .map(|(label, term)| 
                            (ast::Label(label), term)
                        ).separated_by(just(Token::Comma)).collect::<Vec<_>>()               )
                .then_ignore(just(Token::RBrace))
                .map_with(|fields, e|{
                    let fields = fields
                        .leak();
                Spanned(Pattern::Record{ fields,open:false}.into(), e.span())
            }),
             just(Token::At)
                .ignore_then(raw_ident)
                .map_with(|id, e| Spanned(Intern::from(Pattern::Particle(id)), e.span())),
            rnum,

            raw_ident
                .map_with(|x, e| Spanned(Pattern::Var(Untyped(x)).into(), e.span()))
     ))
    }
    )
}


/// Trait that extends `SimpleSpan` to permit adding `FileID` information
pub trait AnnotateRange: std::fmt::Display {
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

/// Helper function for the parser's error handling
fn parse_failure(
    err: &Rich<'_, impl std::fmt::Display, impl AnnotateRange>,
    fid: FileID,
) -> DynamicErr {
    // eprintln!("{}\n{}", err.reason(), err.found().unwrap());
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
    toks: &'_ [Spanned<Token>],
) -> impl BorrowInput<'_, Token = Token, Span = SimpleSpan<usize, FileID>> + ValueInput<'_> {
    toks.map(eoi, |ts| (&ts.0, &ts.1))
}

/// Public parsing function. Produces a parse tree from a source string.
pub fn parse(file: &FileSource) -> CompResult<Vec<Package<UntypedCst>>> {
    let input: &'static str = file.source.clone().leak();
    let tokens: Vec<Spanned<Token>> = match lexer(file.id).parse(input).into_result() {
        Ok(tokens) => tokens,
        Err(errs) => {
            return Err(ErrorCollection::new(
                errs.into_iter()
                    .map(|x| parse_failure(&x, file.id).into())
                    .collect(),
            )
            .into());
        }
    };

    let packg: Result<Vec<Package<UntypedCst>>, CompilerErr> = match parser(&make_input)
        .parse(make_input(
            SimpleSpan::new(file.id, 0..input.len()),
            tokens.leak(),
        ))
        .into_result()
    {
        Ok(p) => Ok(p),
        Err(e) => {
            let errs = ErrorCollection::new(
                e.iter()
                    .map(|e| parse_failure(e, file.id).into())
                    .collect::<Vec<CompilerErr>>(),
            );
            Err(errs.into())
        }
    };
    packg
}

#[cfg(test)]
mod tests {
    use super::*;

    fn make_tokens(src: &'static str) -> CompResult<Vec<Spanned<Token>>> {
        match lexer(0).parse(src).into_result() {
            Ok(tokens) => Ok(tokens),
            Err(errs) => {
                Err(ErrorCollection::new(
                    errs.into_iter()
                        .map(|x| parse_failure(&x, 0).into())
                        .collect(),
                )
                .into())
            }
        }
    }
    /// Given a source str parse a type from it
    fn type_test(src: &'static str) -> CstType {
        let tokens = make_tokens(src);

        ty_parser(&make_input)
            .parse(make_input(
                SimpleSpan::new(0, 0..src.len()),
                tokens.expect("Could not parse test").leak(),
            ))
            .into_result().map_or_else(|_| unreachable!(), |t| *t.0)
    }

    macro_rules! parser_test {
    ($test_fn:expr, [ $( ($input:expr, $pattern:pat $(if $guard:expr)?) ),* $(,)? ]) => {
        $(
            {
                let res = $test_fn($input);
                assert!(matches!(res, $pattern $(if $guard)?),
                    "Failed for input: {}. \nGot: {:?}", $input,  res);
            }
        )*
    };
}

    #[test]
    #[rustfmt::skip::macros(parser_test)]
    fn test_ty_parser() {
        parser_test!(type_test, [
            ("num", CstType::Num),
            // ("?T", Type::Var(TypeVar(_))),
            ("num -> num",
                CstType::Func(
                    a,
                    b
                ) if a == b && matches!(*a.0, CstType::Num)
            ),
            ("num -> num -> num",
               CstType::Func(
                    a,
                    b,
                    
                ) if matches!(*a.0, CstType::Num)
                    && matches!(*b.0, CstType::Func(c, d)
                        if c == d && matches!(*c.0, CstType::Num))
           ),          
        ]);
    }
}
