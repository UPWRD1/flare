
use std::ops::Range;

use chumsky::extra::Full;
use chumsky::extra::SimpleState;
use chumsky::input::BorrowInput;
use chumsky::input::SliceInput;
use chumsky::input::StrInput;
use chumsky::input::ValueInput;
use chumsky::pratt::*;
use chumsky::prelude::*;
use chumsky::span::Span;
use ordered_float::OrderedFloat;

use crate::resource::rep::ComparisonOp;
use crate::resource::rep::FileID;
use crate::resource::rep::PrimitiveType;
use crate::resource::{
    errors::{
        CompResult, CompilerErrKind, DynamicErr
    }, 
    rep::{
        Pattern, PatternAtom, Ty, Definition, Expr, ImportItem, Package, Spanned, StructDef
    }
};


/// Type representing the tokens produced by the lexer. Is private, since tokens are only used in the first stage of parsing.
#[derive(Debug, Clone, PartialEq)]
#[allow(dead_code)]
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
    TyUnit
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
            
        }
    }
}


/// The primary lexer function.
fn lexer<'src, I,>() -> impl Parser<'src, I, Vec<(Token<'src>, SimpleSpan<usize, u64>)>, extra::Full<Rich<'src, char, SimpleSpan<usize, ()>>, SimpleState<u64>, () >/*extra::Err<Rich<'src, char>>*/>  
    //where I:  BorrowInput<'src, Token = char, Span = SimpleSpan<usize, ()>> + ValueInput<'src, Token = char, Span = SimpleSpan<usize, ()>> + StrInput<'src> + SliceInput<'src>,
    where I: ValueInput<'src, Token = char, Span = SimpleSpan<usize, ()>> + SliceInput<'src, Slice = &'src str, Span = SimpleSpan<usize, ()>> + StrInput<'src, Span = SimpleSpan<usize, ()>>,
        

    //where I: str
    {
    let comment = just("#")
        .then_ignore(any().and_is(just('\n').not()).repeated())
        .labelled("comment")
        .padded();
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
            just("<").to(Token::ComparisonOp(ComparisonOp::Lt)),
            just("<=").to(Token::ComparisonOp(ComparisonOp::Lte)),
            just(">").to(Token::ComparisonOp(ComparisonOp::Gt)),
            just(">=").to(Token::ComparisonOp(ComparisonOp::Gte)),
            just("==").to(Token::ComparisonOp(ComparisonOp::Eq)),
            just("!=").to(Token::ComparisonOp(ComparisonOp::Neq)),

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
        .map_with( |t, e: &mut chumsky::input::MapExtra<'src, '_, I, extra::Full<Rich<'src, char, SimpleSpan<usize, ()>>, SimpleState<u64>, () >>|{  (t, SimpleSpan::new(e.state().clone(), e.span().into_range()))})
    })
    .padded_by(comment.repeated())
    .padded()
    .repeated()
    .collect().boxed()
}

/// The main parser function.
fn parser<'tokens, 'src: 'tokens, I, M>(
    fid: FileID,
    make_input: M,
) -> impl Parser<'tokens, I, Package, extra::Full<Rich<'tokens, Token<'src>, SimpleSpan<usize, FileID>>, SimpleState<u64>, () >>
//) -> impl Parser<'tokens, I, Package, extra::Err<Rich<'tokens, Token<'src>, SimpleSpan<usize, FileID>>>>
where
    I: BorrowInput<'tokens, Token = Token<'src>, Span = SimpleSpan<usize, FileID>,>,
    // Because this function is generic over the input type, we need the caller to tell us how to create a new input,
    // `I`, from a nested token tree. This function serves that purpose.
    M: Fn(SimpleSpan<usize, FileID>, &'tokens [Spanned<Token<'src>>]) -> I + Clone + 'src,
{
    // Basic tokens

    //    let whitespace = select_ref! { Token::Whitespace => () }.ignored();

    let ident = select_ref! { Token::Ident(x) => *x }
        .map_with(|x, e| (Expr::Ident(x.to_string()),  e.span()));

    let ty = recursive(|ty| {
        let type_list = ty.clone().separated_by(just(Token::Comma)).collect::<Vec<_>>();
        let path = 
            // Path Access
            // This is super hacky, but it does give us a nice infix operator
        ident.pratt(vec![infix(left(10), just(Token::Dot), |x, _, y, e| {
                (Expr::FieldAccess(Box::new(x), Box::new(y)),  e.span())
            }).boxed()]).or(ident).memoized();
        choice((
                        // Primitive Types
            just(Token::TyNum).map_with(|_, e| (Ty::Primitive(PrimitiveType::Num),  e.span()).into()),
            just(Token::TyStr).map_with(|_, e| (Ty::Primitive(PrimitiveType::Str),  e.span()).into()),
            just(Token::TyBool).map_with(|_, e| (Ty::Primitive(PrimitiveType::Bool), e.span()).into()),
            just(Token::TyUnit).map_with(|_, e| (Ty::Primitive(PrimitiveType::Unit),  e.span()).into()),

            // User Types
            path.clone().then(type_list.clone().delimited_by(just(Token::LBracket), just(Token::RBracket)).or_not()).clone().map_with(|(name, generics), e| (Ty::User(name.into(), generics.unwrap_or_default()),  e.span()).into()),
            // Arrow Type
            //ty.clone()
            // Generic Type
            just(Token::Question).ignore_then(ident).map_with(|name, e| (Ty::Generic(name.into()),  e.span()).into()),
            // Tuple
            type_list.clone().delimited_by(just(Token::LBrace), just(Token::RBrace)).map_with(|types, e| {let len = types.len(); (Ty::Tuple(types, len),  e.span())}.into()),
            
        )).pratt(vec![infix(right(9), just(Token::Arrow), |x, _, y, e| {
                (Ty::Arrow(Box::new(x), Box::new(y)),  e.span()).into()
            })])
    });

    // Pattern parser.
    let pattern = recursive(|pat| {
        let path = 
            // Path Access
            // This is super hacky, but it does give us a nice infix operator
        ident.pratt(vec![infix(left(10), just(Token::Dot), |x, _, y, e| {
                (Expr::FieldAccess(Box::new(x), Box::new(y)),  e.span())
            }).boxed()]).or(ident).memoized();
        choice((
            pat.clone().separated_by(just(Token::Comma)).collect::<Vec<Spanned<Pattern>>>().delimited_by(just(Token::LBrace), just(Token::RBrace))
                //.map_with(|p, e| (Pattern::Tuple(p), e.span())),
                .map_with(|p, e| (Pattern::Tuple(p),  e.span())),

            path.then(pat.separated_by(just(Token::Comma)).collect::<Vec<_>>().delimited_by(just(Token::LBrace), just(Token::RBrace)).or_not())
                .map_with(|(name, args), e| {
                    if let Some(args) = args {
                        (Pattern::Variant(Box::new(name), args),  e.span())
                    } else {
                        (Pattern::Atom(PatternAtom::Variable(name.0.get_ident().unwrap())),  e.span())
                    }
                }),
            //select_ref! { Token::Ident(x) => *x }.map_with(|x, e| (Pattern::Atom(PatternAtom::Variable(x.to_string())), e.span())),
                        // Numbers
            select_ref! { Token::Num(x) => OrderedFloat(*x) }
.map_with(|x, e| (Pattern::Atom(PatternAtom::Num(x)), e.span())),            
            // Strings
            select_ref! { Token::Strlit(x) => (*x).to_string() }
.map_with(|x, e| (Pattern::Atom(PatternAtom::Strlit(x.to_string())), e.span()),            
        )))

        
    });


    // Expression parser
    let expression = recursive(|expr| {
        let rname = select_ref! { Token::Ident(x) => *x };
        let ident = rname.map_with(|x, e| (Expr::Ident(x.to_string()),  e.span()));
        let path = 
            // Path Access
            // This is super hacky, but it does give us a nice infix operator
        ident.pratt(vec![infix(right(9), just(Token::Dot), |x, _, y, e| {
                (Expr::FieldAccess(Box::new(x), Box::new(y)),  e.span())
            }).boxed()]).memoized();
        
        let atom =         recursive(|atom| {
            
            choice((
            // Numbers
            select_ref! { Token::Num(x) => Expr::Number(OrderedFloat(*x)) }
                .map_with(|x, e| (x,  e.span())),
            
            // Strings
            select_ref! { Token::Strlit(x) => Expr::String((*x).to_string()) }
                .map_with(|x, e| (x,  e.span())),
            
            // True
            just(Token::True).map_with(|_, e| (Expr::Bool(true),  e.span())),
            
            // False
            just(Token::False).map_with(|_, e| (Expr::Bool(false),  e.span())),

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

            path.clone().then(ident.then_ignore(just(Token::Eq)).then(expr.clone()).separated_by(just(Token::Comma)).collect::<Vec<_>>().delimited_by(just(Token::LBrace), just(Token::RBrace)).or_not())
                .map_with(|(name, args), e| {
                    if let Some(args) = args {
                        (Expr::FieldedConstructor(Box::new(name), args),  e.span())
                    } else {
                        name
                    }
                }).labelled("struct constructor"),


            // Enum Constructors
            path.clone().then(atom.clone().separated_by(just(Token::Comma)).collect::<Vec<_>>().delimited_by(just(Token::LBrace), just(Token::RBrace)).or_not())
                .map_with(|(name, args), e| {
                    if let Some(args) = args {
                        (Expr::Constructor(Box::new(name), args),  e.span())
                    } else {
                        name
                    }
                }).labelled("enum constructor"),

            
            expr.clone().separated_by(just(Token::Comma)).collect::<Vec<_>>().delimited_by(just(Token::LBrace), just(Token::RBrace)).map_with(|items, e| (Expr::Tuple(items),  e.span())).labelled("tuple").as_context(),

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
                        Expr::Let(Box::new((Expr::Pat(lhs.clone()),  lhs.1)), Box::new(rhs), Box::new(then)),
                        
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
                }).labelled("if expression").as_context(),

            // Match Expression
            just(Token::Match)
                .ignore_then(expr.clone())
                .then(just(Token::Pipe).ignore_then(pattern).then_ignore(just(Token::Then)).then(expr.clone()).map(|(p,e)| (p, Box::new(e))).repeated().collect::<Vec<_>>()).map_with(|(matchee, arms), e| {(Expr::Match(Box::new(matchee), arms),  e.span())})
        ))}).boxed()  ;
        //.memoized();

        choice((
            atom,

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
                (Expr::Mul(Box::new(x), Box::new(y)),  e.span())
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
                (Expr::Sub(Box::new(x), Box::new(y)),  e.span())
            })
            .boxed(),

            infix(left(5), select_ref! { Token::ComparisonOp(c) => *c }, |left, op, right, e| {
                (Expr::Comparison(Box::new(left), op, Box::new(right)), e.span())
            })
            .boxed(),
            // Calls
            infix(left(9), empty(), |func, _, arg, e| {
                (Expr::Call(Box::new(func), Box::new(arg)),  e.span())
            })
            .boxed(),
            // Field Access
            infix(left(10), just(Token::Dot), |x, _, y, e| {
                (Expr::FieldAccess(Box::new(x), Box::new(y)),  e.span())
            }).boxed(),
        ])
        .labelled("expression")
        .as_context()
    });

    let definition = recursive(|_| {
        // Toplevel Let binding
        // let let_binding = just(Token::Let)
        //     .ignore_then(ident)
        //     .then(
        //         ident.repeated().foldr_with(
        //         just(Token::Eq).ignore_then(expression.clone()),
        //         |arg, body, e| Spanned::new(Expr::Lambda(Box::new(arg), Box::new(body)), e.span()),
        //     ))
        //     .map(|(name, value)| Definition::Let(name, value, None));
        let let_binding = just(Token::Let)
            .ignore_then(ident).then(
                just(Token::Colon).ignore_then(ty.clone()).or_not()
            )
            .then(
                ident.repeated().foldr_with(
                just(Token::Eq).ignore_then(expression.clone()),
                |arg, body, e| (Expr::Lambda(Box::new(arg), Box::new(body)), e.span()),
            ))
            .map(|((name, ty), value)| Definition::Let(name, value, ty));
        

        // Struct definition
        let struct_field = ident
            .then_ignore(just(Token::Colon))
            .then(ty.clone());

        let struct_def = just(Token::Struct)
            .ignore_then(ident)
            //.then(ident.separated_by(just(Token::Comma)).delimited_by(just(Token::LBracket), just(Token::RBracket)).collect::<Vec<_>>())
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

        
        let extern_def = just(Token::Extern).ignore_then(ident).then_ignore(just(Token::Colon)).then(ty.clone()).map(|(name, ty)| Definition::Extern(name, ty));

        choice((import, let_binding, struct_def, extern_def))
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
fn parse_failure(err: &Rich<'_, impl std::fmt::Display, impl AnnotateRange>, src: &str, fid: FileID) -> DynamicErr  {

    DynamicErr::new(err.reason().to_string())
        .label((
            err.found()
                .map(|c| c.to_string())
                .unwrap_or_else(|| "end of input".to_string()),
            err.span().annotate(fid),
            //err.span()
        ))
        .extra_labels(
            err.contexts()
                .map(|(l, s)| (format!("while parsing this {l}"), s.annotate(fid)))
                .collect(),
        )
        .src(src.to_string())
}

/// Compiler black magic function that transforms the token vector into a parsable item.
fn make_input<'src>(
    eoi: SimpleSpan<usize, FileID>,
    toks: &'src [(Token<'src>, SimpleSpan<usize, FileID>)],
) -> impl BorrowInput<'src, Token = Token<'src>, Span = SimpleSpan<usize, FileID>> {
    //toks.map(eoi, |(t, s)| (t, s))
    toks.map(eoi, |(t, s)| (t, s))

}


/// Public parsing function. Produces a parse tree from a source string.
pub fn parse(input: &str, fid: FileID) -> CompResult<Package> {
    let tokens = match lexer().parse_with_state(input, &mut SimpleState::from(0)).into_result() {
        Ok(tokens) => tokens,
        Err(errs) => return Err(CompilerErrKind::Dynamic(parse_failure(&errs[0], input, fid)).into()),
    };

    //dbg!(&tokens);

    let packg = match parser(fid.clone(), make_input)
        .parse_with_state(make_input(SimpleSpan::new(fid, 0..input.len()), &tokens), &mut SimpleState::from(0))
        .into_result()
    {
        Ok(p) => Ok(p),
        Err(e) => {
            Err(CompilerErrKind::Dynamic(parse_failure(
                e.first().unwrap(), //SimpleSpan::new(fid, e.first().unwrap().span()),
                input,
                fid,
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
