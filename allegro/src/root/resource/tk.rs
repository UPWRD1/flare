use logos::{Lexer, Logos, Skip};

fn word_callback(lex: &mut Lexer<Tk>) -> (usize, usize) {
    let line = lex.extras.0;
    let column = lex.span().start - lex.extras.1;

    (line, column)
}

fn newline_callback(lex: &mut Lexer<Tk>) -> Skip {
    lex.extras.0 += 1;
    lex.extras.1 = lex.span().end;
    Skip
}

///Enum for type of token
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Logos)]
#[logos(skip r"[\t\f]+")] // Ignore this regex pattern between tokens
#[logos(extras = (usize, usize))]
pub enum Tk {
    #[regex(" ", logos::skip)]
    TkSpace,
    #[token(";")]
    TkSemicolon,
    #[regex(r"\n", newline_callback)]
    Newline,
    #[regex("--.*", logos::skip)]
    TkComment,
    #[token("with", word_callback)]
    TkKwWith((usize, usize)),
    #[token("let")]
    TkKwLet((usize, usize)),
    #[token("def")]
    TkKwDef((usize, usize)),
    #[token("prop")]
    TkKwProp((usize, usize)),
    #[token("where")]
    TkKwWhere((usize, usize)),
    #[token("is")]
    TkKwIs((usize, usize)),
    #[token("do")]
    TkKwDo((usize, usize)),
    #[token("end")]
    TkKwEnd((usize, usize)),
    #[token("devprint")]
    TkKwPrint((usize, usize)),
    #[token("of")]
    TkKwOf((usize, usize)),
    #[token("fn")]
    TkKwFn((usize, usize)),
    #[token("mut")]
    TkKwMut((usize, usize)),
    #[token("if")]
    TkKwIf((usize, usize)),
    #[token("then")]
    TkKwThen((usize, usize)),
    #[token("else")]
    TkKwElse((usize, usize)),
    #[token("return")]
    TkKwReturn((usize, usize)),
    #[token("and")]
    TkKwAnd((usize, usize)),
    #[token("or")]
    TkKwOr((usize, usize)),
    #[token("not")]
    TkKwNot((usize, usize)),
    #[token("int")]
    TkKwInt((usize, usize)),
    #[token("flt")]
    TkKwFlt((usize, usize)),
    #[token("str")]
    TkKwStr((usize, usize)),
    #[token("Fn")]
    TkKwFnTy((usize, usize)),
    #[token("naught")]
    TkKwNaught((usize, usize)),
    #[token("bool")]
    TkKwBool((usize, usize)),
    #[token("type")]
    TkKwType((usize, usize)),
    #[token("struct")]
    TkKwStruct((usize, usize)),
    #[token("enum")]
    TkKwEnum((usize, usize)),
    #[regex("([a-zA-Z]|_)+[a-zA-Z0-9]*", priority=2)]
    TkSymbol((usize, usize)),
    //|true|false|\"[a-zA-Z0-9]*\"
    #[regex("[0-9]+", priority=4)]
    TkInt((usize, usize)),
    #[regex(r"-?(?:0|[1-9]\d*)(?:\.\d+)?(?:[eE][+-]?\d+)?", priority=2)]
    TkFlt((usize, usize)),
    #[regex("true", priority=3)]
    TkTrue((usize, usize)),
    #[regex("false", priority=3)]
    TkFalse((usize, usize)),
    #[regex(r#""([^"\\]|\\["\\bnfrt]|u[a-fA-F0-9]{4})*""#)]
    TkStrLit((usize, usize)),
    #[token("->")]
    TkArr((usize, usize)),
    #[token("!")]
    TkBang((usize, usize)),
    #[token("?")]
    TkQuestion((usize, usize)),
    #[token("&")]
    TkFuncComp((usize, usize)),
    #[token("+")]
    TkPlus((usize, usize)),
    #[token("-")]
    TkMinus((usize, usize)),
    #[token("*")]
    TkStar((usize, usize)),
    #[token("/")]
    TkSlash((usize, usize)),
    #[token("(")]
    TkLparen((usize, usize)),
    #[token(")")]
    TkRparen((usize, usize)),
    #[token("[")]
    TkLbracket((usize, usize)),
    #[token("]")]
    TkRbracket((usize, usize)),
    #[token("{")]
    TkLbrace((usize, usize)),
    #[token("}")]
    TkRbrace((usize, usize)),
    #[token("=")]
    TkAssign((usize, usize)),
    #[token("==")]
    TkCEQ((usize, usize)),
    #[token("<")]
    TkCLT((usize, usize)),
    #[token("<=")]
    TkCLE((usize, usize)),
    #[token(">")]
    TkCGT((usize, usize)),
    #[token(">=")]
    TkCGE((usize, usize)),
    #[token(",")]
    TkComma((usize, usize)),
    #[token(":")]
    TkColon((usize, usize)),
    #[token("::")]
    TkDoubleColon,
    #[token(".")]
    TkDot((usize, usize)),
    #[token("@")]
    TkAt((usize, usize)),
}

#[derive(Debug, Clone)]
pub struct Token {
    pub kind: Tk,
    pub lit: String,
    pub posline: usize,
    pub poscol: usize,
}

impl Token {
    pub fn new(kind: Tk, lit: String, pos: usize, col: usize) -> Self {
        Token {kind, lit, posline: pos, poscol: col}
    }
}