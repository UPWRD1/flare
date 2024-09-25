use logos::Logos;

///Enum for type of token
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Logos)]
#[logos(skip r"[\t\f\n]+")] // Ignore this regex pattern between tokens
pub enum Tk {
    #[regex(" ", logos::skip)]
    TkSpace,
    #[regex(r";")]
    TkSemicolon,
    #[regex("--.*", logos::skip)]
    TkComment,
    #[regex(r"with")]
    TkKwWith,
    #[regex(r"let")]
    TkKwLet,
    #[regex(r"def")]
    TkKwDef,
    #[regex(r"prop")]
    TkKwProp,
    #[regex(r"where")]
    TkKwWhere,
    #[regex(r"is")]
    TkKwIs,
    #[regex(r"do")]
    TkKwDo,
    #[regex(r"end")]
    TkKwEnd,
    #[regex(r"for")]
    TkKwFor,
    #[regex(r"devprint")]
    TkKwPrint,
    #[regex(r"of")]
    TkKwOf,
    #[regex(r"fn")]
    TkKwFn,
    #[regex(r"mut")]
    TkKwMut,
    #[regex(r"if")]
    TkKwIf,
    #[regex(r"then")]
    TkKwThen,
    #[regex(r"else")]
    TkKwElse,
    #[regex(r"match")]
    TkKwMatch,
    #[regex(r"return")]
    TkKwReturn,
    #[regex(r"and")]
    TkKwAnd,
    #[regex(r"or")]
    TkKwOr,
    #[regex(r"not")]
    TkKwNot,
    // #[regex(r"self")]
    // TkKwSelf,
    #[regex(r"uint")]
    TkKwUint,
    #[regex(r"word")]
    TkKwWord,
    #[regex(r"byte")]
    TkKwByte,
    #[regex(r"int")]
    TkKwInt,
    #[regex(r"flt")]
    TkKwFlt,
    #[regex(r"str")]
    TkKwStr,
    #[regex(r"char")]
    TkKwChar,
    #[regex(r"Fn")]
    TkKwFnTy,
    #[regex(r"naught")]
    TkKwNaught,
    #[regex(r"bool")]
    TkKwBool,
    #[regex(r"type")]
    TkKwType,
    #[regex(r"struct")]
    TkKwStruct,
    #[regex(r"enum")]
    TkKwEnum,
    #[regex("([a-zA-Z]|_)+([a-zA-Z0-9]|_)*", priority=2)]
    TkSymbol,
    #[regex("[0-9]+", priority=4)]
    TkInt,
    #[regex(r"-?(?:0|[1-9]\d*)(?:\.\d+)?(?:[eE][+-]?\d+)?", priority=2)]
    TkFlt,
    #[regex("true", priority=3)]
    TkTrue,
    #[regex("false", priority=3)]
    TkFalse,
    #[regex(r#""([^"\\]|\\["\\bnfrt]|u[a-fA-F0-9]{4})*""#)]
    TkStrLit,
    #[regex(r"->")]
    TkArr,
    #[regex(r"!")]
    TkBang,
    #[regex(r"\^")]
    TkPtrArr,
    #[regex(r"\%")]
    TkPtrInit,
    #[regex(r"\?")]
    TkQuestion,
    #[regex(r"&")]
    TkFuncComp,
    #[regex(r"\+")]
    TkPlus,
    #[regex(r"-")]
    TkMinus,
    #[regex(r"\*")]
    TkStar,
    #[regex(r"/")]
    TkSlash,
    #[regex(r"\(")]
    TkLparen,
    #[regex(r"\)")]
    TkRparen,
    #[regex(r"\[")]
    TkLbracket,
    #[regex(r"]")]
    TkRbracket,
    #[regex(r"\{")]
    TkLbrace,
    #[regex(r"}")]
    TkRbrace,
    #[regex(r"=")]
    TkAssign,
    #[regex(r"==")]
    TkCEQ,
    #[regex(r"<")]
    TkCLT,
    #[regex(r"<=")]
    TkCLE,
    #[regex(r">")]
    TkCGT,
    #[regex(r">=")]
    TkCGE,
    #[regex(r"\,")]
    TkComma,
    #[regex(r":")]
    TkColon,
    #[regex(r"::")]
    TkDoubleColon,
    #[regex(r"\.")]
    TkDot,
    #[regex(r"@")]
    TkAt,
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