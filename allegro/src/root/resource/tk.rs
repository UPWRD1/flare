use std::fmt;

use logos::{Lexer, Logos, Skip};

/// Update the line count and the char index.
fn newline_callback(lex: &mut Lexer<Tk>) -> Skip {
    lex.extras.0 += 1;
    lex.extras.1 = lex.span().end;
    Skip
}

fn space_callback(lex: &mut Lexer<Tk>) -> Skip {
    lex.extras.0 += 1;
    Skip
}

fn tab_callback(lex: &mut Lexer<Tk>) -> Skip {
    lex.extras.0 += 4;
    Skip
}


/// Compute the line and column position for the current word.
fn word_callback(lex: &mut Lexer<Tk>) -> (usize, usize) {
    let line = lex.extras.0;
    let column = lex.span().start - lex.extras.1;

    (line, column)
}

///Enum for type of token
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Logos)]
#[logos(skip r"[\f]+")] // Ignore this regex pattern between tokens
#[logos(extras = (usize, usize))]
pub enum Tk {
    #[regex(" ", space_callback)]
    TkSpace,
    #[regex("\t", tab_callback)]
    TkTab,
    #[regex("\n", newline_callback)]
    TkNewline,
    #[regex(r";", word_callback)]
    TkSemicolon((usize, usize)),
    #[regex("--.*", newline_callback)]
    TkComment,
    #[regex(r"with", word_callback)]
    TkKwWith((usize, usize)),
    #[regex(r"let", word_callback)]
    TkKwLet((usize, usize)),
    #[regex(r"def", word_callback)]
    TkKwDef((usize, usize)),
    #[regex(r"prop", word_callback)]
    TkKwProp((usize, usize)),
    #[regex(r"where", word_callback)]
    TkKwWhere((usize, usize)),
    #[regex(r"is", word_callback)]
    TkKwIs((usize, usize)),
    #[regex(r"do", word_callback)]
    TkKwDo((usize, usize)),
    #[regex(r"end", word_callback)]
    TkKwEnd((usize, usize)),
    #[regex(r"for", word_callback)]
    TkKwFor((usize, usize)),
    #[regex(r"devprint", word_callback)]
    TkKwPrint((usize, usize)),
    #[regex(r"of", word_callback)]
    TkKwOf((usize, usize)),
    #[regex(r"fn", word_callback)]
    TkKwFn((usize, usize)),
    //#[regex(r"mut", word_callback)]
    //TkKwMut((usize, usize)),
    #[regex(r"if", word_callback)]
    TkKwIf((usize, usize)),
    #[regex(r"then", word_callback)]
    TkKwThen((usize, usize)),
    #[regex(r"else", word_callback)]
    TkKwElse((usize, usize)),
    #[regex(r"match", word_callback)]
    TkKwMatch((usize, usize)),
    #[regex(r"return", word_callback)]
    TkKwReturn((usize, usize)),
    #[regex(r"and", word_callback)]
    TkKwAnd((usize, usize)),
    #[regex(r"or", word_callback)]
    TkKwOr((usize, usize)),
    #[regex(r"not", word_callback)]
    TkKwNot((usize, usize)),
    // #[regex(r"self", word_callback)]
    // TkKwSelf,
    #[regex(r"uint", word_callback)]
    TkKwUint((usize, usize)),
    #[regex(r"word", word_callback)]
    TkKwWord((usize, usize)),
    #[regex(r"byte", word_callback)]
    TkKwByte((usize, usize)),
    #[regex(r"int", word_callback)]
    TkKwInt((usize, usize)),
    #[regex(r"flt", word_callback)]
    TkKwFlt((usize, usize)),
    #[regex(r"str", word_callback)]
    TkKwStr((usize, usize)),
    #[regex(r"char", word_callback)]
    TkKwChar((usize, usize)),
    #[regex(r"Fn", word_callback)]
    TkKwFnTy((usize, usize)),
    #[regex(r"naught", word_callback)]
    TkKwNaught((usize, usize)),
    #[regex(r"bool", word_callback)]
    TkKwBool((usize, usize)),
    #[regex(r"type", word_callback)]
    TkKwType((usize, usize)),
    #[regex(r"struct", word_callback)]
    TkKwStruct((usize, usize)),
    #[regex(r"enum", word_callback)]
    TkKwEnum((usize, usize)),
    #[regex("([a-zA-Z]|_)+([a-zA-Z0-9]|_)*", priority = 2, callback = word_callback)]
    TkSymbol((usize, usize)),
    #[regex("[0-9]+", priority = 4, callback = word_callback)]
    TkInt((usize, usize)),
    #[regex(r"-?(?:0|[1-9]\d*)(?:\.\d+)?(?:[eE][+-]?\d+)?", priority = 2, callback = word_callback)]
    TkFlt((usize, usize)),
    #[regex("true", priority = 3, callback = word_callback)]
    TkTrue((usize, usize)),
    #[regex("false", priority = 3, callback = word_callback)]
    TkFalse((usize, usize)),
    #[regex(r#""([^"\\]|\\["\\bnfrt]|u[a-fA-F0-9]{4})*""#, word_callback)]
    TkStrLit((usize, usize)),
    #[regex(r"->", word_callback)]
    TkArr((usize, usize)),
    #[regex(r"!", word_callback)]
    TkBang((usize, usize)),
    #[regex(r"\^", word_callback)]
    TkPtrArr((usize, usize)),
    #[regex(r"\%", word_callback)]
    TkPtrInit((usize, usize)),
    #[regex(r"\?", word_callback)]
    TkQuestion((usize, usize)),
    #[regex(r"&", word_callback)]
    TkFuncComp((usize, usize)),
    #[regex(r"\+", word_callback)]
    TkPlus((usize, usize)),
    #[regex(r"-", word_callback)]
    TkMinus((usize, usize)),
    #[regex(r"\*", word_callback)]
    TkStar((usize, usize)),
    #[regex(r"/", word_callback)]
    TkSlash((usize, usize)),
    #[regex(r"\(", word_callback)]
    TkLparen((usize, usize)),
    #[regex(r"\)", word_callback)]
    TkRparen((usize, usize)),
    #[regex(r"\[", word_callback)]
    TkLbracket((usize, usize)),
    #[regex(r"]", word_callback)]
    TkRbracket((usize, usize)),
    #[regex(r"\{", word_callback)]
    TkLbrace((usize, usize)),
    #[regex(r"}", word_callback)]
    TkRbrace((usize, usize)),
    #[regex(r"=", word_callback)]
    TkAssign((usize, usize)),
    #[regex(r"==", word_callback)]
    TkCEQ((usize, usize)),
    #[regex(r"<", word_callback)]
    TkCLT((usize, usize)),
    #[regex(r"<=", word_callback)]
    TkCLE((usize, usize)),
    #[regex(r">", word_callback)]
    TkCGT((usize, usize)),
    #[regex(r">=", word_callback)]
    TkCGE((usize, usize)),
    #[regex(r"\,", word_callback)]
    TkComma((usize, usize)),
    #[regex(r":", word_callback)]
    TkColon((usize, usize)),
    #[regex(r"::", word_callback)]
    TkDoubleColon((usize, usize)),
    #[regex(r"\.", word_callback)]
    TkDot((usize, usize)),
    #[regex(r"@", word_callback)]
    TkAt((usize, usize)),
}

#[derive(Debug, Clone)]
pub struct Token {
    pub kind: Tk,
    pub lit: String,
}

impl Token {
    pub fn new(kind: Tk, lit: String) -> Self {
        Token {
            kind,
            lit,
        }
    }
}

impl fmt::Display for Token {
    // This trait requires `fmt` with this exact signature.
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        // Write strictly the first element into the supplied output
        // stream: `f`. Returns `fmt::Result` which indicates whether the
        // operation succeeded or failed. Note that `write!` uses syntax which
        // is very similar to `println!`.
        let val = match self.kind {
            Tk::TkSpace => todo!(),
            Tk::TkTab => todo!(),
            Tk::TkNewline => todo!(),
            Tk::TkSemicolon(_) => todo!(),
            Tk::TkComment => todo!(),
            Tk::TkKwWith(_) => "with",
            Tk::TkKwLet(_) => "let",
            Tk::TkKwDef(_) => "def",
            Tk::TkKwProp(_) => "prop",
            Tk::TkKwWhere(_) => "where",
            Tk::TkKwIs(_) => "is",
            Tk::TkKwDo(_) => "do",
            Tk::TkKwEnd(_) => "end",
            Tk::TkKwFor(_) => "for",
            Tk::TkKwPrint(_) => "print",
            Tk::TkKwOf(_) => "of",
            Tk::TkKwFn(_) => "fn",
            Tk::TkKwIf(_) => "if",
            Tk::TkKwThen(_) => "then",
            Tk::TkKwElse(_) => todo!(),
            Tk::TkKwMatch(_) => todo!(),
            Tk::TkKwReturn(_) => todo!(),
            Tk::TkKwAnd(_) => todo!(),
            Tk::TkKwOr(_) => todo!(),
            Tk::TkKwNot(_) => todo!(),
            Tk::TkKwUint(_) => todo!(),
            Tk::TkKwWord(_) => todo!(),
            Tk::TkKwByte(_) => todo!(),
            Tk::TkKwInt(_) => todo!(),
            Tk::TkKwFlt(_) => todo!(),
            Tk::TkKwStr(_) => todo!(),
            Tk::TkKwChar(_) => todo!(),
            Tk::TkKwFnTy(_) => todo!(),
            Tk::TkKwNaught(_) => todo!(),
            Tk::TkKwBool(_) => todo!(),
            Tk::TkKwType(_) => todo!(),
            Tk::TkKwStruct(_) => todo!(),
            Tk::TkKwEnum(_) => todo!(),
            Tk::TkSymbol(_) => todo!(),
            Tk::TkInt(_) => todo!(),
            Tk::TkFlt(_) => todo!(),
            Tk::TkTrue(_) => todo!(),
            Tk::TkFalse(_) => todo!(),
            Tk::TkStrLit(_) => todo!(),
            Tk::TkArr(_) => todo!(),
            Tk::TkBang(_) => todo!(),
            Tk::TkPtrArr(_) => todo!(),
            Tk::TkPtrInit(_) => todo!(),
            Tk::TkQuestion(_) => todo!(),
            Tk::TkFuncComp(_) => todo!(),
            Tk::TkPlus(_) => todo!(),
            Tk::TkMinus(_) => todo!(),
            Tk::TkStar(_) => todo!(),
            Tk::TkSlash(_) => todo!(),
            Tk::TkLparen(_) => todo!(),
            Tk::TkRparen(_) => todo!(),
            Tk::TkLbracket(_) => todo!(),
            Tk::TkRbracket(_) => todo!(),
            Tk::TkLbrace(_) => todo!(),
            Tk::TkRbrace(_) => todo!(),
            Tk::TkAssign(_) => todo!(),
            Tk::TkCEQ(_) => todo!(),
            Tk::TkCLT(_) => todo!(),
            Tk::TkCLE(_) => todo!(),
            Tk::TkCGT(_) => todo!(),
            Tk::TkCGE(_) => todo!(),
            Tk::TkComma(_) => todo!(),
            Tk::TkColon(_) => todo!(),
            Tk::TkDoubleColon(_) => todo!(),
            Tk::TkDot(_) => todo!(),
            Tk::TkAt(_) => todo!(),
        };
        write!(f, "{}", val)
    }
}
