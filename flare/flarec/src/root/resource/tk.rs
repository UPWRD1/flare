// use std::fmt;
// use super::errors::LexingError;
// use logos::{Lexer, Logos, Skip, Span};

// /// Update the line count and the char index.
// fn newline_callback(lex: &mut Lexer<Tk>) -> Skip {
//     lex.extras.line += 1;
//     lex.extras.span = lex.span();
//     Skip
// }

// fn space_callback(lex: &mut Lexer<Tk>) -> Skip {
//     lex.extras.span = lex.span();
//     Skip
// }

// fn tab_callback(lex: &mut Lexer<Tk>) -> Skip {
//     lex.extras.span = lex.span();
//     Skip
// }


// /// Compute the line and column position for the current word.
// fn word_callback(lex: &mut Lexer<Tk>) -> PosInfo {
// let line = lex.extras.line;
// let span =  lex.span();
//     PosInfo {
//         line,
//         span
//     }
// }

// #[derive(Clone, Debug, PartialEq, Eq)]
// pub struct PosInfo {
//     line: usize,
//     span: Span,
// }

// impl Default for PosInfo {
//     fn default() -> Self {
//         Self { line: 1, span: Default::default() }
//     }
// }

// ///Enum for type of token
// #[derive(Debug, Clone, PartialEq, Eq, Logos)]
// #[logos(skip r"[\f]+")] // Ignore this regex pattern between tokens
// #[logos(extras = PosInfo)]
// #[logos(error = LexingError)]
// pub enum Tk {
//     #[regex(" ", space_callback)]
//     TkSpace,
//     #[regex("\t", tab_callback)]
//     TkTab,
//     #[regex("\n", newline_callback)]
//     TkNewline,
//     #[regex("--.*", newline_callback)]
//     TkComment,
//     #[regex(r"with", word_callback)]
//     TkKwWith(PosInfo),
//     #[regex(r"extern", word_callback)]
//     TkKwExtern(PosInfo),
//     #[regex(r"effect", word_callback)]
//     TkKwEffect(PosInfo),
//     #[regex(r"let", word_callback)]
//     TkKwLet(PosInfo),
//     #[regex(r"def", word_callback)]
//     TkKwDef(PosInfo),
//     #[regex(r"in", word_callback)]
//     TkKwIn(PosInfo),
//     #[regex(r"prop", word_callback)]
//     TkKwProp(PosInfo),
//     #[regex(r"where", word_callback)]
//     TkKwWhere(PosInfo),
//     #[regex(r"is", word_callback)]
//     TkKwIs(PosInfo),
//     #[regex(r"do", word_callback)]
//     TkKwDo(PosInfo),
//     #[regex(r"end", word_callback)]
//     TkKwEnd(PosInfo),
//     #[regex(r"for", word_callback)]
//     TkKwFor(PosInfo),
//     #[regex(r"devprint", word_callback)]
//     TkKwPrint(PosInfo),
//     #[regex(r"of", word_callback)]
//     TkKwOf(PosInfo),
//     #[regex(r"fn", word_callback)]
//     TkKwFn(PosInfo),
//     //#[regex(r"mut", word_callback)]
//     //TkKwMut(PosInfo),
//     #[regex(r"if", word_callback)]
//     TkKwIf(PosInfo),
//     #[regex(r"then", word_callback)]
//     TkKwThen(PosInfo),
//     #[regex(r"else", word_callback)]
//     TkKwElse(PosInfo),
//     #[regex(r"match", word_callback)]
//     TkKwMatch(PosInfo),
//     #[regex(r"and", word_callback)]
//     TkKwAnd(PosInfo),
//     #[regex(r"or", word_callback)]
//     TkKwOr(PosInfo),
//     #[regex(r"not", word_callback)]
//     TkKwNot(PosInfo),
//     #[regex(r"self", word_callback)]
//     TkKwSelf(PosInfo),
//     #[regex(r"usize", word_callback)]
//     TkKwUsize(PosInfo),
//     #[regex(r"word", word_callback)]
//     TkKwWord(PosInfo),
//     #[regex(r"byte", word_callback)]
//     TkKwByte(PosInfo),
//     #[regex(r"int", word_callback)]
//     TkKwInt(PosInfo),
//     #[regex(r"flt", word_callback)]
//     TkKwFlt(PosInfo),
//     #[regex(r"str", word_callback)]
//     TkKwStr(PosInfo),
//     #[regex(r"char", word_callback)]
//     TkKwChar(PosInfo),
//     #[regex(r"Fn", word_callback)]
//     TkKwFnTy(PosInfo),
//     #[regex(r"unit", word_callback)]
//     TkKwUnit(PosInfo),
//     #[regex(r"bool", word_callback)]
//     TkKwBool(PosInfo),
//     #[regex(r"type", word_callback)]
//     TkKwType(PosInfo),
//     #[regex(r"struct", word_callback)]
//     TkKwStruct(PosInfo),
//     #[regex(r"enum", word_callback)]
//     TkKwEnum(PosInfo),
//     #[regex("([a-zA-Z]|_)+([a-zA-Z0-9]|_)*", priority = 2, callback = word_callback)]
//     TkSymbol(PosInfo),
//     #[regex("[0-9]+", priority = 4, callback = word_callback)]
//     TkInt(PosInfo),
//     #[regex(r"-?(?:0|[1-9]\d*)(?:\.\d+)?(?:[eE][+-]?\d+)?", priority = 2, callback = word_callback)]
//     TkFlt(PosInfo),
//     #[regex("true", priority = 3, callback = word_callback)]
//     TkTrue(PosInfo),
//     #[regex("false", priority = 3, callback = word_callback)]
//     TkFalse(PosInfo),
//     #[regex(r#""([^"\\]|\\["\\bnfrt]|u[a-fA-F0-9]{4})*""#, word_callback)]
//     TkStrLit(PosInfo),
//     #[regex(r"->", word_callback)]
//     TkArr(PosInfo),
//     #[regex(r"!", word_callback)]
//     TkBang(PosInfo),
//     #[token(r"^", word_callback)]
//     TkPtrArr(PosInfo),
//     #[token(r"%", word_callback)]
//     TkPtrInit(PosInfo),
//     #[regex(r"\?", word_callback)]
//     TkQuestion(PosInfo),
//     #[regex(r"&", word_callback)]
//     TkFuncComp(PosInfo),
//     #[regex(r"\+", word_callback)]
//     TkPlus(PosInfo),
//     #[regex(r"-", word_callback)]
//     TkMinus(PosInfo),
//     #[regex(r"\*", word_callback)]
//     TkStar(PosInfo),
//     #[regex(r"/", word_callback)]
//     TkSlash(PosInfo),
//     #[regex(r"\(", word_callback)]
//     TkLparen(PosInfo),
//     #[regex(r"\)", word_callback)]
//     TkRparen(PosInfo),
//     #[regex(r"\[", word_callback)]
//     TkLbracket(PosInfo),
//     #[regex(r"]", word_callback)]
//     TkRbracket(PosInfo),
//     #[regex(r"\{", word_callback)]
//     TkLbrace(PosInfo),
//     #[regex(r"}", word_callback)]
//     TkRbrace(PosInfo),
//     #[regex(r"=", word_callback)]
//     TkAssign(PosInfo),
//     #[regex(r"==", word_callback)]
//     TkCEQ(PosInfo),
//     #[regex(r"<", word_callback)]
//     TkCLT(PosInfo),
//     #[regex(r"<=", word_callback)]
//     TkCLE(PosInfo),
//     #[regex(r">", word_callback)]
//     TkCGT(PosInfo),
//     #[regex(r">=", word_callback)]
//     TkCGE(PosInfo),
//     #[regex(r"\,", word_callback)]
//     TkComma(PosInfo),
//     #[regex(r":", word_callback)]
//     TkColon(PosInfo),
//     #[regex(r"::", word_callback)]
//     TkDoubleColon(PosInfo),
//     #[token("...", word_callback)]
//     TkTripleDot(PosInfo),
//     #[regex(r"\.", word_callback)]
//     TkDot(PosInfo),
//     #[regex(r"@", word_callback)]
//     TkAt(PosInfo),
//     #[token("|", word_callback)]
//     TkPipe(PosInfo),

// }

// #[derive(Debug, Clone)]
// pub struct Token {
//     pub kind: Tk,
//     pub lit: String,
// }

// impl Token {
//     pub fn new(kind: Tk, lit: String) -> Self {
//         Token {
//             kind,
//             lit,
//         }
//     }

//     pub fn get_span(&self) -> Span {
//         match self.kind.clone() {
//             Tk::TkSpace => todo!(),
//             Tk::TkTab => todo!(),
//             Tk::TkNewline => todo!(),
//             Tk::TkComment => todo!(),
//             Tk::TkKwWith(v) => v.span,
//             Tk::TkKwExtern(v) => v.span,
//             Tk::TkKwEffect(v) => v.span,
//             Tk::TkKwSelf(v) => v.span,

//             Tk::TkKwLet(v) => v.span,
//             Tk::TkKwDef(v) => v.span,
//             Tk::TkKwIn(v) => v.span,
//             Tk::TkKwProp(v) => v.span,
//             Tk::TkKwWhere(v) => v.span,
//             Tk::TkKwIs(v) => v.span,
//             Tk::TkKwDo(v) => v.span,
//             Tk::TkKwEnd(v) => v.span,
//             Tk::TkKwFor(v) => v.span,
//             Tk::TkKwPrint(v) => v.span,
//             Tk::TkKwOf(v) => v.span,
//             Tk::TkKwFn(v) => v.span,
//             Tk::TkKwIf(v) => v.span,
//             Tk::TkKwThen(v) => v.span,
//             Tk::TkKwElse(v) => v.span,
//             Tk::TkKwMatch(v) => v.span,
//             Tk::TkKwAnd(v) => v.span,
//             Tk::TkKwOr(v) => v.span,
//             Tk::TkKwNot(v) => v.span,
//             Tk::TkKwUsize(v) => v.span,
//             Tk::TkKwWord(v) => v.span,
//             Tk::TkKwByte(v) => v.span,
//             Tk::TkKwInt(v) => v.span,
//             Tk::TkKwFlt(v) => v.span,
//             Tk::TkKwStr(v) => v.span,
//             Tk::TkKwChar(v) => v.span,
//             Tk::TkKwFnTy(v) => v.span,
//             Tk::TkKwUnit(v) => v.span,
//             Tk::TkKwBool(v) => v.span,
//             Tk::TkKwType(v) => v.span,
//             Tk::TkKwStruct(v) => v.span,
//             Tk::TkKwEnum(v) => v.span,
//             Tk::TkSymbol(v) => v.span,
//             Tk::TkInt(v) => v.span,
//             Tk::TkFlt(v) => v.span,
//             Tk::TkTrue(v) => v.span,
//             Tk::TkFalse(v) => v.span,
//             Tk::TkStrLit(v) => v.span,
//             Tk::TkArr(v) => v.span,
//             Tk::TkBang(v) => v.span,
//             Tk::TkPtrArr(v) => v.span,
//             Tk::TkPtrInit(v) => v.span,
//             Tk::TkQuestion(v) => v.span,
//             Tk::TkFuncComp(v) => v.span,
//             Tk::TkPlus(v) => v.span,
//             Tk::TkMinus(v) => v.span,
//             Tk::TkStar(v) => v.span,
//             Tk::TkSlash(v) => v.span,
//             Tk::TkLparen(v) => v.span,
//             Tk::TkRparen(v) => v.span,
//             Tk::TkLbracket(v) => v.span,
//             Tk::TkRbracket(v) => v.span,
//             Tk::TkLbrace(v) => v.span,
//             Tk::TkRbrace(v) => v.span,
//             Tk::TkAssign(v) => v.span,
//             Tk::TkCEQ(v) => v.span,
//             Tk::TkCLT(v) => v.span,
//             Tk::TkCLE(v) => v.span,
//             Tk::TkCGT(v) => v.span,
//             Tk::TkCGE(v) => v.span,
//             Tk::TkComma(v) => v.span,
//             Tk::TkColon(v) => v.span,
//             Tk::TkDoubleColon(v) => v.span,
//             Tk::TkDot(v) => v.span,
//             Tk::TkTripleDot(v) => v.span,
//             Tk::TkAt(v) => v.span,
//             Tk::TkPipe(v) => v.span,

//         }
//     }

//     pub fn get_line(&self) -> usize {
//         match self.kind.clone() {
//             Tk::TkSpace => todo!(),
//             Tk::TkTab => todo!(),
//             Tk::TkNewline => todo!(),
//             Tk::TkComment => todo!(),
//             Tk::TkKwWith(v) => v.line,
//             Tk::TkKwExtern(v) => v.line,
//             Tk::TkKwEffect(v) => v.line,
//             Tk::TkKwSelf(v) => v.line,

//             Tk::TkKwLet(v) => v.line,
//             Tk::TkKwDef(v) => v.line,

//             Tk::TkKwIn(v) => v.line,
//             Tk::TkKwProp(v) => v.line,
//             Tk::TkKwWhere(v) => v.line,
//             Tk::TkKwIs(v) => v.line,
//             Tk::TkKwDo(v) => v.line,
//             Tk::TkKwEnd(v) => v.line,
//             Tk::TkKwFor(v) => v.line,
//             Tk::TkKwPrint(v) => v.line,
//             Tk::TkKwOf(v) => v.line,
//             Tk::TkKwFn(v) => v.line,
//             Tk::TkKwIf(v) => v.line,
//             Tk::TkKwThen(v) => v.line,
//             Tk::TkKwElse(v) => v.line,
//             Tk::TkKwMatch(v) => v.line,
//             Tk::TkKwAnd(v) => v.line,
//             Tk::TkKwOr(v) => v.line,
//             Tk::TkKwNot(v) => v.line,
//             Tk::TkKwUsize(v) => v.line,
//             Tk::TkKwWord(v) => v.line,
//             Tk::TkKwByte(v) => v.line,
//             Tk::TkKwInt(v) => v.line,
//             Tk::TkKwFlt(v) => v.line,
//             Tk::TkKwStr(v) => v.line,
//             Tk::TkKwChar(v) => v.line,
//             Tk::TkKwFnTy(v) => v.line,
//             Tk::TkKwUnit(v) => v.line,
//             Tk::TkKwBool(v) => v.line,
//             Tk::TkKwType(v) => v.line,
//             Tk::TkKwStruct(v) => v.line,
//             Tk::TkKwEnum(v) => v.line,
//             Tk::TkSymbol(v) => v.line,
//             Tk::TkInt(v) => v.line,
//             Tk::TkFlt(v) => v.line,
//             Tk::TkTrue(v) => v.line,
//             Tk::TkFalse(v) => v.line,
//             Tk::TkStrLit(v) => v.line,
//             Tk::TkArr(v) => v.line,
//             Tk::TkBang(v) => v.line,
//             Tk::TkPtrArr(v) => v.line,
//             Tk::TkPtrInit(v) => v.line,
//             Tk::TkQuestion(v) => v.line,
//             Tk::TkFuncComp(v) => v.line,
//             Tk::TkPlus(v) => v.line,
//             Tk::TkMinus(v) => v.line,
//             Tk::TkStar(v) => v.line,
//             Tk::TkSlash(v) => v.line,
//             Tk::TkLparen(v) => v.line,
//             Tk::TkRparen(v) => v.line,
//             Tk::TkLbracket(v) => v.line,
//             Tk::TkRbracket(v) => v.line,
//             Tk::TkLbrace(v) => v.line,
//             Tk::TkRbrace(v) => v.line,
//             Tk::TkAssign(v) => v.line,
//             Tk::TkCEQ(v) => v.line,
//             Tk::TkCLT(v) => v.line,
//             Tk::TkCLE(v) => v.line,
//             Tk::TkCGT(v) => v.line,
//             Tk::TkCGE(v) => v.line,
//             Tk::TkComma(v) => v.line,
//             Tk::TkColon(v) => v.line,
//             Tk::TkDoubleColon(v) => v.line,
//             Tk::TkDot(v) => v.line,
//             Tk::TkTripleDot(v) => v.line,
//             Tk::TkAt(v) => v.line,
//             Tk::TkPipe(v) => v.line,

//         }
//     }
// }

// impl fmt::Display for Token {
//     // This trait requires `fmt` with this exact signature.
//     fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
//         // Write strictly the first element into the supplied output
//         // stream: `f`. Returns `fmt::Result` which indicates whether the
//         // operation succeeded or failed. Note that `write!` uses syntax which
//         // is very similar to `println!`.
//         let val = match self.kind {
//             Tk::TkSpace => todo!(),
//             Tk::TkTab => todo!(),
//             Tk::TkNewline => todo!(),
//             Tk::TkComment => todo!(),
//             Tk::TkKwWith(_) => "with",
//             Tk::TkKwExtern(_) => "extern",
//             Tk::TkKwEffect(_) => "effect",
//             Tk::TkKwSelf(_) => "self",

//             Tk::TkKwLet(_) => "let",
//             Tk::TkKwDef(_) => "def",

//             Tk::TkKwIn(_) => "in",
//             Tk::TkKwProp(_) => "prop",
//             Tk::TkKwWhere(_) => "where",
//             Tk::TkKwIs(_) => "is",
//             Tk::TkKwDo(_) => "do",
//             Tk::TkKwEnd(_) => "end",
//             Tk::TkKwFor(_) => "for",
//             Tk::TkKwPrint(_) => "print",
//             Tk::TkKwOf(_) => "of",
//             Tk::TkKwFn(_) => "fn",
//             Tk::TkKwIf(_) => "if",
//             Tk::TkKwThen(_) => "then",
//             Tk::TkKwElse(_) => todo!(),
//             Tk::TkKwMatch(_) => todo!(),
//             Tk::TkKwAnd(_) => todo!(),
//             Tk::TkKwOr(_) => todo!(),
//             Tk::TkKwNot(_) => todo!(),
//             Tk::TkKwUsize(_) => todo!(),
//             Tk::TkKwWord(_) => todo!(),
//             Tk::TkKwByte(_) => todo!(),
//             Tk::TkKwInt(_) => todo!(),
//             Tk::TkKwFlt(_) => todo!(),
//             Tk::TkKwStr(_) => todo!(),
//             Tk::TkKwChar(_) => todo!(),
//             Tk::TkKwFnTy(_) => todo!(),
//             Tk::TkKwUnit(_) => todo!(),
//             Tk::TkKwBool(_) => todo!(),
//             Tk::TkKwType(_) => todo!(),
//             Tk::TkKwStruct(_) => todo!(),
//             Tk::TkKwEnum(_) => todo!(),
//             Tk::TkSymbol(_) => todo!(),
//             Tk::TkInt(_) => todo!(),
//             Tk::TkFlt(_) => todo!(),
//             Tk::TkTrue(_) => todo!(),
//             Tk::TkFalse(_) => todo!(),
//             Tk::TkStrLit(_) => todo!(),
//             Tk::TkArr(_) => todo!(),
//             Tk::TkBang(_) => todo!(),
//             Tk::TkPtrArr(_) => todo!(),
//             Tk::TkPtrInit(_) => todo!(),
//             Tk::TkQuestion(_) => todo!(),
//             Tk::TkFuncComp(_) => todo!(),
//             Tk::TkPlus(_) => todo!(),
//             Tk::TkMinus(_) => todo!(),
//             Tk::TkStar(_) => todo!(),
//             Tk::TkSlash(_) => todo!(),
//             Tk::TkLparen(_) => todo!(),
//             Tk::TkRparen(_) => todo!(),
//             Tk::TkLbracket(_) => todo!(),
//             Tk::TkRbracket(_) => todo!(),
//             Tk::TkLbrace(_) => todo!(),
//             Tk::TkRbrace(_) => todo!(),
//             Tk::TkAssign(_) => todo!(),
//             Tk::TkCEQ(_) => todo!(),
//             Tk::TkCLT(_) => todo!(),
//             Tk::TkCLE(_) => todo!(),
//             Tk::TkCGT(_) => todo!(),
//             Tk::TkCGE(_) => todo!(),
//             Tk::TkComma(_) => todo!(),
//             Tk::TkColon(_) => todo!(),
//             Tk::TkDoubleColon(_) => todo!(),
//             Tk::TkDot(_) => todo!(),
//             Tk::TkTripleDot(_) => todo!(),
//             Tk::TkAt(_) => todo!(),
//             Tk::TkPipe(_) => todo!(),

//         };
//         write!(f, "{}", val)
//     }
// }
