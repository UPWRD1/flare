use crate::core::resource::{
    ast::SymbolKind,
    lexemes::Lexeme,
    lexemes::LexemeKind::*,
    tokens::{Token, TokenKind, TokenKind::*},
};

use crate::create_token;

pub struct Analyzer {
    lexvec: Vec<Lexeme>,
    tkvec: Vec<Token>,
    loc: usize,
}
/*
macro_rules! ctk {
    ($kind:tt, $lexeme:expr, $literal:expr, $location:expr) => {
        Token {
            kind: $kind,
            lexeme: $lexeme,
            literal: $literal,
            location: $location,
        }
    };
}
*/

impl Analyzer {
    pub fn new(lxvec: Vec<Lexeme>) -> Self {
        Analyzer {
            lexvec: lxvec,
            loc: 0,
            tkvec: vec![],
        }
    }

    fn add(&mut self, t: Token) {
        self.tkvec.push(t);
    }

    pub fn analyze(&mut self) {
        //println!("{:?}", self.lexvec);
        self.extract_keywords();
        self.fix_types();
    }

    fn extract_keywords(&mut self) {
        while self.loc < self.lexvec.len() {
            let el = &self.lexvec[self.loc].clone();
            //println!("{:?}", el);

            match &el.kind {
                LxSymbol => {
                    let tkk = match &el.value {
                        SymbolKind::Identity(id, _) => match id.as_str() {
                            "val" => TokenKind::TkKWVal,
                            "op" => TokenKind::TkKWOp,
                            "print" => TokenKind::TkKwPrint,
                            "is" => TokenKind::TkKwIs,
                            "if" => TokenKind::TkKwIf,
                            "else" => TokenKind::TkKwElse,
                            "match" => TokenKind::TkKwMatch,
                            "while" => TokenKind::TkKwWhile,
                            "for" => TokenKind::TkKwFor,
                            "in" => TokenKind::TkKwIn,
                            "task" => TokenKind::TkKwTask,
                            "invoke" => TokenKind::TkKwInvoke,

                            "Int" => TokenKind::TkTyInt,
                            "Flt" => TokenKind::TkTyFlt,
                            "Str" => TokenKind::TkTyStr,
                            "Bool" => TokenKind::TkTyBool,
                            &_ => TokenKind::TkSymbol,
                        },
                        _ => panic!("How did you get here?"),
                    };
                    let toadd: Token = Token {
                        kind: tkk,
                        literal: el.value.clone(),
                        location: el.location,
                    };
                    self.add(toadd)
                }
                LxLiteral => {
                    match &el.value {
                        SymbolKind::Str(_) => {
                            self.add(Token {
                                kind: TokenKind::TkLiteral,
                                literal: el.value.clone(),
                                location: el.location,
                            });
                        }
                        _ => panic!("How did you get here? Who are you? What do you want?!"),
                    };
                }
                LxNumeric => match el.value {
                    SymbolKind::Int(_) => {
                        self.add(Token {
                            kind: TokenKind::TkNumeric,
                            literal: el.value.clone(),
                            location: el.location,
                        });
                    }
                    SymbolKind::Float(_) => {
                        self.add(Token {
                            kind: TokenKind::TkNumeric,
                            literal: el.value.clone(),
                            location: el.location,
                        });
                    }
                    _ => panic!("How did you get here? Who are you? What do you want?!"),
                },
                LxPlus => self.add(create_token!(el, TkPlus)),
                LxMinus => self.add(create_token!(el, TkMinus)),
                LxStar => self.add(create_token!(el, TkStar)),
                LxSlash => self.add(create_token!(el, TkSlash)),
                LxLparen => self.add(create_token!(el, TkLparen)),
                LxRparen => self.add(create_token!(el, TkRparen)),
                LxSmallArr => self.add(create_token!(el, TkSmallArr)),
                LxBigArr => self.add(create_token!(el, TkBigArr)),
                LxPipe => self.add(create_token!(el, TkPipe)),
                LxPercent => self.add(create_token!(el, TkPercent)),
                LxDoubleDot => self.add(create_token!(el, TkTyMute)),
                LxLBrace => self.add(create_token!(el, TkLBrace)),
                LxRBrace => self.add(create_token!(el, TkRBrace)),
                LxStatementEnd => {
                    if self.loc != 0 && self.lexvec[self.loc - 1].kind != LxLBrace {
                        self.add(create_token!(el, TkStatementEnd))
                    }
                }
                LxEqual => self.add(create_token!(el, TkEqual)),
                LxCEQ => self.add(create_token!(el, TkCEQ)),
                LxCNE => self.add(create_token!(el, TkCNE)),
                LxCLT => self.add(create_token!(el, TkCLT)),
                LxCLE => self.add(create_token!(el, TkCLE)),
                LxCGT => self.add(create_token!(el, TkCGT)),
                LxCGE => self.add(create_token!(el, TkCGE)),
                LxAnd => self.add(create_token!(el, TkAnd)),
                LxOr => self.add(create_token!(el, TkOr)),
                LxComma => self.add(create_token!(el, TkComma)),
                LxColon => self.add(create_token!(el, TkColon)),
                LxDot => self.add(create_token!(el, TkDot)),
                Err => {
                    panic!("uh oh")
                }
                Eof => self.add(create_token!(el, TEof)),
            }

            self.loc += 1;
        }
    }

    fn fix_types(&mut self) {
        self.loc = 0;
        while self.loc < self.tkvec.len() {
            let el = &mut self.tkvec[self.loc].clone();
            //println!("{:?}", el);

            let new_literal: SymbolKind = match &el.literal {
                SymbolKind::Identity(_, _) => match el.kind {
                    TkTyInt => SymbolKind::TyInt,
                    TkTyFlt => SymbolKind::TyFlt,
                    TkTyStr => SymbolKind::TyStr,
                    TkTyMute => SymbolKind::TyMute,
                    _ => el.literal.clone(),
                },
                _ => el.literal.clone(),
            };
            self.tkvec[self.loc] = Token {
                kind: el.kind.clone(),
                literal: new_literal,
                location: self.loc,
            };
            self.loc += 1;
        }
    }
    pub fn supply(&mut self) -> Vec<Token> {
        self.tkvec.clone()
    }
}
