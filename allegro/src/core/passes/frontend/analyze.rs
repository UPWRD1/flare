use crate::core::resource::{
    ast::SymbolValue,
    environment::AKind,
    lexemes::{Lexeme, LexemeKind::*},
    tokens::{
        Token,
        TokenType::{self, *},
    },
};

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
        //self.fix_types();
    }

    fn extract_keywords(&mut self) {
        while self.loc < self.lexvec.len() {
            let el = &self.lexvec[self.loc].clone();
            //println!("{:?}", el);

            match &el.kind {
                LxSymbol(sv) => {
                    let tkk = match sv.clone() {
                        SymbolValue::Identity(id) => match id.name.as_str() {
                            "let" => TokenType::TkKwLet,
                            "op" => TokenType::TkKWOp,
                            "print" => TokenType::TkKwPrint,
                            "of" => TokenType::TkKwOf,
                            "if" => TokenType::TkKwIf,
                            "else" => TokenType::TkKwElse,
                            "match" => TokenType::TkKwMatch,
                            "while" => TokenType::TkKwWhile,
                            "for" => TokenType::TkKwFor,
                            "in" => TokenType::TkKwIn,
                            "task" => TokenType::TkKwTask,
                            "invoke" => TokenType::TkKwInvoke,
                            "return" => TokenType::TkKwReturn,

                            "Int" => TokenType::TkType(AKind::TyInt),
                            "Flt" => TokenType::TkType(AKind::TyFlt),
                            "Str" => TokenType::TkType(AKind::TyStr),
                            "Bool" => TokenType::TkType(AKind::TyBool),

                            &_ => TokenType::TkSymbol,
                        },

                        _ => panic!("How did you get here?"),
                    };
                    let toadd: Token;
                    if tkk == TokenType::TkSymbol {
                        toadd = Token {
                            tokentype: tkk,
                            value: Some(sv.clone()),
                            location: el.location,
                        };
                    } else if tkk == TokenType::TkType(AKind::TyInt)
                        || tkk == TokenType::TkType(AKind::TyFlt)
                        || tkk == TokenType::TkType(AKind::TyStr)
                        || tkk == TokenType::TkType(AKind::TyBool)
                    {
                        toadd = Token {
                            tokentype: tkk,
                            value: Some(sv.clone()),
                            location: el.location,
                        };
                    } else {
                        toadd = Token {
                            tokentype: tkk,
                            value: None,
                            location: el.location,
                        };
                    }
                    self.add(toadd)
                }
                LxScalar(s) => {
                    self.add(Token {
                        tokentype: TokenType::TkScalar,
                        value: Some(SymbolValue::Scalar(s.clone())),
                        //kind: Some(AKind::TyStr),
                        location: el.location,
                    });
                }

                LxPlus => self.add(Token {
                    tokentype: TkPlus,
                    value: None,
                    location: el.location,
                }),
                LxMinus => self.add(Token {
                    tokentype: TkMinus,
                    value: None,
                    location: el.location,
                }),
                LxStar => self.add(Token {
                    tokentype: TkStar,
                    value: None,
                    location: el.location,
                }),
                LxSlash => self.add(Token {
                    tokentype: TkSlash,
                    value: None,
                    location: el.location,
                }),
                LxLparen => self.add(Token {
                    tokentype: TkLparen,
                    value: None,
                    location: el.location,
                }),
                LxRparen => self.add(Token {
                    tokentype: TkRparen,
                    value: None,
                    location: el.location,
                }),
                LxSmallArr => self.add(Token {
                    tokentype: TkSmallArr,
                    value: None,
                    location: el.location,
                }),
                LxBigArr => self.add(Token {
                    tokentype: TkBigArr,
                    value: None,
                    location: el.location,
                }),
                LxPipe => self.add(Token {
                    tokentype: TkPipe,
                    value: None,
                    location: el.location,
                }),
                LxPercent => self.add(Token {
                    tokentype: TkPercent,
                    value: None,
                    location: el.location,
                }),
                LxDoubleDot => {
                    let toadd: Token = Token {
                        tokentype: TkType(AKind::TyMute),
                        value: Some(SymbolValue::Mute),
                        location: el.location.clone(),
                    };
                    self.add(toadd)
                }
                LxLBrace => self.add(Token {
                    tokentype: TkLBrace,
                    value: None,
                    location: el.location,
                }),
                LxRBrace => self.add(Token {
                    tokentype: TkRBrace,
                    value: None,
                    location: el.location,
                }),
                LxStatementEnd => {
                    if self.loc != 0 && self.lexvec[self.loc - 1].kind != LxLBrace {
                        self.add(Token {
                            tokentype: TkStatementEnd,
                            value: None,
                            location: el.location,
                        })
                    }
                }
                LxEqual => self.add(Token {
                    tokentype: TkAssign,
                    value: None,
                    location: el.location,
                }),
                LxAssignInfer => self.add(Token {
                    tokentype: TkAssignInfer,
                    value: None,
                    location: el.location,
                }),
                LxCEQ => self.add(Token {
                    tokentype: TkCEQ,
                    value: None,
                    location: el.location,
                }),
                LxCNE => self.add(Token {
                    tokentype: TkCNE,
                    value: None,
                    location: el.location,
                }),
                LxCLT => self.add(Token {
                    tokentype: TkCLT,
                    value: None,
                    location: el.location,
                }),
                LxCLE => self.add(Token {
                    tokentype: TkCLE,
                    value: None,
                    location: el.location,
                }),
                LxCGT => self.add(Token {
                    tokentype: TkCGT,
                    value: None,
                    location: el.location,
                }),
                LxCGE => self.add(Token {
                    tokentype: TkCGE,
                    value: None,
                    location: el.location,
                }),
                LxAnd => self.add(Token {
                    tokentype: TkAnd,
                    value: None,
                    location: el.location,
                }),
                LxOr => self.add(Token {
                    tokentype: TkOr,
                    value: None,
                    location: el.location,
                }),
                LxComma => self.add(Token {
                    tokentype: TkComma,
                    value: None,
                    location: el.location,
                }),
                LxColon => self.add(Token {
                    tokentype: TkColon,
                    value: None,
                    location: el.location,
                }),
                LxDot => self.add(Token {
                    tokentype: TkDot,
                    value: None,
                    location: el.location,
                }),
                Err => {
                    panic!("uh oh")
                }
                Eof => self.add(Token {
                    tokentype: TokenType::TkEof,
                    value: None,
                    location: el.location,
                }),
            }

            self.loc += 1;
        }
    }

    pub fn supply(&mut self) -> Vec<Token> {
        self.tkvec.clone()
    }
}
