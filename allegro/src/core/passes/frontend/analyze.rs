use crate::core::resource::{
    ast::SymbolValue, environment::AKind, lexemes::Lexeme, lexemes::LexemeKind::*, tokens::{Token, TokenType, TokenType::*}
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
        //self.fix_types();
    }

    fn extract_keywords(&mut self) {
        while self.loc < self.lexvec.len() {
            let el = &self.lexvec[self.loc].clone();
            //println!("{:?}", el);

            match &el.kind {
                LxSymbol => {
                    let tkk = match el.value.clone() {
                        SymbolValue::Identity(id) => match id.name.unwrap().as_str() {
                            "val" => TokenType::TkKwVal,
                            "op" => TokenType::TkKWOp,
                            "print" => TokenType::TkKwPrint,
                            "is" => TokenType::TkKwIs,
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
                            }
                            
                        _ => panic!("How did you get here?"),
                    };
                    let toadd: Token;
                    if tkk == TokenType::TkSymbol {
                        toadd = Token {
                            tokentype: tkk,
                            value: el.value.clone(),
                            location: el.location,
                        };
                    } else if tkk == TokenType::TkType(AKind::TyInt) ||  tkk == TokenType::TkType(AKind::TyFlt) || tkk == TokenType::TkType(AKind::TyStr) || tkk == TokenType::TkType(AKind::TyBool){
                        toadd = Token {
                            tokentype: tkk,
                            value: el.value.clone(),
                            location: el.location,
                        };
                    } else {
                        toadd = Token {
                            tokentype: tkk,
                            value: SymbolValue::Nothing,
                            location: el.location,
                        };
                    }
                    self.add(toadd)
                }
                LxLiteral => {
                    match &el.value {
                        SymbolValue::Str(_) => {
                            self.add(Token {
                                tokentype: TokenType::TkLiteral,
                                value: el.value.clone(),
                                //kind: Some(AKind::TyStr),
                                location: el.location,
                            });
                        }
                        _ => panic!("How did you get here? Who are you? What do you want?!"),
                    };
                }
                LxNumeric => match el.value {
                    SymbolValue::Int(_) => {
                        self.add(Token {
                            tokentype: TokenType::TkNumeric,
                            value: el.value.clone(),
                            //kind: Some(AKind::TyInt),
                            location: el.location,
                        });
                    }
                    SymbolValue::Float(_) => {
                        self.add(Token {
                            tokentype: TokenType::TkNumeric,
                            value: el.value.clone(),

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
                LxDoubleDot => {
                    let toadd: Token = Token { tokentype: TkType(AKind::TyMute), value: el.value.clone(), location: el.location.clone() };
                    self.add(toadd)
                },
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
            
        }
    }
    pub fn supply(&mut self) -> Vec<Token> {
        self.tkvec.clone()
    }
}
