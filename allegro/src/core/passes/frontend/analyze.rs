use crate::core::resource::{
    ast::{self, Scalar, SymbolValue},
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
    lx_loc: usize,
    tk_loc: usize,
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
            lx_loc: 0,
            tk_loc: 0,
            tkvec: vec![],
        }
    }

    fn add(&mut self, t: Token) {
        self.tkvec.push(t);
    }

    fn next_t(&mut self) -> Token {
        self.tkvec[self.tk_loc + 1].clone()
    }

    pub fn analyze(&mut self) {
        //println!("{:?}", self.lexvec);
        self.extract_keywords();
        self.import_pass();
        //self.fix_types();
    }

    fn extract_keywords(&mut self) {
        while self.lx_loc < self.lexvec.len() {
            let el = &self.lexvec[self.lx_loc].clone();
            //println!("{:?}", el);

            match &el.kind {
                LxSymbol(sv) => {
                    let tkk = match sv.clone() {
                        SymbolValue::Pair(id) => match id.name.as_str() {
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

                            "int" => TokenType::TkType(AKind::TyInt),
                            "flt" => TokenType::TkType(AKind::TyFlt),
                            "str" => TokenType::TkType(AKind::TyStr),
                            "bool" => TokenType::TkType(AKind::TyBool),

                            "use" => TokenType::TkKwUse,
                            "true" => TokenType::TkScalar,
                            "false" => TokenType::TkScalar,

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
                            //lit: sv.clone().get_string().unwrap()
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
                            //lit: format!("{} ", sv.clone().get_string().unwrap())
                        };
                    } else if tkk == TokenType::TkScalar {
                        if <ast::SymbolValue as Clone>::clone(sv).get_string().unwrap() == "true" {
                            toadd = Token {
                                tokentype: tkk,
                                value: Some(SymbolValue::Scalar(Scalar::Bool(true))),
                                location: el.location,
                                //lit: format!("{} ", sv.clone().get_string().unwrap())
                            }
                        } else {
                            toadd = Token {
                                tokentype: tkk,
                                value: Some(SymbolValue::Scalar(Scalar::Bool(false))),
                                location: el.location,
                                //lit: format!("{} ", sv.clone().get_string().unwrap()),
                            }
                        }
                    } else {
                        toadd = Token {
                            tokentype: tkk,
                            value: None,
                            location: el.location,
                            //lit: format!("{} ", sv.clone().get_string().unwrap()),
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
                        //lit: SymbolValue::Scalar(s.clone()).get_string().unwrap(),
                    });
                }

                LxPlus => self.add(Token {
                    tokentype: TkPlus,
                    value: None,
                    location: el.location,
                    //lit: "+".to_string()
                }),
                LxMinus => self.add(Token {
                    tokentype: TkMinus,
                    value: None,
                    location: el.location,
                    //lit: "-".to_string()
                }),
                LxStar => self.add(Token {
                    tokentype: TkStar,
                    value: None,
                    location: el.location,
                    //lit: "*".to_string()

                }),
                LxSlash => self.add(Token {
                    tokentype: TkSlash,
                    value: None,
                    location: el.location,
                    //lit: "/".to_string()

                }),
                LxLparen => self.add(Token {
                    tokentype: TkLparen,
                    value: None,
                    location: el.location,
                    //lit: "(".to_string()

                }),
                LxRparen => self.add(Token {
                    tokentype: TkRparen,
                    value: None,
                    location: el.location,
                    //lit: ") ".to_string()

                }),
                LxSmallArr => self.add(Token {
                    tokentype: TkSmallArr,
                    value: None,
                    location: el.location,
                    //lit: "-> ".to_string()

                }),
                LxBigArr => self.add(Token {
                    tokentype: TkBigArr,
                    value: None,
                    location: el.location,
                    //lit: "=>".to_string()

                }),
                LxPipe => self.add(Token {
                    tokentype: TkPipe,
                    value: None,
                    location: el.location,
                    //lit: "|".to_string()

                }),
                LxPercent => self.add(Token {
                    tokentype: TkPercent,
                    value: None,
                    location: el.location,
                    //lit: "%".to_string()

                }),
                LxDoubleDot => {
                    let toadd: Token = Token {
                        tokentype: TkType(AKind::TyMute),
                        value: Some(SymbolValue::Mute),
                        location: el.location,
                        //lit: "..".to_string()

                    };
                    self.add(toadd)
                }
                LxLBrace => self.add(Token {
                    tokentype: TkLBrace,
                    value: None,
                    location: el.location,
                    //lit: "{".to_string()
                }),
                LxRBrace => self.add(Token {
                    tokentype: TkRBrace,
                    value: None,
                    location: el.location,
                    //lit: "}".to_string()

                }),
                LxStatementEnd => {
                    if self.lx_loc != 0 && self.lexvec[self.lx_loc - 1].kind != LxLBrace {
                        self.add(Token {
                            tokentype: TkStatementEnd,
                            value: None,
                            location: el.location,
                            //lit: "".to_string()

                        })
                    }
                }
                LxEqual => self.add(Token {
                    tokentype: TkAssign,
                    value: None,
                    location: el.location,
                    //lit: "=".to_string()

                }),
                LxAssignInfer => self.add(Token {
                    tokentype: TkAssignInfer,
                    value: None,
                    location: el.location,
                    //lit: ":=".to_string()

                }),
                LxOpMuteShorthand => self.add(Token {
                    tokentype: TkOpMuteShortHand,
                    value: None,
                    location: el.location,
                    //lit: ":->".to_string()

                }),
                LxCEQ => self.add(Token {
                    tokentype: TkCEQ,
                    value: None,
                    location: el.location,
                    //lit: "==".to_string()

                }),
                LxCNE => self.add(Token {
                    tokentype: TkCNE,
                    value: None,
                    location: el.location,
                    //lit: "!=".to_string()

                }),
                LxCLT => self.add(Token {
                    tokentype: TkCLT,
                    value: None,
                    location: el.location,
                    //lit: "<".to_string()

                }),
                LxCLE => self.add(Token {
                    tokentype: TkCLE,
                    value: None,
                    location: el.location,
                    //lit: "<=".to_string()

                }),
                LxCGT => self.add(Token {
                    tokentype: TkCGT,
                    value: None,
                    location: el.location,
                    //lit: ">".to_string()

                }),
                LxCGE => self.add(Token {
                    tokentype: TkCGE,
                    value: None,
                    location: el.location,
                    //lit: ">=".to_string()

                }),
                LxAnd => self.add(Token {
                    tokentype: TkAnd,
                    value: None,
                    location: el.location,
                    //lit: "&&".to_string()

                }),
                LxOr => self.add(Token {
                    tokentype: TkOr,
                    value: None,
                    location: el.location,
                    //lit: "||".to_string()

                }),
                LxComma => self.add(Token {
                    tokentype: TkComma,
                    value: None,
                    location: el.location,
                    //lit: ",".to_string()

                }),
                LxColon => self.add(Token {
                    tokentype: TkColon,
                    value: None,
                    location: el.location,
                    //lit: ": ".to_string()

                }),
                LxDot => self.add(Token {
                    tokentype: TkDot,
                    value: None,
                    location: el.location,
                    //lit: ".".to_string()

                }),
                Err => {
                    panic!("uh oh")
                }
                Eof => self.add(Token {
                    tokentype: TokenType::TkEof,
                    value: None,
                    location: el.location,
                    //lit: "".to_string()

                }),
            }

            self.lx_loc += 1;
        }
    }

    fn import_pass(&mut self) -> Vec<Token> {
        let tv: Vec<Token> = self.tkvec.clone();
        while self.tk_loc < tv.len() {
            match self.tkvec[self.tk_loc].tokentype {
                TkKwUse => {
                    let to_import = self.next_t().value.unwrap().get_string().unwrap();
                    let mut nfile = crate::core::compile_import(&to_import);
                    nfile.remove(nfile.len() - 1);
                    self.tkvec.remove(self.tkvec.len() - 1);

                    self.tkvec.append(&mut nfile);
                    println!("{:?}", self.tkvec.remove(self.tk_loc));
                    println!("{:?}", self.tkvec.remove(self.tk_loc));
                }
                _ => {
                    self.tk_loc += 1;
                }
            }
        }
        self.tkvec.clone()
    }

    pub fn supply(&mut self) -> Vec<Token> {
        self.tkvec.clone()
    }
}
