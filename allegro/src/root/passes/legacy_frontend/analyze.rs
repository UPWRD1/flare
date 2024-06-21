use crate::root::legacy_resource::{
    ast::{self, Scalar, SymbolValue},
    environment::AKind,
    lexemes::{Lexeme, LexemeKind::*},
    tokens::{
        LegacyToken,
        TokenType::{self, *},
    },
};

pub struct Analyzer {
    lexvec: Vec<Lexeme>,
    tkvec: Vec<LegacyToken>,
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

    fn add(&mut self, t: LegacyToken) {
        self.tkvec.push(t);
    }

    fn next_t(&mut self) -> LegacyToken {
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
                            "var" => TokenType::TkKwVar,
                            "let" => TokenType::TkKwLet,
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
                    let toadd: LegacyToken;
                    if tkk == TokenType::TkSymbol {
                        toadd = LegacyToken {
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
                        toadd = LegacyToken {
                            tokentype: tkk,
                            value: None, //Some(sv.clone()),
                            location: el.location,
                            //lit: format!("{} ", sv.clone().get_string().unwrap())
                        };
                    } else if tkk == TokenType::TkScalar {
                        if <ast::SymbolValue as Clone>::clone(sv).get_string().unwrap() == "true" {
                            toadd = LegacyToken {
                                tokentype: tkk,
                                value: Some(SymbolValue::Scalar(Scalar::Bool(true))),
                                location: el.location,
                                //lit: format!("{} ", sv.clone().get_string().unwrap())
                            }
                        } else {
                            toadd = LegacyToken {
                                tokentype: tkk,
                                value: Some(SymbolValue::Scalar(Scalar::Bool(false))),
                                location: el.location,
                                //lit: format!("{} ", sv.clone().get_string().unwrap()),
                            }
                        }
                    } else {
                        toadd = LegacyToken {
                            tokentype: tkk,
                            value: None,
                            location: el.location,
                            //lit: format!("{} ", sv.clone().get_string().unwrap()),
                        };
                    }
                    self.add(toadd)
                }
                LxScalar(s) => {
                    self.add(LegacyToken {
                        tokentype: TokenType::TkScalar,
                        value: Some(SymbolValue::Scalar(s.clone())),
                        //kind: Some(AKind::TyStr),
                        location: el.location,
                        //lit: SymbolValue::Scalar(s.clone()).get_string().unwrap(),
                    });
                }

                LxPlus => self.add(LegacyToken {
                    tokentype: TkPlus,
                    value: None,
                    location: el.location,
                    //lit: "+".to_string()
                }),
                LxMinus => self.add(LegacyToken {
                    tokentype: TkMinus,
                    value: None,
                    location: el.location,
                    //lit: "-".to_string()
                }),
                LxStar => self.add(LegacyToken {
                    tokentype: TkStar,
                    value: None,
                    location: el.location,
                    //lit: "*".to_string()

                }),
                LxSlash => self.add(LegacyToken {
                    tokentype: TkSlash,
                    value: None,
                    location: el.location,
                    //lit: "/".to_string()

                }),
                LxLparen => self.add(LegacyToken {
                    tokentype: TkLparen,
                    value: None,
                    location: el.location,
                    //lit: "(".to_string()

                }),
                LxRparen => self.add(LegacyToken {
                    tokentype: TkRparen,
                    value: None,
                    location: el.location,
                    //lit: ") ".to_string()

                }),
                LxSmallArr => self.add(LegacyToken {
                    tokentype: TkSmallArr,
                    value: None,
                    location: el.location,
                    //lit: "-> ".to_string()

                }),
                LxBigArr => self.add(LegacyToken {
                    tokentype: TkBigArr,
                    value: None,
                    location: el.location,
                    //lit: "=>".to_string()

                }),
                LxPipe => self.add(LegacyToken {
                    tokentype: TkPipe,
                    value: None,
                    location: el.location,
                    //lit: "|".to_string()

                }),
                LxPercent => self.add(LegacyToken {
                    tokentype: TkPercent,
                    value: None,
                    location: el.location,
                    //lit: "%".to_string()

                }),
                LxDoubleDot => {
                    let toadd: LegacyToken = LegacyToken {
                        tokentype: TkType(AKind::TyMute),
                        value: Some(SymbolValue::Mute),
                        location: el.location,
                        //lit: "..".to_string()

                    };
                    self.add(toadd)
                }
                LxLBrace => self.add(LegacyToken {
                    tokentype: TkLBrace,
                    value: None,
                    location: el.location,
                    //lit: "{".to_string()
                }),
                LxRBrace => self.add(LegacyToken {
                    tokentype: TkRBrace,
                    value: None,
                    location: el.location,
                    //lit: "}".to_string()

                }),
                LxStatementEnd => {
                    if self.lx_loc != 0 && self.lexvec[self.lx_loc - 1].kind != LxLBrace {
                        self.add(LegacyToken {
                            tokentype: TkStatementEnd,
                            value: None,
                            location: el.location,
                            //lit: "".to_string()

                        })
                    }
                }
                LxEqual => self.add(LegacyToken {
                    tokentype: TkAssign,
                    value: None,
                    location: el.location,
                    //lit: "=".to_string()

                }),
                LxAssignInfer => self.add(LegacyToken {
                    tokentype: TkAssignInfer,
                    value: None,
                    location: el.location,
                    //lit: ":=".to_string()

                }),
                
                LxCEQ => self.add(LegacyToken {
                    tokentype: TkCEQ,
                    value: None,
                    location: el.location,
                    //lit: "==".to_string()

                }),
                LxCNE => self.add(LegacyToken {
                    tokentype: TkCNE,
                    value: None,
                    location: el.location,
                    //lit: "!=".to_string()

                }),
                LxCLT => self.add(LegacyToken {
                    tokentype: TkCLT,
                    value: None,
                    location: el.location,
                    //lit: "<".to_string()

                }),
                LxCLE => self.add(LegacyToken {
                    tokentype: TkCLE,
                    value: None,
                    location: el.location,
                    //lit: "<=".to_string()

                }),
                LxCGT => self.add(LegacyToken {
                    tokentype: TkCGT,
                    value: None,
                    location: el.location,
                    //lit: ">".to_string()

                }),
                LxCGE => self.add(LegacyToken {
                    tokentype: TkCGE,
                    value: None,
                    location: el.location,
                    //lit: ">=".to_string()

                }),
                LxAnd => self.add(LegacyToken {
                    tokentype: TkAnd,
                    value: None,
                    location: el.location,
                    //lit: "&&".to_string()

                }),
                LxOr => self.add(LegacyToken {
                    tokentype: TkOr,
                    value: None,
                    location: el.location,
                    //lit: "||".to_string()

                }),
                LxComma => self.add(LegacyToken {
                    tokentype: TkComma,
                    value: None,
                    location: el.location,
                    //lit: ",".to_string()

                }),
                LxColon => self.add(LegacyToken {
                    tokentype: TkColon,
                    value: None,
                    location: el.location,
                    //lit: ": ".to_string()

                }),
                LxDot => self.add(LegacyToken {
                    tokentype: TkDot,
                    value: None,
                    location: el.location,
                    //lit: ".".to_string()

                }),
                Err => {
                    panic!("uh oh")
                }
                Eof => self.add(LegacyToken {
                    tokentype: TokenType::TkEof,
                    value: None,
                    location: el.location,
                    //lit: "".to_string()

                }),
            }

            self.lx_loc += 1;
        }
    }

    fn import_pass(&mut self) -> Vec<LegacyToken> {
        let tv: Vec<LegacyToken> = self.tkvec.clone();
        while self.tk_loc < tv.len() {
            match self.tkvec[self.tk_loc].tokentype {
                TkKwUse => {
                    let to_import = self.next_t().value.unwrap().get_string().unwrap();
                    let mut nfile = crate::root::legacy_compile_import(&to_import);
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

    pub fn supply(&mut self) -> Vec<LegacyToken> {
        self.tkvec.clone()
    }
}
