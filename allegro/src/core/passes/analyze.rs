use crate::core::resource::{
    lexemes::Lexeme,
    lexemes::LexemeKind::{*, self},
    tokens::{TokenKind, Token},
};

pub struct Analyzer {
    lexvec: Vec<Lexeme>,
    tkvec: Vec<Token>,
    loc: usize,
}

macro_rules! ctk {
    ($kind:tt, $lexeme:expr, $literal:expr, $location:expr) => {
        Token {
            kind: $kind,
            lexeme: $lexeme,
            literal: $literal,
            location: $location
        }
    };
}

impl Analyzer {
    pub fn new(cstvec: Vec<Lexeme>) -> Self {
        Analyzer {
            lexvec: cstvec,
            loc: 0,
            tkvec: vec![],
        }
    }

    fn add(&mut self, t: TokenKind, el: LexemeKind) {
        self.tkvec.push(ctk!(t, self.lexvec[self.loc].character.to_string(), el, self.loc));
    }

    pub fn analyze(&mut self) {
        while self.loc < self.lexvec.len() {
            let el = &self.lexvec[self.loc].clone();
            //println!("{:?}", el);

            match &el.kind {
                LxSymbol(sy) => {
                    let toadd = match sy.as_str() {
                        "val" => TokenKind::TkKWVal,
                        "op" => TokenKind::TkKWOp,

                        "Int" => TokenKind::TkTyInt,
                        "Flt" => TokenKind::TkTyFlt,
                        "Str" => TokenKind::TkTyStr,
                        &_ => TokenKind::TkSymbol(sy.to_string()),
                    };
                    self.add(toadd, LxSymbol(sy.to_string()))
                },
                LxLiteral(st) => {self.add(TokenKind::TkLiteral(st.to_string()), LxLiteral(st.to_string()))},
                LxNumeric(i) => {self.add(TokenKind::TkNumeric(*i))},
                LxPlus => {self.add(TokenKind::TkPlus)},
                LxMinus => {self.add(TokenKind::TkMinus)},
                LxStar => {self.add(TokenKind::TkStar)},
                LxSlash => {self.add(TokenKind::TkSlash)},
                LxLparen => {self.add(TokenKind::TkLparen)},
                LxRparen => {self.add(TokenKind::TkRparen)},
                LxSmallArr => {self.add(TokenKind::TkSmallArr)},
                LxBigArr => {self.add(TokenKind::TkBigArr)},
                LxPipe => {self.add(TokenKind::TkPipe)},
                LxPercent => {self.add(TokenKind::TkPercent)},
                LxDoubleDot => {self.add(TokenKind::TkDoubleDot)},
                LxLBrace => {self.add(TokenKind::TkLBrace)},
                LxRBrace => {self.add(TokenKind::TkRBrace)},
                LxStatementEnd => {self.add(TokenKind::TkStatementEnd)},
                LxEqual => {self.add(TokenKind::TkEqual)},
                LxCEQ => {self.add(TokenKind::TkCEQ)},
                LxCNE => {self.add(TokenKind::TkCNE)},
                LxCLT => {self.add(TokenKind::TkCLT)},
                LxCLE => {self.add(TokenKind::TkCLE)},
                LxCGT => {self.add(TokenKind::TkCGT)},
                LxCGE => {self.add(TokenKind::TkCGE)},
                LxAnd => {self.add(TokenKind::TkAnd)},
                LxOr => {self.add(TokenKind::TkOr)},
                LxComma => {self.add(TokenKind::TkComma)},
                LxColon => {self.add(TokenKind::TkColon)},
                LxDot => {self.add(TokenKind::TkDot)},
                Err => {panic!("uh oh")},
                Eof => {self.add(TokenKind::TEof)},
            }
            /*
            match el {
                LxItem => {
                    match id.name.as_str() {
                        "val" => self.add(TkKWVal) => {},
                        "op" => self.add(TkKWOp) => {},

                        "Flt" => self.add(TkTyFlt),
                        "Int" => self.add(TkTyInt),
                        "Str" => self.add(TkTyStr),
                        _ => {
                            self.add(TkItem(Item {
                                name: id.name.clone(),
                                class: id.class.clone(),
                                value: id.value.clone(),
                            }));
                        }
                    }
                    /*
                        id.
                        match id {
                            Identtype::Sym(s, at) => match s.as_str() {
                            "val" => self.add(TkKWVal),
                            "op" => self.add(TkKWOp),

                            "Flt" => self.add(TkTyFlt),
                            "Int" => self.add(TkTyInt),
                            "Str" => self.add(TkTyStr),
                            &_ => {
                                let previous_tk = &self.tkvec[self.tkvec.len() - 1];
                                match previous_tk {
                                    TkKWOp => self.add(TkIdent(Identtype::Sym(
                                        s.to_string(),
                                        Identclass::Operation,
                                    ))),
                                    TkKWVal => self.add(TkIdent(Identtype::Sym(
                                        s.to_string(),
                                        Identclass::Symbol,
                                    ))),
                                    _ => self.add(TkIdent(Identtype::Sym(
                                        s.to_string(),
                                        Identclass::Symbol,
                                    ))),
                                }
                            }
                        },
                        _ => unreachable!(),
                    }
                    */
                }

                LxLiteral(s) => self.add(TkItem(Item {
                    name: s.to_string(),
                    class: Itemclass::Literal,
                    value: Itemtype::Str(s.to_string()),
                })),
                LxNumeric(n) => {
                    self.add(TkItem(Item {
                    name: n.to_string(),
                    class: Itemclass::Integer,
                    value: Itemtype::Int(*n),
                }));}
                LxPlus => self.add(TkPlus),
                LxMinus => self.add(TkMinus),
                LxStar => self.add(TkStar),
                LxSlash => self.add(TkSlash),
                LxLparen => self.add(TkLparen),
                LxRparen => self.add(TkRparen),
                LxSmallArr => self.add(TkSmallArr),
                LxBigArr => self.add(TkBigArr),
                LxPipe => self.add(TkPipe),
                LxPercent => self.add(TkPercent),
                LxDoubleDot => self.add(TkTyMute),
                LxLBrace => self.add(TkLBrace),
                LxRBrace => self.add(TkRBrace),
                LxStatementEnd => {
                    //nothing
                }
                LxEqual => {
                    //self.add(TkEqual);
                }
                LxCEQ => self.add(TkCEQ),
                LxCNE => self.add(TkCNE),
                LxCLT => self.add(TkCLT),
                LxCLE => self.add(TkCLE),
                LxCGT => self.add(TkCGT),
                LxCGE => self.add(TkCGE),
                LxAnd => self.add(TkAnd),
                LxOr => self.add(TkOr),
                LxComma => self.add(TkComma),
                LxColon => {
                    //self.add(TkColon);
                }
                LxDot => self.add(TkDot),
                Eof => self.add(TEof),
            }
             */
            
            self.loc += 1;
        }
    }

    pub fn supply(&mut self) -> Vec<Token> {
        self.tkvec.clone()
    }
}
