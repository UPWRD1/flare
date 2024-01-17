use self::ASTNode::*;
use super::ast::ASTNode;
use super::tokens::CSToken;
use super::tokens::CSToken::*;
use std::collections::HashSet;

pub struct Processor {
    cstvec: Vec<CSToken>,
    loc: usize,
    astvec: Vec<ASTNode>,
}

impl Processor {
    pub fn new(cstvec: Vec<CSToken>) -> Self {
        Processor {
            cstvec,
            loc: 0,
            astvec: vec![],
        }
    }

    pub fn process(&mut self) {
        while self.loc < self.cstvec.len() {
            let el = &self.cstvec[self.loc];
            //println!("{:?}", el);
            match el {
                KwVal => {
                    //println!("searching for vdecl at {}", self.loc);
                    //println!("{:?}", self.cstvec);
                    let nametk = &self.cstvec[self.loc + 1];
                    let valtk = &self.cstvec[self.loc + 5];
                    match nametk {
                        TkIdent(name) => match valtk {
                            TkLiteral(val) => {
                                let to_match1: Vec<CSToken> = vec![
                                    KwVal,
                                    TkIdent(name.clone()),
                                    TkColon,
                                    TyDerive,
                                    TkEqual,
                                    TkLiteral(val.clone()),
                                ];
                                let to_match2: Vec<CSToken> = vec![
                                    KwVal,
                                    TkIdent(name.clone()),
                                    TkEqual,
                                    TkLiteral(val.clone()),
                                ];
                                if self.cmp_cstvec(to_match1) {
                                    self.astvec.push(ValDecl(name.clone(), val.clone()))
                                } else if self.cmp_cstvec(to_match2) {
                                    self.astvec.push(ValDecl(name.clone(), val.clone()))
                                }
                            }
                            _ => {
                                panic!("Fail 1 ")
                            }
                        },
                        _ => {
                            panic!("Fail 2 ")
                        }
                    }
                }
                KwOperation => {}
                TkIdent(_s) => {}
                TkLiteral(_) => {}
                TkPlus => {}
                TkMinus => {}
                TkStar => {}
                TkSlash => {}
                TkLparen => {}
                TkRparen => {}
                TkSmallArr => {}
                TkBigArr => {}
                TkPipe => {}
                TkLBrace => {}
                TkRBrace => {}
                TkStatementEnd => {}
                TyFloat => {}
                TyDerive => {}
                TyInt => {}
                KwDo => {}
                KwIs => {}
                KwEnd => {}
                TkEqual => {}
                TkCEQ => {}
                TkCNE => {}
                TkCLT => {}
                TkCLE => {}
                TkCGT => {}
                TkCGE => {}
                TkAnd => {}
                TkOr => {}
                TkComma => {}
                TkColon => {}
                Eof => {}
                _ => panic!("Unknown Token {:?}", el),
            }
            self.loc += 1;
        }
    }

    fn cmp_cstvec(&self, to_match: Vec<CSToken>) -> bool {
        let actual: Vec<CSToken> = self.cstvec[self.loc..(self.loc + to_match.len())].to_vec();
        let tmset: HashSet<_> = to_match.iter().cloned().collect();
        //println!("{:?}", to_match);
        //println!("{:?}", actual);
        actual.iter().all(|item| tmset.contains(item))
    }

    pub fn supply(&mut self) -> Vec<ASTNode> {
        self.astvec.clone()
    }
}
