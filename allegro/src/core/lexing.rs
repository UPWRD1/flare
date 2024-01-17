use super::tokens::CSToken;
use super::tokens::CSToken::*;

#[derive(Debug, Clone)]
pub struct Lexer {
    source: String,
    location: usize,
    srccharvec: Vec<char>,
    tkvec: Vec<CSToken>,
}

impl Lexer {
    pub fn new(source: String) -> Self {
        Lexer {
            source,
            location: 0,
            srccharvec: vec![],
            tkvec: vec![],
        }
    }

    pub fn check_keyword(&mut self, pos: usize, length: usize, tocheck: &str) -> bool {
        let mut collector: Vec<char> = vec![];
        //eprintln!("Searching for {} at {} with room for {}",tocheck, pos, length);
        for i in pos..(pos + length) {
            collector.push(self.srccharvec[i]);
        }
        //println!("{:?}", collector);
        let str: String = collector.iter().collect();
        let tc: String = tocheck.to_string();
        if str == tc {
            //println!("found");
            self.jump(length);
            true
        } else {
            //println!("not found");
            self.jump(1);
            false
        }
    }

    fn advance(&mut self) {
        self.location += 1;
    }

    fn retreat(&mut self) {
        self.location -= 1;
    }

    fn jump(&mut self, amt: usize) {
        self.location += amt;
    }

    pub fn lex(&mut self) {
        self.srccharvec = self.source.chars().collect();
        while self.location < self.srccharvec.len() - 1 {
            //println!("{} {}", self.location, self.svec[self.location]);
            match self.srccharvec[self.location] {
                ' ' | '\t' | '\r' => self.advance(),
                '\n' => self.add(TkStatementEnd),
                '(' => {
                    self.add(TkLparen);
                }
                ')' => {
                    self.add(TkRparen);
                }
                '=' => {
                    if self.srccharvec[self.location + 1] == '>' {
                        self.add(TkBigArr);
                        self.advance();
                    } else {
                        self.add(TkEqual);
                    }
                }
                '+' => {
                    self.add(TkPlus);
                }
                '-' => {
                    if self.srccharvec[self.location + 1] == '>' {
                        self.add(TkSmallArr);
                        self.advance()
                    } else {
                        self.add(TkMinus);
                        self.advance()
                    }
                }
                '*' => {
                    self.add(TkStar);
                    self.advance()
                }
                '/' => {
                    self.add(TkSlash);
                    self.advance()
                }

                '%' => {
                    self.add(TyDerive);
                    self.advance()
                }
                ',' => {
                    self.add(TkComma);
                    self.advance()
                }
                '|' => {
                    self.add(TkPipe);
                }
                '{' => {
                    self.add(TkLBrace);
                }
                '}' => {
                    self.add(TkRBrace);
                }
                '>' => {
                    if !((self.srccharvec[self.location - 1] == '-')
                        || (self.srccharvec[self.location - 1] == '='))
                    {
                        if self.srccharvec[self.location + 1] == '=' {
                            self.add(TkCGE);
                            self.advance()
                        } else {
                            self.add(TkCGT);
                            self.advance()
                        }
                    }
                }

                '.' => {
                    if self.srccharvec[self.location + 1] == '.' {
                            self.add(TyMute);
                            self.advance()
                        } else {
                            self.add(TkDot);
                            self.advance()
                        }
                }

                '<' => {
                    if self.srccharvec[self.location + 1] == '=' {
                        self.add(TkCLE);
                        self.advance()
                    } else {
                        self.add(TkCLT);
                        self.advance()
                    }
                }
                '0' | '1' | '2' | '3' | '4' | '5' | '6' | '7' | '8' | '9' => {
                    self.get_nliteral();
                }

                'a'..='z' | 'A'..='Z' | '_' => match self.srccharvec[self.location] {
                    'v' => {
                        if self.check_keyword(self.location, 3, "val") {
                            self.add(KwVal);
                        } else {
                            self.retreat();
                            self.get_ident()
                        }
                    }

                    'o' => {
                        if self.check_keyword(self.location, 2, "op") {
                            self.add(KwOperation);
                            //self.advance()
                        } else {
                            self.retreat();
                            self.get_ident()
                        }
                    }
                    'i' => {
                        if self.check_keyword(self.location, 2, "is") {
                            self.add(KwIs);
                            self.advance()
                        } else if self.check_keyword(self.location, 3, "int") {
                                self.add(TyInt);
                                self.advance()
                            } else {
                                self.retreat();
                                self.get_ident()
                            }
                            

                    }
                    'e' => {
                        if self.check_keyword(self.location, 3, "end") {
                            self.add(KwEnd);
                        } else {
                            self.retreat();
                            self.get_ident()
                        }
                    }

                    'f' => {
                        if self.check_keyword(self.location, 3, "flt") {
                            self.add(TyFloat);
                        } else {
                            self.retreat();
                            self.get_ident()
                        }
                    }

                    _ => {
                        //print!("ident: ");
                        self.get_ident();
                        //println!("{}", accumulator.iter().collect::<String>());
                    }
                },

                ':' => {
                    self.add(TkColon);
                    self.advance();
                }

                _ => panic!(
                    "IDK: '{}' {:b}, {}",
                    self.srccharvec[self.location],
                    self.srccharvec[self.location] as usize,
                    self.location
                ),
            }
            //println!("{:?}", self.tkvec);
        }
        self.add(Eof);
    }

    fn add(&mut self, tk: CSToken) {
        self.tkvec.push(tk);
        self.advance();
    }

    fn get_ident(&mut self) {
        let mut accumulator: Vec<char> = vec![];
        while (self.srccharvec[self.location] != ' ')
            && (self.srccharvec[self.location] != ':')
            && (self.srccharvec[self.location] != '(')
            && (self.srccharvec[self.location] != ';')
            && (self.srccharvec[self.location] != '\n')
            && (self.srccharvec[self.location] != ',')
            && (self.srccharvec[self.location] != ')')
        {
            accumulator.push(self.srccharvec[self.location]);
            self.advance();
        }
        self.tkvec.push(TkIdent(accumulator.iter().collect()))
    }

    fn get_nliteral(&mut self) {
        let mut accumulator: Vec<char> = vec![];
        while (self.srccharvec[self.location] != ' ')
            && (self.srccharvec[self.location] != ':')
            && (self.srccharvec[self.location] != '(')
            && (self.srccharvec[self.location] != ';')
            && (self.srccharvec[self.location] != '\n')
            && (self.srccharvec[self.location] != ',')
            && (self.srccharvec[self.location] != ')')
        {
            accumulator.push(self.srccharvec[self.location]);
            self.advance();
        }
        self.tkvec.push(TkLiteral(super::tokens::Atype::Int(accumulator.iter().collect::<String>().parse().unwrap())))
    }

    pub fn supply(&mut self) -> Vec<CSToken> {
        self.tkvec.clone()
    }
}
