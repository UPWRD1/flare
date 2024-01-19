use crate::core::resource::lexemes::Lexeme;
use crate::core::resource::{
    lexemes::LexemeKind,
    lexemes::LexemeKind::*,
};

use crate::lexingerror;
use crate::quit;

#[derive(Debug, Clone)]
pub struct Lexer {
    source: String,
    location: usize,
    srccharvec: Vec<char>,
    lxvec: Vec<Lexeme>,
}

macro_rules! create_lexeme {
    ($lx:tt, $st:expr, $ln:expr) => {
        Lexeme {
            kind: $lx,
            character: $st,
            location: $ln
        }
    };
}

impl Lexer {
    pub fn new(source: String) -> Self {
        Lexer {
            source,
            location: 0,
            srccharvec: vec![],
            lxvec: vec![],
        }
    }
    /*
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
    */

    fn add(&mut self, lx: LexemeKind) {
        self.lxvec.push(create_lexeme!(lx, self.srccharvec[self.location], self.location));
        self.advance();
    }

    fn advance(&mut self) {
        self.location += 1;
    }

    fn next(&mut self) -> char {
        self.srccharvec[self.location + 1]
    }

    fn current(&mut self) -> char {
        self.srccharvec[self.location]
    }

    pub fn lex(&mut self) {
        self.srccharvec = self.source.chars().collect();
        let mut _lc = self.location;
        while self.location < self.srccharvec.len() - 1 {
            let _lc: usize = self.location;
            //println!("{} {}", self.location, self.svec[self.location]);
            match self.srccharvec[self.location] {
                ' ' | '\t' => self.advance(),
                '\n' | '\r' => self.add(LxStatementEnd),
                '(' => {
                    self.add(LxLparen);
                }
                ')' => {
                    self.add(LxRparen);
                }
                '=' => {
                    if self.next() == '>' {
                        self.add(LxBigArr);
                        self.advance();
                    } else {
                        self.add(LxEqual);
                    }
                }
                '+' => {
                    self.add(LxPlus);
                    self.advance();
                }
                '-' => {
                    if self.next() == '>' {
                        self.add(LxSmallArr);
                        self.advance()
                    } else if self.next() == '-' {
                        while self.srccharvec[self.location] != '\n' && self.location < self.srccharvec.len() {
                            self.advance();
                        }
                    } else {
                        self.add(LxMinus);
                        self.advance()
                    }
                }
                '*' => {
                    self.add(LxStar);
                }
                '/' => {
                    self.add(LxSlash);
                }

                '%' => {
                    self.add(LxPercent);
                }
                ',' => {
                    self.add(LxComma);
                }
                '|' => {
                    self.add(LxPipe);
                }
                '{' => {
                    self.add(LxLBrace);
                }
                '}' => {
                    self.add(LxRBrace);
                }
                '>' => {
                    if !((self.srccharvec[self.location - 1] == '-')
                        || (self.srccharvec[self.location - 1] == '='))
                    {
                        if self.next() == '=' {
                            self.add(LxCGE);
                            self.advance()
                        } else {
                            self.add(LxCGT);
                            self.advance()
                        }
                    }
                }

                '.' => {
                    if self.next() == '.' {
                        self.add(LxDoubleDot);
                        self.advance()
                    } else {
                        self.add(LxDot);
                        self.advance()
                    }
                }

                '<' => {
                    if self.next() == '=' {
                        self.add(LxCLE);
                        self.advance()
                    } else {
                        self.add(LxCLT);
                        self.advance()
                    }
                }
                '0' | '1' | '2' | '3' | '4' | '5' | '6' | '7' | '8' | '9' => {
                    self.get_numeric();
                }

                'a'..='z' | 'A'..='Z' | '_' => {
                    self.get_symbol();
                }

                '"' => {
                    self.get_literal();
                }

                ':' => {
                    self.add(LxColon);
                    self.advance();
                }

                _ => {lexingerror!(
                    "IDK: '{}' {:b}, {}",
                    self.current(),
                    self.current() as usize,
                    self.location
                );},
            }
            //println!("{:?}", self.Lxvec);
        }
        _lc = self.location;
        self.add(LxStatementEnd);
        self.add(Eof);
    }



    fn get_symbol(&mut self) {
        let mut accumulator: Vec<char> = vec![];
        while (self.current() != ' ')
            && (self.current() != ':')
            && (self.current() != '(')
            && (self.current() != ';')
            && (self.current() != '\n')
            && (self.current() != ',')
            && (self.current() != ')')
        {
            accumulator.push(self.current());
            self.advance();
        }
        /*
        self.lxvec.push(LxIdent(Identtype::Sym(
            accumulator.iter().collect::<String>(),
            Identclass::Symbol,
        )))
        */
        self.add(LxSymbol(accumulator.iter().collect::<String>(),
    ))
    }

    fn get_numeric(&mut self) {
        let mut accumulator: Vec<char> = vec![];
        while (self.current() != ' ')
            && (self.current() != ':')
            && (self.current() != '(')
            && (self.current() != ';')
            && (self.current() != '\n')
            && (self.current() != ',')
            && (self.current() != ')')
        {
            accumulator.push(self.current());
            self.advance();
        }
        self.add(LxNumeric(
            accumulator.iter().collect::<String>().parse().unwrap(),
        ))
    }

    fn get_literal(&mut self) {
        let mut accumulator: Vec<char> = vec![];
        self.advance(); //Continue past the initial "
        while (self.current() != '"') && (self.current() != '\n') {
            accumulator.push(self.current());
            self.advance();
        }
        self.advance(); //Continue past the final "
        self.add(LxLiteral(accumulator.iter().collect::<String>()));
    }

    pub fn supply(&mut self) -> Vec<Lexeme> {
        self.lxvec.clone()
    }
}
