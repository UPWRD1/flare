use crate::core::resource::ast::SymbolKind;
use crate::core::resource::ast::SymbolKind::*;
use crate::core::resource::lexemes::Lexeme;
use crate::core::resource::{lexemes::LexemeKind, lexemes::LexemeKind::*};

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
    ($lx:tt, $st:expr, $lt:expr, $ln:expr) => {
        Lexeme {
            kind: $lx,
            character: $st,
            literal: $lt,
            location: $ln,
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

    fn add(&mut self, lx: LexemeKind, sy: SymbolKind) {
        print!("{lx:?}:{sy:?}, ");
        //self.lxvec.push(create_lexeme!(lx,self.srccharvec[self.location - 0],sy,self.location));
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
                '\n' | '\r' => self.add(LxStatementEnd, Nothing),
                '(' => {
                    self.add(LxLparen, Nothing);
                }
                ')' => {
                    self.add(LxRparen, Nothing);
                }
                '=' => {
                    if self.next() == '>' {
                        self.add(LxBigArr, Nothing);
                        self.advance();
                    } else {
                        self.add(LxEqual, Nothing);
                    }
                }
                '+' => {
                    self.add(LxPlus, Nothing);
                    self.advance();
                }
                '-' => {
                    if self.next() == '>' {
                        self.add(LxSmallArr, Nothing);
                        self.advance()
                    } else if self.next() == '-' {
                        while self.srccharvec[self.location] != '\n'
                            && self.location < self.srccharvec.len()
                        {
                            self.advance();
                        }
                    } else {
                        self.add(LxMinus, Nothing);
                        self.advance()
                    }
                }
                '*' => {
                    self.add(LxStar, Nothing);
                }
                '/' => {
                    self.add(LxSlash, Nothing);
                }

                '%' => {
                    self.add(LxPercent, Nothing);
                }
                ',' => {
                    self.add(LxComma, Nothing);
                }
                '|' => {
                    self.add(LxPipe, Nothing);
                }
                '{' => {
                    self.add(LxLBrace, Nothing);
                }
                '}' => {
                    self.add(LxRBrace, Nothing);
                }
                '>' => {
                    if !((self.srccharvec[self.location - 1] == '-')
                        || (self.srccharvec[self.location - 1] == '='))
                    {
                        if self.next() == '=' {
                            self.add(LxCGE, Nothing);
                            self.advance()
                        } else {
                            self.add(LxCGT, Nothing);
                            self.advance()
                        }
                    }
                }

                '.' => {
                    if self.next() == '.' {
                        self.add(LxDoubleDot, Nothing);
                        self.advance()
                    } else {
                        self.add(LxDot, Nothing);
                        self.advance()
                    }
                }

                '<' => {
                    if self.next() == '=' {
                        self.add(LxCLE, Nothing);
                        self.advance()
                    } else {
                        self.add(LxCLT, Nothing);
                        self.advance()
                    }
                }
                '0' | '1' | '2' | '3' | '4' | '5' | '6' | '7' | '8' | '9' => {
                    self.create_numeric();
                }

                'a'..='z' | 'A'..='Z' | '_' => {
                    self.create_symbol();
                }

                '"' => {
                    self.create_literal();
                }

                ':' => {
                    self.add(LxColon, Nothing);
                    self.advance();
                }

                _ => {
                    lexingerror!(
                        "IDK: '{}' {:b}, {}",
                        self.current(),
                        self.current() as usize,
                        self.location
                    );
                }
            }
            //println!("{:?}", self.Lxvec);
        }
        _lc = self.location;
        self.add(LxStatementEnd, Nothing);
        self.add(Eof, Nothing);
    }

    fn create_symbol(&mut self) {
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
        self.add(
            LxSymbol,
            SymbolKind::Identity(accumulator.iter().collect::<String>()),
        )
    }

    fn create_numeric(&mut self) {
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
        self.add(
            LxNumeric,
            Int(accumulator.iter().collect::<String>().parse().unwrap()),
        )
    }

    fn create_literal(&mut self) {
        let mut accumulator: Vec<char> = vec![];
        self.advance(); //Continue past the initial "
        while (self.current() != '"') && (self.current() != '\n') {
            accumulator.push(self.current());
            self.advance();
        }
        self.advance(); //Continue past the final "
        self.add(
            LxLiteral,
            SymbolKind::Str(accumulator.iter().collect::<String>()),
        );
    }

    pub fn supply(&mut self) -> Vec<Lexeme> {
        self.lxvec.clone()
    }
}
