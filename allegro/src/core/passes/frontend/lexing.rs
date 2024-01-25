use crate::core::resource::ast::SymbolValue;
use crate::core::resource::ast::SymbolValue::*;
use crate::core::resource::lexemes::Lexeme;
use crate::core::resource::lexemes::LexemeKind::*;
use crate::core::resource::ast::Ident;


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
    ($kind:tt, $literal:expr, $sel:expr) => {
        Lexeme {
            kind: $kind,
            value: $literal,
            location: $sel.location,
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

    fn add(&mut self, lx: Lexeme) {
        //print!("{lx:?}");
        self.lxvec.push(lx);
        self.advance();
    }

    fn advance(&mut self) {
        self.location += 1;
    }

    fn retreat(&mut self) {
        self.location -= 1;
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
                '\r' | '\n' | ';' => { self.add(create_lexeme!(LxStatementEnd, Nothing, self));},
                '(' => {
                    self.add(create_lexeme!(LxLparen, Nothing, self));
                }
                ')' => {
                    self.add(create_lexeme!(LxRparen, Nothing, self));
                }
                '=' => {
                    if self.next() == '>' {
                        self.add(create_lexeme!(LxBigArr, Nothing, self));
                        self.advance();
                    } else {
                        self.add(create_lexeme!(LxEqual, Nothing, self));
                    }
                }
                '+' => {
                    self.add(create_lexeme!(LxPlus, Nothing, self));
                    self.advance();
                }
                '-' => {
                    if self.next() == '>' {
                        self.add(create_lexeme!(LxSmallArr, Nothing, self));
                        self.advance()
                    } else if self.next() == '-' {
                        while self.srccharvec[self.location] != '\n'
                            && self.location < self.srccharvec.len()
                        {
                            self.advance();
                        }
                    } else {
                        self.add(create_lexeme!(LxMinus, Nothing, self));
                        self.advance()
                    }
                }
                '*' => {
                    self.add(create_lexeme!(LxStar, Nothing, self));
                }
                '/' => {
                    self.add(create_lexeme!(LxSlash, Nothing, self));
                }

                '%' => {
                    self.add(create_lexeme!(LxPercent, Nothing, self));
                }
                ',' => {
                    self.add(create_lexeme!(LxComma, Nothing, self));
                }
                '|' => {
                    self.add(create_lexeme!(LxPipe, Nothing, self));
                }
                '{' => {
                    self.add(create_lexeme!(LxLBrace, Nothing, self));
                }
                '}' => {
                    self.add(create_lexeme!(LxRBrace, Nothing, self));
                }
                '>' => {
                    if !((self.srccharvec[self.location - 1] == '-')
                        || (self.srccharvec[self.location - 1] == '='))
                    {
                        if self.next() == '=' {
                            self.add(create_lexeme!(LxCGE, Nothing, self));
                            self.advance()
                        } else {
                            self.add(create_lexeme!(LxCGT, Nothing, self));
                            self.advance()
                        }
                    }
                }

                '.' => {
                    if self.next() == '.' {
                        self.add(create_lexeme!(LxDoubleDot, Nothing, self));
                        self.advance();
                        self.advance()
                    } else {
                        self.add(create_lexeme!(LxDot, Nothing, self));
                        self.advance()
                    }
                }

                '<' => {
                    if self.next() == '=' {
                        self.add(create_lexeme!(LxCLE, Nothing, self));
                        self.advance()
                    } else {
                        self.add(create_lexeme!(LxCLT, Nothing, self));
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
                    self.add(create_lexeme!(LxColon, Nothing, self));
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
        self.add(create_lexeme!(LxStatementEnd, Nothing, self));
        self.add(create_lexeme!(Eof, Nothing, self));
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
        self.add(create_lexeme!(
            LxSymbol,
            SymbolValue::Identity(Ident::S(accumulator.iter().collect::<String>())),
            self
        ));
        self.retreat();
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
        if accumulator.contains(&'.') {
            self.add(create_lexeme!(
                LxNumeric,
                Float(accumulator.iter().collect::<String>().parse().unwrap()),
                self
            ))
        } else {
            self.add(create_lexeme!(
            LxNumeric,
            Int(accumulator.iter().collect::<String>().parse().unwrap()),
            self
        ))
        }
        self.retreat();
    }

    fn create_literal(&mut self) {
        let mut accumulator: Vec<char> = vec![];
        self.advance(); //Continue past the initial "
        while (self.current() != '"') && (self.current() != '\n') {
            accumulator.push(self.current());
            self.advance();
        }
        self.advance(); //Continue past the final "
        self.add(create_lexeme!(
            LxLiteral,
            SymbolValue::Str(accumulator.iter().collect::<String>()),
            self
        ));
        self.retreat();
    }

    pub fn supply(&mut self) -> Vec<Lexeme> {
        self.lxvec.clone()
    }
}
