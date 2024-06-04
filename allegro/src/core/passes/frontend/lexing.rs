use crate::core::resource::ast::Ident;
use crate::core::resource::ast::Scalar;
use crate::core::resource::ast::SymbolValue;
use crate::core::resource::environment::AKind;
use crate::core::resource::lexemes::Lexeme;
use crate::core::resource::lexemes::LexemeKind::*;

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
        //let mut _lc = self.location;
        while self.location < self.srccharvec.len() - 1 {
            let _lc: usize = self.location;
            match self.srccharvec[self.location] {
                ' ' | '\t' => self.advance(),
                '\r' | '\n' | ';' => {
                    self.add(create_lexeme!(LxStatementEnd, Nothing, self));
                }
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
                        self.advance();
                    } else if self.next() == '-' {
                        while self.srccharvec[self.location] != '\n'
                            && self.location < self.srccharvec.len()
                        {
                            self.advance();
                        }
                    } else if (0..9).contains(&self.next().to_string().parse::<i32>().unwrap()) {
                        self.create_numeric()
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
                        self.add(create_lexeme!(LxDoubleDot, Mute, self));
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
                    if self.next() == '=' {
                        self.add(create_lexeme!(LxAssignInfer, Nothing, self));
                        self.advance();
                    } else {
                        self.add(create_lexeme!(LxColon, Nothing, self));
                        self.advance();
                    }
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
        }
        //_lc = self.location;
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
        self.add(Lexeme {
            kind: LxSymbol(SymbolValue::Identity(Ident {
                name: accumulator.iter().collect::<String>(),
                value: Box::new(SymbolValue::Unknown),
                kind: AKind::TyUnknown
                //kind: None,
                //value: Box::new(SymbolValue::Scalar(Scalar::Str(accumulator.iter().collect::<String>()))),
            })),

            location: self.location,
        });
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
            self.add(Lexeme {
                kind: LxScalar(Scalar::Float(
                    accumulator.iter().collect::<String>().parse().unwrap(),
                )),
                //value: SymbolValue::Float(accumulator.iter().collect::<String>().parse().unwrap()),
                location: self.location,
            })
        } else {
            self.add(Lexeme {
                kind: LxScalar(Scalar::Int(
                    accumulator.iter().collect::<String>().parse().unwrap(),
                )),
                //value: SymbolValue::Int(accumulator.iter().collect::<String>().parse().unwrap()),
                location: self.location,
            })
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
        self.advance();
        self.add(Lexeme {
            kind: LxScalar(Scalar::Str(accumulator.iter().collect::<String>())),
            //value: SymbolValue::Str(accumulator.iter().collect::<String>()),
            location: self.location,
        }); //Continue past the final "
        self.retreat();
    }

    pub fn supply(&mut self) -> Vec<Lexeme> {
        self.lxvec.clone()
    }
}
