use super::cst::SymbolType;


#[derive(Debug, Clone)]
pub enum Ast {
    Func(Vec<Box<SymbolType>>, Box<SymbolType>),
    Symbol(SymbolType),
}