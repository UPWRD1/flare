use super::tokens::{Atype, Ident};

#[derive(Debug, Clone)]
pub enum ASTNode {
    ValDecl(Ident, Atype) // name value
}