use super::{environment::AKind, tokens::{Token, TokenType}};

#[derive(Clone, Debug, PartialEq)]
pub struct AssignExpr {
    pub name: Token,
    pub value: Box<Expr>,
}

#[derive(Clone, Debug, PartialEq)]
pub struct BinExpr {
    pub left: Box<Expr>,
    pub operator: Token,
    pub right: Box<Expr>,
}

#[derive(Clone, Debug, PartialEq)]
pub struct CallExpr {
    pub callee: Token,
    pub paren: Token,
    pub args: Vec<Expr>,
}

#[derive(Clone, Debug, PartialEq)]
pub struct GroupExpr {
    pub expression: Box<Expr>,
}

#[derive(Clone, Debug, PartialEq)]
pub struct ScalarExpr {
    pub value: Token,
}

#[derive(Clone, Debug, PartialEq)]
pub struct LogicalExpr {
    pub left: Box<Expr>,
    pub operator: Token,
    pub right: Box<Expr>,
}

#[derive(Clone, Debug, PartialEq)]
pub struct UnaryExpr {
    pub operator: Token,
    pub right: Box<Expr>,
}

#[derive(Clone, Debug, PartialEq)]
pub struct ValueExpr {
    pub name: Token,
}


///Enum representing expression types
#[derive(Clone, Debug, PartialEq)]
#[allow(dead_code)]
pub enum Expr {
    Assign(AssignExpr),
    Binary(BinExpr),
    Call(CallExpr),
    Grouping(GroupExpr),
    ScalarEx(ScalarExpr),
    Logical(LogicalExpr),
    Unary(UnaryExpr),
    Value(ValueExpr),
    Empty,
}

impl Expr {
    ///Gets the token of the expressions
    pub fn get_expr_value(&self) -> Token {
        match self {
            Self::Assign(a) => a.name.clone(),
            Self::Binary(b) => b.left.clone().get_expr_value(),
            Self::Call(c) => c.callee.clone(),
            Self::Empty => {
                Token { tokentype: TokenType::TkType(AKind::TyMute), value: Some(SymbolValue::Mute), location: 0 }
            }
            Self::Grouping(g) => g.expression.get_expr_value().clone(),
            Self::ScalarEx(l) => l.value.clone(),
            Self::Logical(l) => l.left.clone().get_expr_value(),
            Self::Unary(u) => u.operator.clone(),
            Self::Value(v) => v.name.clone(),
        }
    }
    /*
    pub fn get_expr_type(&mut self) -> Option<AKind> {
        match self {
            Self::Assign(a) => Some(a.kind.clone()),
            Self::Binary(b) => b.left.clone().get_expr_type(),
            Self::Call(c) => c.callee.get_expr_type(),
            Self::Empty => {
                panic!("Cannot get value of empty expression!")
            }
            Self::Grouping(g) => g.expression.get_expr_type().clone(),
            Self::Literal(l) => l.value.value.clone(),
            Self::Logical(l) => l.left.get_expr_type().clone(),
            Self::Unary(u) => u.right.get_expr_type().clone(),
            Self::Value(v) => v.name.kind.clone(),
        }
    }
     */
}

///AST Block Statement
#[derive(Clone, Debug, PartialEq)]
pub struct BlockStmt {
    pub statements: Vec<Statement>,
}

///AST Expression Wrapper
#[derive(Clone, Debug, PartialEq)]
pub struct ExpressionStmt {
    pub expression: Expr,
}

///AST Operation Statement
#[derive(Clone, Debug, PartialEq)]
pub struct FuncDecl {
    pub name: Token,
    pub params: Vec<BindingDecl>,
    pub returnval: AKind,
    pub body: BlockStmt,
}

///AST If Statement
#[derive(Clone, Debug, PartialEq)]
pub struct IfStmt {
    pub condition: Expr,
    pub then_branch: Box<BlockStmt>,
    pub else_branch: Option<Box<BlockStmt>>,
}

//#[deprecated(since = "0.0.0", note = "PrintStmt may be replaced by standard library features")]
///AST Print Statement
#[derive(Clone, Debug, PartialEq)]
pub struct PrintStmt {
    pub expression: Expr,
}

///AST Return Statement
#[derive(Clone, Debug, PartialEq)]
pub struct ReturnStmt {
    pub value: Expr,
    pub returntype: AKind,
}

///AST Binding Declaration
#[derive(Clone, Debug, PartialEq)]
pub struct BindingDecl {
    pub name: Pair,
    pub initializer: Expr,
}

///Enum representing AST nodes.
#[derive(Clone, Debug, PartialEq)]
#[allow(dead_code)]
pub enum Statement {
    Block(BlockStmt),
    Expression(ExpressionStmt),
    Function(FuncDecl),
    If(IfStmt),
    Print(PrintStmt),
    Return(ReturnStmt),
    Bind(BindingDecl),
    Empty,
}

///Enum representing different scalar types
#[derive(Clone, Debug, PartialEq, PartialOrd)]
pub enum Scalar {
    Str(String),
    Int(i32),
    Float(f32),
    Bool(bool),
}

impl Scalar {
    ///Converts a Scalar to its equivalent AKind representation
    #[allow(dead_code)]
    pub fn to_akind(&self) -> AKind {
        match self {
            Self::Bool(_) => AKind::TyBool,
            Self::Float(_) => AKind::TyFlt,
            Self::Int(_) => AKind::TyInt,
            Self::Str(_) => AKind::TyStr,
        }
    }
}

///Enum showing different kinds of internal values
#[derive(Clone, Debug, PartialEq, PartialOrd)]
pub enum SymbolValue {
    Scalar(Scalar),
    Pair(Pair), //name value
    Unknown,
    Mute,
}

impl SymbolValue {
    ///Converts a SymbolValue to its equivalent AKind representation
    pub fn to_akind(&self) -> AKind {
        match self {
            Self::Scalar(s) => match s {
                Scalar::Bool(_) => AKind::TyBool,
                Scalar::Float(_) => AKind::TyFlt,
                Scalar::Int(_) => AKind::TyInt,
                Scalar::Str(_) => AKind::TyStr,
            },
            Self::Pair(i) => {
                i.clone().value.to_akind()
            }
            Self::Mute => AKind::TyMute,
            Self::Unknown => AKind::TyUnknown,
            //_ => panic!("Unknown type! {:?}", self),
        }
    }
    
    ///Converts the internal value of a SymbolValue to its string representation, or the name of the pair
    pub fn get_string(self) -> Option<String> {
        match self {
            Self::Pair(p) => Some(p.name),
            Self::Scalar(s) => match s {
                Scalar::Int(v) => format!("{v}").into(),
                Scalar::Float(v) => format!("{v}").into(),
                Scalar::Str(v) => v.to_string().into(),
                Scalar::Bool(v) => format!("{v}").into(),
            },
            Self::Mute => {
                Some("()".to_string())
            }
            _ => panic!("Cannot get string of value {:?}", self),
        }
    }
}

///Recursive struct representing a key/value pair, with type information:
#[derive(Clone, Debug, PartialEq, PartialOrd)]
pub struct Pair {
    pub name: String,
    pub kind: AKind,
    pub value: Box<SymbolValue>,
}
