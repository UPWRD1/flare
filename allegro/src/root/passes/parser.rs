use peg::{Parse, ParseElem, RuleResult};

pub struct SliceByRef<'a, T>(pub &'a [T]);

impl<'a, T> Parse for SliceByRef<'a, T> {
    type PositionRepr = usize;
    fn start(&self) -> usize {
        0
    }

    fn is_eof(&self, pos: usize) -> bool {
        pos >= self.0.len()
    }

    fn position_repr(&self, pos: usize) -> usize {
        pos
    }
}

impl<'a, T: 'a> ParseElem<'a> for SliceByRef<'a, T> {
    type Element = &'a T;

    fn parse_elem(&'a self, pos: usize) -> RuleResult<&'a T> {
        match self.0[pos..].first() {
            Some(c) => RuleResult::Matched(pos + 1, c),
            None => RuleResult::Failed,
        }
    }
}

peg::parser!( grammar lang<'a>() for SliceByRef<'a, Token> {
    pub rule program() -> crate::root::resource::ast::Module
        = a: clause()* {crate::root::resource::ast::Module {body: a}}

    rule clause() -> crate::root::resource::ast::Ast
        = funcdef()
        / typedef()
        / defblock()
        / with_clause()

    rule typedef() -> crate::root::resource::ast::Ast
        = [Token { kind: Tk::TkKwType, .. }] [Token { kind: Tk::TkSymbol, lit: name }] t: generic_brackets()? [Token { kind: Tk::TkAssign, .. }] v: typedef_choice(name.to_string()) {v}

    rule typedef_choice(name: String) -> crate::root::resource::ast::Ast
        = structdef(name.clone())
        / enumdef(name.clone())

    rule structdef(name: String) -> crate::root::resource::ast::Ast
        = [Token { kind: Tk::TkKwStruct, .. }] [Token { kind: Tk::TkKwOf, .. }] m: memberlist() { crate::root::resource::ast::Ast::Struct { name: name.to_string(), members: m.into() }}

    rule memberlist() -> Vec<(String, crate::root::resource::ast::SymbolType)>
        = v: ([Token { kind: Tk::TkSymbol, lit: name }] [Token { kind: Tk::TkColon, .. }] a: atype() {(name.to_string(), a)}) ** [Token { kind: Tk::TkComma, .. }]

    rule enumdef(name: String) -> crate::root::resource::ast::Ast
        = [Token { kind: Tk::TkKwEnum, .. }] [Token { kind: Tk::TkKwOf, .. }] m: variantlist() { crate::root::resource::ast::Ast::Enum { name: name.to_string(), members: m.into() }}

    rule variantlist() -> Vec<(crate::root::resource::ast::SymbolType)>
        = v: ([Token { kind: Tk::TkSymbol, lit: name }] a: varianttype()? {crate::root::resource::ast::SymbolType::Variant(name.to_string(), if a.is_some() {a.unwrap().into()} else {vec![].into()})}) ** [Token { kind: Tk::TkComma, .. }]

    rule varianttype() -> Vec<crate::root::resource::ast::SymbolType>
        = [Token { kind: Tk::TkLparen, .. }] a: atype() ** [Token { kind: Tk::TkComma, .. }] [Token { kind: Tk::TkRparen, .. }] {a}

    rule defblock() -> crate::root::resource::ast::Ast
        = [Token { kind: Tk::TkKwDef, .. }] name: atype() [Token { kind: Tk::TkAssign, .. }] f: funcdef() * [Token { kind: Tk::TkKwEnd, .. }] { crate::root::resource::ast::Ast::TypeDef { name: name, funcs: f.into() }}

    rule with_clause() -> crate::root::resource::ast::Ast
        = [Token { kind: Tk::TkKwWith, .. }] s: namespace() {crate::root::resource::ast::Ast::WithClause { include: s }}

    rule funcdef() -> crate::root::resource::ast::Ast
        = [Token { kind: Tk::TkKwLet, .. }] [Token { kind: Tk::TkSymbol, lit: name }] args: func_args() r: func_ret_type()? limits: where_limit() [Token { kind: Tk::TkAssign, .. }] start: position!() body: expr()+ { crate::root::resource::ast::Ast::FnDef { name: name.to_string(), args: if args.is_some() {args.unwrap().into()} else {vec![].into()}, rettype: if r.is_some() {r.unwrap()} else {crate::root::resource::ast::SymbolType::Generic(name.to_string())}, limits, body } }
        / [Token { kind: Tk::TkKwLet, .. }] [Token { kind: Tk::TkSymbol, lit: name }] [Token { kind: Tk::TkAssign, .. }] body: expr()+ { crate::root::resource::ast::Ast::FnDef { name: name.to_string(), args: vec![].into(), rettype: crate::root::resource::ast::SymbolType::Naught, limits: None, body } }

    rule func_ret_type() -> crate::root::resource::ast::SymbolType
        = [Token { kind: Tk::TkArr, .. }] r: atype() {r}

    rule func_args() -> Option<Vec<(String, crate::root::resource::ast::SymbolType)>> 
        = ([Token { kind: Tk::TkKwOf, .. }] l: func_args_list() {l})?

    rule where_limit() -> Option<Vec<crate::root::resource::ast::FnArgLimit>> =
        (([Token { kind: Tk::TkKwWhere, .. }] n: [n if n.kind == Tk::TkSymbol] [Token { kind: Tk::TkKwIs, .. }] l: [l if l.kind == Tk::TkSymbol] {crate::root::resource::ast::FnArgLimit {name: n.lit.clone(), limit: l.lit.clone()}}) ** [Token { kind: Tk::TkComma, .. }])?

    rule func_args_list() -> Vec<(String, crate::root::resource::ast::SymbolType)>
        = a: type_arg() ** [Token { kind: Tk::TkComma, .. }] {a}

    rule type_arg() -> (String, crate::root::resource::ast::SymbolType)
        = t: [t if t.kind == Tk::TkSymbol] k: arg_type()? start: position!() {let r = if k.is_some()  {k.unwrap()} else {crate::root::resource::ast::SymbolType::Generic(format!("?_{}", crate::root::resource::ast::calculate_hash::<String>(&t.lit) ))}; return (t.lit.clone(), r)}

    rule arg_type() -> crate::root::resource::ast::SymbolType
        = [Token { kind: Tk::TkColon, .. }] k: atype() {k}

    rule expr() -> crate::root::resource::ast::Expr
        = assignment()
        / closure()
        / structinstance()
        / ifexpr()
        / r#return()
        / binary_op()

    rule assignment() -> crate::root::resource::ast::Expr
        = n: symbol() [Token { kind: Tk::TkAssign, .. }] v: expr()  { crate::root::resource::ast::Expr::Assignment { name: Box::new(n), value: Box::new(v) } }
        / [Token { kind: Tk::TkKwMut, .. }] n: symbol() [Token { kind: Tk::TkAssign, .. }] v: expr()  { crate::root::resource::ast::Expr::MutableAssignment { name: Box::new(n), value: Box::new(v) } }

    rule closure() -> crate::root::resource::ast::Expr
        = [Token { kind: Tk::TkKwFn, .. }] args: func_args() [Token { kind: Tk::TkArr, .. }] body: expr()+ { crate::root::resource::ast::Expr::Closure { args: args.unwrap(), body } }

    rule r#return() -> crate::root::resource::ast::Expr
        = [Token { kind: Tk::TkKwReturn, .. }] v: expr() { crate::root::resource::ast::Expr::Return { value: Box::new(v) } }

    rule binary_op() -> crate::root::resource::ast::Expr = precedence!{
        l: (@) op: [Token { kind: Tk::TkFuncComp, .. }] r: @ {crate::root::resource::ast::Expr::Composition { l: Box::new(l), r: Box::new(r) }}
        l: (@) op: [Token { kind: Tk::TkCEQ, .. }] r: @ {crate::root::resource::ast::Expr::Logical { l: Box::new(l), op: crate::root::resource::ast::LogicOp::CEQ, r: Box::new(r) }}
        l: (@) op: [Token { kind: Tk::TkCLT, .. }] r: @ {crate::root::resource::ast::Expr::Logical { l: Box::new(l), op: crate::root::resource::ast::LogicOp::CLT, r: Box::new(r) }}
        l: (@) op: [Token { kind: Tk::TkCLE, .. }] r: @ {crate::root::resource::ast::Expr::Logical { l: Box::new(l), op: crate::root::resource::ast::LogicOp::CLE, r: Box::new(r) }}
        l: (@) op: [Token { kind: Tk::TkCGT, .. }] r: @ {crate::root::resource::ast::Expr::Logical { l: Box::new(l), op: crate::root::resource::ast::LogicOp::CGT, r: Box::new(r) }}
        l: (@) op: [Token { kind: Tk::TkCGE, .. }] r: @ {crate::root::resource::ast::Expr::Logical { l: Box::new(l), op: crate::root::resource::ast::LogicOp::CGE, r: Box::new(r) }}
        --
        l: (@) [Token { kind: Tk::TkPlus, .. }] r: @  { crate::root::resource::ast::Expr::BinAdd { l: Box::new(l), r: Box::new(r) } }
        l: (@) [Token { kind: Tk::TkMinus, .. }] r: @  { crate::root::resource::ast::Expr::BinSub { l: Box::new(l), r: Box::new(r) } }
        --
        l: (@) [Token { kind: Tk::TkStar, .. }] r: @  { crate::root::resource::ast::Expr::BinMul { l: Box::new(l), r: Box::new(r) } }
        l: (@) [Token { kind: Tk::TkSlash, .. }] r: @  { crate::root::resource::ast::Expr::BinDiv { l: Box::new(l), r: Box::new(r) } }
        --
        a: atom() {a}
    }

    rule atom() -> crate::root::resource::ast::Expr
        = intrinsic()
        / call()
        / symbol()
        / group()

    rule ifexpr() -> crate::root::resource::ast::Expr
        = [Token { kind: Tk::TkKwIf, .. }] c: expr() [Token { kind: Tk::TkKwThen, .. }] t: expr() e: else_branch() {crate::root::resource::ast::Expr::If { condition: Box::new(c), then: Box::new(t), otherwise: e }}

    rule else_branch() -> Box<crate::root::resource::ast::Expr>
        = ([Token { kind: Tk::TkKwElse, .. }] o: expr() {Box::new(o)})

    rule intrinsic() -> crate::root::resource::ast::Expr
        = [Token { kind: Tk::TkFlt, lit: n }] { crate::root::resource::ast::Expr::Flt(n.parse().unwrap() )}
        / [Token { kind: Tk::TkInt, lit: n }] { crate::root::resource::ast::Expr::Int(n.parse().unwrap() )}
        / [Token { kind: Tk::TkStrLit, lit: n }] { crate::root::resource::ast::Expr::Str(n.parse().unwrap() )}
        / [Token { kind: Tk::TkFalse, lit: n }] { crate::root::resource::ast::Expr::Bool(false)}
        / [Token { kind: Tk::TkTrue, lit: n }] { crate::root::resource::ast::Expr::Bool(true)}

    rule call() -> crate::root::resource::ast::Expr
        = name: symbol() [Token { kind: Tk::TkLparen, .. }] args: call_list() [Token { kind: Tk::TkRparen, .. }] { crate::root::resource::ast::Expr::Call { name: Box::new(name), args}}
        // parent: namespace() [Token { kind: Tk::TkLparen, .. }] args: call_list() [Token { kind: Tk::TkRparen, .. }] { crate::root::resource::ast::Expr::Call { name: Box::new(parent.get(0).unwrap().clone()), args: args, namespace: parent}}
    
    rule call_list() -> Vec<crate::root::resource::ast::Expr>
        = expr() ** [Token { kind: Tk::TkComma, .. }]

    rule structinstance() -> crate::root::resource::ast::Expr
        = n: symbol() [Token { kind: Tk::TkLbrace, .. }] f: assignment() ** [Token { kind: Tk::TkComma, .. }] [Token { kind: Tk::TkRbrace, .. }] {crate::root::resource::ast::Expr::StructInstance { name: Box::new(n), fields: f }}

    rule symbol() -> crate::root::resource::ast::Expr
        = s: simplesymbol() a: ([Token { kind: Tk::TkDot, ..}] c: simplesymbol() {c.get_symbol_name()})? {crate::root::resource::ast::Expr::Symbol(format!("{}{}", s.get_symbol_name(), if a.is_some() {format!(".{}", a.unwrap().clone())} else {"".to_string()}))}

    rule simplesymbol() -> crate::root::resource::ast::Expr
        = [Token { kind: Tk::TkSymbol, lit: n }] { crate::root::resource::ast::Expr::Symbol(n.to_string())}

    rule atype() -> crate::root::resource::ast::SymbolType
        = [Token { kind: Tk::TkKwInt, .. }] {crate::root::resource::ast::SymbolType::Int}
        / [Token { kind: Tk::TkKwFlt, .. }] {crate::root::resource::ast::SymbolType::Flt}
        / [Token { kind: Tk::TkKwStr, .. }] {crate::root::resource::ast::SymbolType::Str}
        / [Token { kind: Tk::TkKwBool, .. }] {crate::root::resource::ast::SymbolType::Bool}
        / [Token { kind: Tk::TkKwFnTy, .. }] {crate::root::resource::ast::SymbolType::Fn(vec![].into(), crate::root::resource::ast::SymbolType::Unknown.into())}
        / [Token { kind: Tk::TkKwNaught, .. }] {crate::root::resource::ast::SymbolType::Naught}
        / s: symbol() a: (generic_brackets())? {crate::root::resource::ast::SymbolType::Custom(s.get_symbol_name(), if a.is_some() {a.unwrap().into()} else {vec![].into()})}
        / [Token { kind: Tk::TkQuestion, .. }] s: simplesymbol() start:position!() {crate::root::resource::ast::SymbolType::Generic(s.get_symbol_name())}

    rule generic_brackets() -> Vec<crate::root::resource::ast::SymbolType>
        = [Token { kind: Tk::TkCLT, .. }] a: atype() ** [Token { kind: Tk::TkComma, .. }] [Token { kind: Tk::TkCGT, .. }]{a}

    rule namespace() -> Vec<crate::root::resource::ast::Expr>
        = simplesymbol() ** [Token { kind: Tk::TkDoubleColon, .. }] 

    rule group() -> crate::root::resource::ast::Expr
        = [Token { kind: Tk::TkLparen, .. }] v:expr() [Token { kind: Tk::TkRparen, .. }] { v }

});

use lang::program;

use crate::root::resource::{
    ast::Module,
    tk::{Tk, Token},
};

pub fn parse(tokens: &[Token]) -> Module {
    let p = program(&SliceByRef(&tokens)).unwrap();
    p
}
