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
        / with_clause()

    rule with_clause() -> crate::root::resource::ast::Ast
        = [Token { kind: Tk::TkKwWith, .. }] s: namespace() {crate::root::resource::ast::Ast::WithClause { include: s }}

    rule funcdef() -> crate::root::resource::ast::Ast
        = [Token { kind: Tk::TkKwLet, .. }] [Token { kind: Tk::TkSymbol, lit: name }] args: func_args() [Token { kind: Tk::TkAssign, .. }] body: expr()+ { crate::root::resource::ast::Ast::FnDef { name: name.to_string(), args: if args.is_some() {args.unwrap()} else {vec![]}, body: body } }

    rule func_args() -> Option<Vec<String>> =
        ([Token { kind: Tk::TkKwOf, .. }] l: func_args_list() {l})?

    rule func_args_list() -> Vec<String>
        = t: [t if t.kind == Tk::TkSymbol] ** [Token { kind: Tk::TkComma, .. }] {let mut v = vec![]; for i in t {v.push(i.lit.clone())}; return v}

    rule expr() -> crate::root::resource::ast::Expr
        = assignment()
        / closure()
        / r#return()
        / product()

    rule assignment() -> crate::root::resource::ast::Expr
        = n: symbol() [Token { kind: Tk::TkAssign, .. }] v: expr()  { crate::root::resource::ast::Expr::Assignment { name: Box::new(n), value: Box::new(v) } }
        / [Token { kind: Tk::TkKwMut, .. }] n: symbol() [Token { kind: Tk::TkAssign, .. }] v: expr()  { crate::root::resource::ast::Expr::MutableAssignment { name: Box::new(n), value: Box::new(v) } }

    rule closure() -> crate::root::resource::ast::Expr
        = [Token { kind: Tk::TkKwFn, .. }] args: func_args() [Token { kind: Tk::TkArr, .. }] body: expr()+ { crate::root::resource::ast::Expr::Closure { args: if args.is_some() {args.unwrap()} else {vec![]}, body: body } }

    rule r#return() -> crate::root::resource::ast::Expr
        = [Token { kind: Tk::TkKwReturn, .. }] v: expr() { crate::root::resource::ast::Expr::Return { value: Box::new(v) } }

    rule product() -> crate::root::resource::ast::Expr
        = l:sum() [Token { kind: Tk::TkStar, .. }] r:sum()  { crate::root::resource::ast::Expr::BinMul { l: Box::new(l), r: Box::new(r) } }
        / l:sum() [Token { kind: Tk::TkSlash, .. }] r:sum()  { crate::root::resource::ast::Expr::BinDiv { l: Box::new(l), r: Box::new(r) } }
        / sum()

    rule sum() -> crate::root::resource::ast::Expr
        = l:atom() [Token { kind: Tk::TkPlus, .. }] r:atom() { crate::root::resource::ast::Expr::BinAdd { l: Box::new(l), r: Box::new(r) } }
       / l:atom() [Token { kind: Tk::TkMinus, .. }] r:atom() { crate::root::resource::ast::Expr::BinSub { l: Box::new(l), r: Box::new(r) } }
       / atom()

    rule atom() -> crate::root::resource::ast::Expr
        = intrinsic()
        / call()
        / symbol()
        / group()

    rule intrinsic() -> crate::root::resource::ast::Expr
        = [Token { kind: Tk::TkFlt, lit: n }] { crate::root::resource::ast::Expr::Flt(n.parse().unwrap() )}
        / [Token { kind: Tk::TkInt, lit: n }] { crate::root::resource::ast::Expr::Int(n.parse().unwrap() )}
        / [Token { kind: Tk::TkStrLit, lit: n }] { crate::root::resource::ast::Expr::Str(n.parse().unwrap() )}
        / [Token { kind: Tk::TkFalse, lit: n }] { crate::root::resource::ast::Expr::Bool(false)}
        / [Token { kind: Tk::TkTrue, lit: n }] { crate::root::resource::ast::Expr::Bool(true)}

    rule call() -> crate::root::resource::ast::Expr
        = name: symbol() [Token { kind: Tk::TkLparen, .. }] args: call_list() [Token { kind: Tk::TkRparen, .. }] { crate::root::resource::ast::Expr::Call { name: Box::new(name), args: args, namespace: vec![]}}
        / parent: namespace() [Token { kind: Tk::TkLparen, .. }] args: call_list() [Token { kind: Tk::TkRparen, .. }] { crate::root::resource::ast::Expr::Call { name: Box::new(parent.get(0).unwrap().clone()), args: args, namespace: parent}}

    rule call_list() -> Vec<crate::root::resource::ast::Expr>
        = expr() ** [Token { kind: Tk::TkComma, .. }]

    rule symbol() -> crate::root::resource::ast::Expr
        = simplesymbol()
    
    rule simplesymbol() -> crate::root::resource::ast::Expr 
        = [Token { kind: Tk::TkSymbol, lit: n }] { crate::root::resource::ast::Expr::Symbol(n.to_string())}

    rule namespace() -> Vec<crate::root::resource::ast::Expr>
        = simplesymbol() ** [Token { kind: Tk::TkDot, .. }]

    rule group() -> crate::root::resource::ast::Expr
        = [Token { kind: Tk::TkLparen, .. }] v:product() [Token { kind: Tk::TkRparen, .. }] { v }

});

use lang::program;

use crate::root::resource::{ast::Module, tk::{Tk, Token}};

pub fn parse(tokens: Vec<Token>) -> Module {
    let p = program(&SliceByRef(&tokens[..])).unwrap();
    p
}
