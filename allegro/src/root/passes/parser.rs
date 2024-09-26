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
        // prop_def()

    rule typedef() -> crate::root::resource::ast::Ast
        = [Token { kind: Tk::TkKwType, .. }] [Token { kind: Tk::TkSymbol, lit: name,.. }] t: generic_brackets()? [Token { kind: Tk::TkAssign, .. }] v: typedef_choice(name.to_string()) {v}

    rule typedef_choice(name: String) -> crate::root::resource::ast::Ast
        = structdef(name.clone())
        / enumdef(name.clone())
        / type_alias(name.clone())

    rule structdef(name: String) -> crate::root::resource::ast::Ast
        = [Token { kind: Tk::TkKwStruct, .. }] [Token { kind: Tk::TkKwOf, .. }] m: memberlist(name.clone()) { crate::root::resource::ast::Ast::Struct { name: name.to_string(), members: m.into() }}

    rule memberlist(parent: String) -> Vec<(String, crate::root::resource::ast::SymbolType)>
        = v: ([Token { kind: Tk::TkSymbol, lit: name,.. }] [Token { kind: Tk::TkColon, .. }] a: atype() {(format!("$_{}", name.to_string()), a)}) ** [Token { kind: Tk::TkComma, .. }]

    rule enumdef(name: String) -> crate::root::resource::ast::Ast
        = [Token { kind: Tk::TkKwEnum, .. }] [Token { kind: Tk::TkKwOf, .. }] m: variantlist() { crate::root::resource::ast::Ast::Enum { name: name.to_string(), members: m.into() }}

    rule variantlist() -> Vec<(crate::root::resource::ast::SymbolType)>
        = v: ([Token { kind: Tk::TkSymbol, lit: name,.. }] a: varianttype()? {crate::root::resource::ast::SymbolType::Variant(name.to_string(), if a.is_some() {a.unwrap().into()} else {vec![].into()})}) ** [Token { kind: Tk::TkComma, .. }]

    rule varianttype() -> Vec<crate::root::resource::ast::SymbolType>
        = [Token { kind: Tk::TkLparen, .. }] a: atype() ** [Token { kind: Tk::TkComma, .. }] [Token { kind: Tk::TkRparen, .. }] {a}

    rule type_alias(name: String) -> crate::root::resource::ast::Ast
        = a: atype() {crate::root::resource::ast::Ast::TypeAlias{name, is: a}}

    rule defblock() -> crate::root::resource::ast::Ast
        = [Token { kind: Tk::TkKwDef, .. }] name: atype() [Token { kind: Tk::TkAssign, .. }] f: methoddef(name.get_custom_name()) * [Token { kind: Tk::TkKwEnd, .. }] { crate::root::resource::ast::Ast::TypeDef { name: name, funcs: f.into() }}

    rule methoddef(parent: String) -> crate::root::resource::ast::Ast
        = f: funcdef() {f.convert_fn_to_methodfn(parent)}

    rule with_clause() -> crate::root::resource::ast::Ast
        = [Token { kind: Tk::TkKwWith, .. }] s: expr() ** [Token { kind: Tk::TkComma, .. }] {crate::root::resource::ast::Ast::WithClause { include: s }}

    rule funcdef() -> crate::root::resource::ast::Ast
        = [Token { kind: Tk::TkKwLet, .. }] [Token { kind: Tk::TkSymbol, lit: name,.. }] args: func_args() r: func_ret_type()? limits: where_limit() [Token { kind: Tk::TkAssign, .. }] start: position!() body: expr()+ { crate::root::resource::ast::Ast::FnDef { name: name.to_string(), args: if args.is_some() {args.unwrap().into()} else {vec![].into()}, rettype: if r.is_some() {r.unwrap()} else {crate::root::resource::ast::SymbolType::Generic(format!("?_{}", crate::root::resource::ast::calculate_hash::<String>(&name) ))}, limits, body } }
        / [Token { kind: Tk::TkKwLet, .. }] [Token { kind: Tk::TkSymbol, lit: name,.. }] [Token { kind: Tk::TkAssign, .. }] body: expr()+ { crate::root::resource::ast::Ast::FnDef { name: name.to_string(), args: vec![].into(), rettype: crate::root::resource::ast::SymbolType::Naught, limits: None, body } }

    rule func_ret_type() -> crate::root::resource::ast::SymbolType
        = [Token { kind: Tk::TkArr, .. }] r: atype() {r}

    rule func_args() -> Option<Vec<(String, crate::root::resource::ast::SymbolType)>> 
        = ([Token { kind: Tk::TkKwOf, .. }] l: func_args_list() {l})?

    rule where_limit() -> Option<Vec<crate::root::resource::ast::FnArgLimit>> =
        (([Token { kind: Tk::TkKwWhere, .. }] n: symbol() [Token { kind: Tk::TkKwIs, .. }] l: atype() {crate::root::resource::ast::FnArgLimit {name: n.get_symbol_name().clone(), limit: l}}) ** [Token { kind: Tk::TkComma, .. }])?

    rule func_args_list() -> Vec<(String, crate::root::resource::ast::SymbolType)>
        = a: type_arg() ** [Token { kind: Tk::TkComma, .. }] {a}

    rule type_arg() -> (String, crate::root::resource::ast::SymbolType)
        = m: [Token { kind: Tk::TkKwMut, .. }]? t: symbol() k: arg_type()? start: position!() {let r = if k.is_some()  {k.unwrap()} else {crate::root::resource::ast::SymbolType::Generic(format!("?_{}", crate::root::resource::ast::calculate_hash::<String>(&t.get_symbol_name()) ))}; if m.is_some() {return (t.get_symbol_name(), crate::root::resource::ast::SymbolType::Mut(Box::new(r)))} else {return (t.get_symbol_name(), r)}}
    
    rule arg_type() -> crate::root::resource::ast::SymbolType
        = [Token { kind: Tk::TkColon, .. }] k: atype() {k}

    rule prop_def() -> crate::root::resource::ast::Ast 
        = [Token { kind: Tk::TkKwProp, .. }] n: symbol() [Token { kind: Tk::TkKwFor, .. }] t: atype() [Token { kind: Tk::TkAssign, .. }] f: fn_sig() ** [Token { kind: Tk::TkComma, .. }] {crate::root::resource::ast::Ast::Propdef { p: crate::root::resource::ast::Property {name: n.get_symbol_name(), req: f}  }}

    rule fn_sig() -> crate::root::resource::ast::FnSignature
        = n: symbol() args: sig_args() r: func_ret_type()? limits: where_limit() {crate::root::resource::ast::FnSignature {name: n.get_symbol_name(), args: args.into(), rettype: if r.is_some() {r.unwrap()} else {crate::root::resource::ast::SymbolType::Generic(format!("?_{}", crate::root::resource::ast::calculate_hash::<String>(&n.get_symbol_name()) ))}, limits}}

    rule sig_args() -> Vec<crate::root::resource::ast::SymbolType>
        = a: ([Token { kind: Tk::TkKwOf, .. }] l: sig_arg_list() {l})? {if a.is_some() {a.unwrap()} else {vec![]}}
        
    rule sig_arg_list() -> Vec<(crate::root::resource::ast::SymbolType)>
        = a: sig_arg_type() ** [Token { kind: Tk::TkComma, .. }] {a}

    rule sig_arg_type() -> crate::root::resource::ast::SymbolType
        = k: atype() start: position!() {k}

    rule expr() -> crate::root::resource::ast::Expr
        = assignment()
        / closure()
        / structinstance()
        / ifexpr()
        // field_access()
        / matchexpr()
        / r#return()
        / binary_op()

    rule assignment() -> crate::root::resource::ast::Expr
        = n: symbol() [Token { kind: Tk::TkAssign, .. }] v: expr()  { crate::root::resource::ast::Expr::Assignment { name: Box::new(n), value: Box::new(v) } }
        / [Token { kind: Tk::TkKwMut, .. }] n: symbol() [Token { kind: Tk::TkAssign, .. }] v: expr()  { crate::root::resource::ast::Expr::MutableAssignment { name: Box::new(n), value: Box::new(v) } }
        / n: field_access() [Token { kind: Tk::TkAssign, .. }] v: expr()  { crate::root::resource::ast::Expr::MutableAssignment { name: Box::new(n), value: Box::new(v) } }
        / [Token { kind: Tk::TkKwMut, .. }] n: field_access() [Token { kind: Tk::TkAssign, .. }] v: expr()  { crate::root::resource::ast::Expr::MutableAssignment { name: Box::new(n), value: Box::new(v) } }
    
    rule closure() -> crate::root::resource::ast::Expr
        = [Token { kind: Tk::TkKwFn, .. }] args: func_args() [Token { kind: Tk::TkArr, .. }] body: expr()+ { crate::root::resource::ast::Expr::Closure { args: if args.is_some() {args.unwrap()} else {vec![]}, body } }

    rule r#return() -> crate::root::resource::ast::Expr
        = [Token { kind: Tk::TkKwReturn, .. }] v: expr() { crate::root::resource::ast::Expr::Return { value: Box::new(v) } }

    rule binary_op() -> crate::root::resource::ast::Expr = precedence!{
        l: (@) op: [Token { kind: Tk::TkFuncComp, .. }] r: @ {crate::root::resource::ast::Expr::Composition { l: Box::new(l), r: Box::new(r) }}
        --
        l: (@) op: [Token { kind: Tk::TkKwIs, .. }] r: @ {crate::root::resource::ast::Expr::Logical { l: Box::new(l), op: crate::root::resource::ast::LogicOp::Is, r: Box::new(r) }}
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
        / field_access()
        / call()
        / symbol()
        / group()

    rule ifexpr() -> crate::root::resource::ast::Expr
        = [Token { kind: Tk::TkKwIf, .. }] c: expr() [Token { kind: Tk::TkKwThen, .. }] t: expr() e: else_branch() {crate::root::resource::ast::Expr::If { condition: Box::new(c), then: Box::new(t), otherwise: e }}

    rule else_branch() -> Box<crate::root::resource::ast::Expr>
        = ([Token { kind: Tk::TkKwElse, .. }] o: expr() {Box::new(o)})

    rule matchexpr() -> crate::root::resource::ast::Expr
        = [Token { kind: Tk::TkKwMatch, .. }] e: expr() [Token { kind: Tk::TkAssign, .. }] match_arm()+ {e}

    rule match_arm() -> (crate::root::resource::ast::Predicate, crate::root::resource::ast::Expr)
        = p: predicate()  e: expr() {(p, e)}

    rule predicate() -> crate::root::resource::ast::Predicate
        = logic_predicate()
    
    rule logic_predicate() -> crate::root::resource::ast::Predicate
        = precedence! {
            op: [Token { kind: Tk::TkCEQ, .. }] r: expr() [Token { kind: Tk::TkKwThen, .. }] {crate::root::resource::ast::Predicate::Comparison { op: crate::root::resource::ast::LogicOp::CEQ, rhs: Box::new(r) }}
            op: [Token { kind: Tk::TkCLT, .. }] r: expr() [Token { kind: Tk::TkKwThen, .. }] {crate::root::resource::ast::Predicate::Comparison { op: crate::root::resource::ast::LogicOp::CLT, rhs: Box::new(r) }}
            op: [Token { kind: Tk::TkCLE, .. }] r: expr() [Token { kind: Tk::TkKwThen, .. }] {crate::root::resource::ast::Predicate::Comparison { op: crate::root::resource::ast::LogicOp::CLE, rhs: Box::new(r) }}
            op: [Token { kind: Tk::TkCGT, .. }] r: expr() [Token { kind: Tk::TkKwThen, .. }] {crate::root::resource::ast::Predicate::Comparison { op: crate::root::resource::ast::LogicOp::CGT, rhs: Box::new(r) }}
            op: [Token { kind: Tk::TkCGE, .. }] r: expr() [Token { kind: Tk::TkKwThen, .. }] {crate::root::resource::ast::Predicate::Comparison { op: crate::root::resource::ast::LogicOp::CGE, rhs: Box::new(r) }}
        }

    rule intrinsic() -> crate::root::resource::ast::Expr
        = [Token { kind: Tk::TkFlt, lit: n,.. }] { crate::root::resource::ast::Expr::Flt(n.parse().unwrap() )}
        / [Token { kind: Tk::TkInt, lit: n,.. }] { crate::root::resource::ast::Expr::Int(n.parse().unwrap() )}
        / [Token { kind: Tk::TkStrLit, lit: n,.. }] { crate::root::resource::ast::Expr::Str(n.parse().unwrap() )}
        / [Token { kind: Tk::TkFalse, lit: n,.. }] { crate::root::resource::ast::Expr::Bool(false)}
        / [Token { kind: Tk::TkTrue, lit: n,.. }] { crate::root::resource::ast::Expr::Bool(true)}
        / [Token { kind: Tk::TkPtrInit, .. }] e: expr() {crate::root::resource::ast::Expr::AddressOf(Box::new(e))}

    rule call() -> crate::root::resource::ast::Expr
        = name: symbol() c: call_switch(name) {c}

    rule call_switch(name: crate::root::resource::ast::Expr) -> crate::root::resource::ast::Expr
        = [Token { kind: Tk::TkLparen, .. }] args: call_list() [Token { kind: Tk::TkRparen, .. }] { crate::root::resource::ast::Expr::Call { name: Box::new(name.clone()), args}}
        / [Token { kind: Tk::TkDot, .. }] method: symbol() [Token { kind: Tk::TkLparen, .. }] args: call_list() [Token { kind: Tk::TkRparen, .. }] call_switch(name.clone()) { crate::root::resource::ast::Expr::MethodCall {obj: Box::new(name.clone()), name: Box::new(method.clone()), args}}
        / [Token { kind: Tk::TkDoubleColon, .. }] method: symbol() [Token { kind: Tk::TkLparen, .. }] args: call_list() [Token { kind: Tk::TkRparen, .. }] call_switch(name.clone()) { crate::root::resource::ast::Expr::ModuleCall {module: Box::new(name.clone()), name: Box::new(method.clone()), args}}
        / {name.clone()}

    rule call_list() -> Vec<crate::root::resource::ast::Expr>
        = expr() ** [Token { kind: Tk::TkComma, .. }] //{ expr }

    rule structinstance() -> crate::root::resource::ast::Expr
        = n: symbol() [Token { kind: Tk::TkLbrace, .. }] f: assignment() ** [Token { kind: Tk::TkComma, .. }] [Token { kind: Tk::TkRbrace, .. }] {crate::root::resource::ast::Expr::StructInstance { name: Box::new(n), fields: f }}

    rule field_access() -> crate::root::resource::ast::Expr
        = n: simplesymbol() [Token { kind: Tk::TkColon, .. }] s: symbol() {crate::root::resource::ast::Expr::FieldAccess(Box::new(n), Box::new(s))}

    rule symbol() -> crate::root::resource::ast::Expr
        = s: simplesymbol() {s}
        // [Token { kind: Tk::TkKwSelf, .. }] {crate::root::resource::ast::Expr::Symbol("self".to_string())}

    rule simplesymbol() -> crate::root::resource::ast::Expr
        = [Token { kind: Tk::TkSymbol, lit: n,.. }] { crate::root::resource::ast::Expr::Symbol(n.to_string())}

    rule atype() -> crate::root::resource::ast::SymbolType
        = [Token { kind: Tk::TkKwInt, .. }] {crate::root::resource::ast::SymbolType::Int}
        / [Token { kind: Tk::TkKwUint, .. }] {crate::root::resource::ast::SymbolType::Uint}
        / [Token { kind: Tk::TkKwWord, .. }] {crate::root::resource::ast::SymbolType::Word}
        / [Token { kind: Tk::TkKwByte, .. }] {crate::root::resource::ast::SymbolType::Byte}
        / [Token { kind: Tk::TkKwFlt, .. }] {crate::root::resource::ast::SymbolType::Flt}
        / [Token { kind: Tk::TkKwStr, .. }] {crate::root::resource::ast::SymbolType::Str}
        / [Token { kind: Tk::TkKwChar, .. }] {crate::root::resource::ast::SymbolType::Char}
        / [Token { kind: Tk::TkKwBool, .. }] {crate::root::resource::ast::SymbolType::Bool}
        / [Token { kind: Tk::TkKwFnTy, .. }] {crate::root::resource::ast::SymbolType::Fn(vec![].into(), crate::root::resource::ast::SymbolType::Unknown.into(), false)}
        / [Token { kind: Tk::TkKwNaught, .. }] {crate::root::resource::ast::SymbolType::Naught}
        / s: symbol() a: (generic_brackets())? {crate::root::resource::ast::SymbolType::Custom(s.get_symbol_name(), if a.is_some() {a.unwrap().into()} else {vec![].into()})}
        / [Token { kind: Tk::TkQuestion, .. }] s: simplesymbol() start:position!() {crate::root::resource::ast::SymbolType::Generic(format!("?_{}", s.get_symbol_name()))}
        / [Token { kind: Tk::TkPtrArr, .. }] t: atype() {crate::root::resource::ast::SymbolType::Pointer(Box::new(t))}
    
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
