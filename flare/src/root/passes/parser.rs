
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
    pub rule module(modname: &String) -> crate::root::resource::cst::Cst
        = a: clause()* {crate::root::resource::cst::Cst::Module {body: a, name: modname.clone()}}

    rule clause() -> crate::root::resource::cst::Cst
        = let_func()
        / defblock()
        / typedef()
        / with_clause()
        / extern_clause()

    rule defblock_clause() -> crate::root::resource::cst::Cst
        = [Token { kind: Tk::TkKwIn(_), .. }] f: funcdef() {f}
        / typedef()
        / with_clause()
        / extern_clause()
        // prop_def()

    rule typedef() -> crate::root::resource::cst::Cst
        = quiet! {[Token { kind: Tk::TkKwType(_), .. }] name: simplesymbol() t: generic_brackets()? [Token { kind: Tk::TkAssign(_), .. }] v: typedef_choice(name.get_symbol_name().unwrap().to_string())  {v}}
        / expected!("a 'type' clause")

    rule typedef_choice(name: String) -> crate::root::resource::cst::Cst
        = structdef(name.clone())
        / enumdef(name.clone())
        / type_alias(name.clone())

    rule structdef(name: String) -> crate::root::resource::cst::Cst
        = [Token { kind: Tk::TkKwStruct(_), .. }] [Token { kind: Tk::TkKwOf(_), .. }] m: memberlist(name.clone())  { crate::root::resource::cst::Cst::Struct { name: name.to_string(), members: m }}

    rule memberlist(parent: String) -> Vec<(String, crate::root::resource::cst::SymbolType)>
        = v: ([Token { kind: Tk::TkSymbol(_), lit: name,.. }] [Token { kind: Tk::TkColon(_), .. }] a: atype() {(name.to_string(), a)}) ** [Token { kind: Tk::TkComma(_), .. }]

    rule enumdef(name: String) -> crate::root::resource::cst::Cst
        = [Token { kind: Tk::TkKwEnum(_), .. }] [Token { kind: Tk::TkKwOf(_), .. }] m: variantlist() { crate::root::resource::cst::Cst::Enum { name: name.to_string(), members: m}}

    rule variantlist() -> Vec<(String, Vec<crate::root::resource::cst::SymbolType>)>
        = v: ([Token { kind: Tk::TkSymbol(_), lit: name,.. }] a: varianttype()? {(name.to_string(), if a.is_some() {a.unwrap()} else {vec![]})}) ** [Token { kind: Tk::TkComma(_), .. }]

    rule varianttype() -> Vec<crate::root::resource::cst::SymbolType>
        = [Token { kind: Tk::TkLbrace(_), .. }] a: atype() ** [Token { kind: Tk::TkComma(_), .. }] [Token { kind: Tk::TkRbrace(_), .. }] {a}

    rule type_alias(name: String) -> crate::root::resource::cst::Cst
        = a: atype() {crate::root::resource::cst::Cst::TypeAlias{name, is: a}}

    rule with_clause() -> crate::root::resource::cst::Cst
        = quiet! {[Token { kind: Tk::TkKwWith(_), .. }] s: expr()  {crate::root::resource::cst::Cst::WithClause { include: s }}}
        / expected!("a 'with' clause")

    rule extern_clause() -> crate::root::resource::cst::Cst
        = quiet! {[Token { kind: Tk::TkKwExtern(_), .. }] name: simplesymbol() [Token { kind: Tk::TkColon(_), .. }] [Token { kind: Tk::TkLparen(_), .. }] args: atype() ** [Token { kind: Tk::TkComma(_), .. }] [Token { kind: Tk::TkRparen(_), .. }] [Token { kind: Tk::TkArr(_), .. }] ret: atype() effect: do_effect() {crate::root::resource::cst::Cst::ExternClause {name: name.get_symbol_name().unwrap(), args, variadic: false, ret, effect}}}
        / quiet! {[Token { kind: Tk::TkKwExtern(_), .. }] name: simplesymbol() [Token { kind: Tk::TkColon(_), .. }] [Token { kind: Tk::TkLparen(_), .. }] args: atype() ** [Token { kind: Tk::TkComma(_), .. }] [Token { kind: Tk::TkComma(_), .. }] [Token { kind: Tk::TkTripleDot(_), .. }] [Token { kind: Tk::TkRparen(_), .. }] [Token { kind: Tk::TkArr(_), .. }] ret: atype() effect: do_effect() {crate::root::resource::cst::Cst::ExternClause {name: name.get_symbol_name().unwrap(), args, variadic: true, ret, effect}}}

        / expected!("an 'extern' clause")

    rule let_func() -> crate::root::resource::cst::Cst
        = quiet! {[Token { kind: Tk::TkKwLet(_), .. }] f: funcdef() {f}}
        / expected!("a 'let' clause")

    rule funcdef() -> crate::root::resource::cst::Cst
        = name: simplesymbol() [Token { kind: Tk::TkColon(_), .. }] args: func_args() r: func_ret_type() limits: where_limit() effect: do_effect() [Token { kind: Tk::TkAssign(_), .. }] start: position!() body: expr() { crate::root::resource::cst::Cst::FnDef { name: name.get_symbol_name().unwrap().to_string(), args, rettype: r, limits, effect, body } }
        // [Token { kind: Tk::TkKwLet(_), .. }] name: simplesymbol() [Token { kind: Tk::TkAssign(_), .. }] body: expr()+ { crate::root::resource::ast::Ast::FnDef { name: name.get_symbol_name().to_string(), args: vec![], rettype: crate::root::resource::ast::SymbolType::Naught, limits: None, body } }

    rule func_ret_type() -> crate::root::resource::cst::SymbolType
        = [Token { kind: Tk::TkArr(_), .. }] r: atype() {r}

    rule func_args() -> Vec<(String, crate::root::resource::cst::SymbolType)>
        =  ([Token { kind: Tk::TkLparen(_), .. }] l: func_args_list() [Token { kind: Tk::TkRparen(_), .. }] {l})

    rule where_limit() -> Option<Vec<crate::root::resource::cst::Expr>> =
        (([Token { kind: Tk::TkKwWhere(_), .. }] e: expr() {e}) ** [Token { kind: Tk::TkComma(_), .. }])?

    rule do_effect() -> Option<crate::root::resource::cst::Expr> =
        ([Token { kind: Tk::TkKwDo(_), .. }] s: simplesymbol() {s})?

    rule func_args_list() -> Vec<(String, crate::root::resource::cst::SymbolType)>
        = a: (type_arg() / [Token { kind: Tk::TkKwSelf(_), .. }]{ ("self".to_string(), crate::root::resource::cst::SymbolType::Selff)}) ** [Token { kind: Tk::TkComma(_), .. }] {a}

    rule type_arg() -> (String, crate::root::resource::cst::SymbolType)
        = t: symbol() k: arg_type() start: position!() {return (t.get_symbol_name().unwrap(), k)}

    rule arg_type() -> crate::root::resource::cst::SymbolType
        = [Token { kind: Tk::TkColon(_), .. }] k: atype() {k}

    rule defblock() -> crate::root::resource::cst::Cst
        = quiet! {[Token { kind: Tk::TkKwDef(_), .. }] item: atype() m: defblock_clause()*  {crate::root::resource::cst::Cst::DefBlock {name: item, funcs: m}}}
        / expected!("a 'def-in' clause")

    rule prop_def() -> crate::root::resource::cst::Cst
        = [Token { kind: Tk::TkKwProp(_), .. }] n: simplesymbol() [Token { kind: Tk::TkKwFor(_), .. }] t: atype() [Token { kind: Tk::TkAssign(_), .. }] f: fn_sig() ** [Token { kind: Tk::TkComma(_), .. }] {crate::root::resource::cst::Cst::Propdef { p: crate::root::resource::cst::Property {name: n.get_symbol_name().unwrap(), req: f}  }}

    rule fn_sig() -> crate::root::resource::cst::FnSignature
        = n: simplesymbol() args: sig_args() r: func_ret_type()? limits: where_limit() {crate::root::resource::cst::FnSignature {name: n.get_symbol_name().unwrap(), args, rettype: if r.is_some() {r.unwrap()} else {crate::root::resource::cst::SymbolType::Generic(crate::root::passes::midend::environment::GenericValue::Ref(format!("?_{}", crate::root::resource::cst::calculate_hash::<String>(&n.get_symbol_name().unwrap()) )))}, limits}}

    rule sig_args() -> Vec<crate::root::resource::cst::SymbolType>
        = a: ([Token { kind: Tk::TkColon(_), .. }] l: sig_arg_list() {l})? {if a.is_some() {a.unwrap()} else {vec![]}}

    rule sig_arg_list() -> Vec<(crate::root::resource::cst::SymbolType)>
        = a: sig_arg_type() ** [Token { kind: Tk::TkComma(_), .. }] {a}

    rule sig_arg_type() -> crate::root::resource::cst::SymbolType
        = k: atype() start: position!() {k}

    rule expr() -> crate::root::resource::cst::Expr
        = assignment()
        / closure()
        / ifexpr()
        / matchexpr()
        / binary_op()

    rule assignment() -> crate::root::resource::cst::Expr
        = quiet! {n: simplesymbol() [Token { kind: Tk::TkAssign(_), .. }] v: expr()  a: expr() { crate::root::resource::cst::Expr::Assignment { name: Box::new(n), value: Box::new(v), and_in: Box::new(a) } }}
        / expected!("assignment")

    rule closure() -> crate::root::resource::cst::Expr
        = quiet! {[Token { kind: Tk::TkKwFn(_), .. }] args: func_args() [Token { kind: Tk::TkArr(_), .. }] body: expr()+ { crate::root::resource::cst::Expr::Closure { args, body } }}
        / expected!("a closure")

    rule structinstance() -> crate::root::resource::cst::Expr
        = n: simplesymbol() [Token { kind: Tk::TkLbrace(_), .. }] f: fieldinit() ** [Token { kind: Tk::TkComma(_), .. }] [Token { kind: Tk::TkRbrace(_), .. }] {crate::root::resource::cst::Expr::StructInstance { name: Box::new(n), fields: f }}

    rule fieldinit() -> (String, crate::root::resource::cst::Expr)
        = n: simplesymbol() [Token { kind: Tk::TkColon(_), .. }] e: expr() {(n.get_symbol_name().unwrap(), e)}

    rule variantinstance() -> crate::root::resource::cst::Expr
        = [Token { kind: Tk::TkColon(_), .. }] n: simplesymbol() f: variantfields()? {crate::root::resource::cst::Expr::VariantInstance { name: Box::new(n), fields: f.unwrap_or_else(std::vec::Vec::new) }}

    rule variantfields() -> Vec<crate::root::resource::cst::Expr>
        = [Token { kind: Tk::TkLbrace(_), .. }] e: expr() ** [Token { kind: Tk::TkComma(_), .. }] [Token { kind: Tk::TkRbrace(_), .. }] {e}


    rule binary_op() -> crate::root::resource::cst::Expr =
    quiet! {precedence!{


        l: (@) op: [Token { kind: Tk::TkFuncComp(_), .. }] r: @ {crate::root::resource::cst::Expr::SeqComp { l: Box::new(l), r: Box::new(r) }}
        --
        l: (@) op: [Token { kind: Tk::TkKwIs(_), .. }] r: @ {crate::root::resource::cst::Expr::Logical { l: Box::new(l), op: crate::root::resource::cst::LogicOp::Is, r: Box::new(r) }}
        l: (@) op: [Token { kind: Tk::TkKwOr(_), .. }] r: @ {crate::root::resource::cst::Expr::Logical { l: Box::new(l), op: crate::root::resource::cst::LogicOp::Is, r: Box::new(r) }}
        l: (@) op: [Token { kind: Tk::TkKwAnd(_), .. }] r: @ {crate::root::resource::cst::Expr::Logical { l: Box::new(l), op: crate::root::resource::cst::LogicOp::Is, r: Box::new(r) }}

        l: (@) op: [Token { kind: Tk::TkCEQ(_), .. }] r: @ {crate::root::resource::cst::Expr::Logical { l: Box::new(l), op: crate::root::resource::cst::LogicOp::CEQ, r: Box::new(r) }}
        l: (@) op: [Token { kind: Tk::TkCLT(_), .. }] r: @ {crate::root::resource::cst::Expr::Logical { l: Box::new(l), op: crate::root::resource::cst::LogicOp::CLT, r: Box::new(r) }}
        l: (@) op: [Token { kind: Tk::TkCLE(_), .. }] r: @ {crate::root::resource::cst::Expr::Logical { l: Box::new(l), op: crate::root::resource::cst::LogicOp::CLE, r: Box::new(r) }}
        l: (@) op: [Token { kind: Tk::TkCGT(_), .. }] r: @ {crate::root::resource::cst::Expr::Logical { l: Box::new(l), op: crate::root::resource::cst::LogicOp::CGT, r: Box::new(r) }}
        l: (@) op: [Token { kind: Tk::TkCGE(_), .. }] r: @ {crate::root::resource::cst::Expr::Logical { l: Box::new(l), op: crate::root::resource::cst::LogicOp::CGE, r: Box::new(r) }}
        --
        l: (@) [Token { kind: Tk::TkPlus(_), .. }] r: @  { crate::root::resource::cst::Expr::BinAdd { l: Box::new(l), r: Box::new(r) } }
        l: (@) [Token { kind: Tk::TkMinus(_), .. }] r: @  { crate::root::resource::cst::Expr::BinSub { l: Box::new(l), r: Box::new(r) } }
        --
        l: (@) [Token { kind: Tk::TkStar(_), .. }] r: @  { crate::root::resource::cst::Expr::BinMul { l: Box::new(l), r: Box::new(r) } }
        l: (@) [Token { kind: Tk::TkSlash(_), .. }] r: @  { crate::root::resource::cst::Expr::BinDiv { l: Box::new(l), r: Box::new(r) } }
        --
        a: atom() {a}
        --
       // l: (@) op: [Token { kind: Tk::TkDot(_), .. }] r: @ {crate::root::resource::cst::Expr::FieldAccess(Box::new(l), Box::new(r))}
        l: (@) op: [Token { kind: Tk::TkDot(_), .. }] r: @ {crate::root::resource::cst::Expr::Path(Box::new(l), Box::new(r))}

        //l: (@) op: [Token { kind: Tk::TkDoubleColon(_), .. }] r: @ {crate::root::resource::cst::Expr::Path(Box::new(l), Box::new(r))}
    }}
    / expected!("an arithmetic or comparison operator")


    //#[cache_left_rec]
    rule atom() -> crate::root::resource::cst::Expr
        = intrinsic()
        / call()
        / variantinstance()
        / structinstance()

        / symbol()
        / group()

    rule ifexpr() -> crate::root::resource::cst::Expr
        = quiet! {[Token { kind: Tk::TkKwIf(_), .. }] c: expr() [Token { kind: Tk::TkKwThen(_), .. }] t: expr() e: else_branch() {crate::root::resource::cst::Expr::If { condition: Box::new(c), then: Box::new(t), otherwise: e }}}
        / expected!("an if/else expression")

    rule else_branch() -> Box<crate::root::resource::cst::Expr>
        = quiet! {([Token { kind: Tk::TkKwElse(_), .. }] o: expr() {Box::new(o)})}
        / expected!("an else clause")

    rule matchexpr() -> crate::root::resource::cst::Expr
        = quiet! {[Token { kind: Tk::TkKwMatch(_), .. }] e: expr() arms: match_arm()+ {crate::root::resource::cst::Expr::Match { matchee: Box::new(e), arms }}}
        / expected!("a match expression")

    rule match_arm() -> (crate::root::resource::cst::Predicate, crate::root::resource::cst::Expr)
        = [Token { kind: Tk::TkPipe(_), .. }] p: predicate() [Token { kind: Tk::TkKwThen(_), .. }] e: expr() {(p, e)}
        / [Token { kind: Tk::TkKwElse(_), .. }] e: expr() {(crate::root::resource::cst::Predicate::Wildcard, e)}


    rule predicate() -> crate::root::resource::cst::Predicate
        = destructuring_predicate()
/*
    rule logic_predicate() -> crate::root::resource::cst::Predicate
        = quiet! {precedence! {
            op: [Token { kind: Tk::TkCEQ(_), .. }] r: expr() {crate::root::resource::cst::Predicate::Comparison { op: crate::root::resource::cst::LogicOp::CEQ, rhs: Box::new(r) }}
            op: [Token { kind: Tk::TkCLT(_), .. }] r: expr() {crate::root::resource::cst::Predicate::Comparison { op: crate::root::resource::cst::LogicOp::CLT, rhs: Box::new(r) }}
            op: [Token { kind: Tk::TkCLE(_), .. }] r: expr() {crate::root::resource::cst::Predicate::Comparison { op: crate::root::resource::cst::LogicOp::CLE, rhs: Box::new(r) }}
            op: [Token { kind: Tk::TkCGT(_), .. }] r: expr() {crate::root::resource::cst::Predicate::Comparison { op: crate::root::resource::cst::LogicOp::CGT, rhs: Box::new(r) }}
            op: [Token { kind: Tk::TkCGE(_), .. }] r: expr() {crate::root::resource::cst::Predicate::Comparison { op: crate::root::resource::cst::LogicOp::CGE, rhs: Box::new(r) }}
        }}
        / expected!("a predicate")
    */
    rule destructuring_predicate() -> crate::root::resource::cst::Predicate
        = quiet! {
            v: variantinstance() {crate::root::resource::cst::Predicate::Variant { name: v.get_variant_name(), membervars: v.get_variant_fields() }}
        }
        / expected!("a predicate")


    rule intrinsic() -> crate::root::resource::cst::Expr
        = quiet! {[Token { kind: Tk::TkFlt(_), lit: n,.. }] { crate::root::resource::cst::Expr::Flt(n.parse().unwrap() )}}
        / quiet! {[Token { kind: Tk::TkInt(_), lit: n,.. }] { crate::root::resource::cst::Expr::Int(n.parse().unwrap() )}}
        / quiet! {[Token { kind: Tk::TkStrLit(_), lit: n,.. }] { crate::root::resource::cst::Expr::Str(n.parse().unwrap() )}}
        / quiet! {[Token { kind: Tk::TkFalse(_), lit: n,.. }] { crate::root::resource::cst::Expr::Bool(false)}}
        / quiet! {[Token { kind: Tk::TkTrue(_), lit: n,.. }] { crate::root::resource::cst::Expr::Bool(true)}}
        / quiet! {[Token { kind: Tk::TkKwUnit(_), lit: n,.. }] { crate::root::resource::cst::Expr::Naught}}
        / quiet! {[Token { kind: Tk::TkPtrInit(_), .. }] e: expr() {crate::root::resource::cst::Expr::AddressOf(Box::new(e))}}
        / expected!("a value")

    //#[cache_left_rec]
    rule call() -> crate::root::resource::cst::Expr
        = quiet! {primary: simplesymbol() r: call_suffix(primary) {r}}
        / expected!("a function call")

rule call_suffix(lhs: crate::root::resource::cst::Expr) -> crate::root::resource::cst::Expr
    = [Token { kind: Tk::TkDot(_), .. }] name: simplesymbol() &[Token { kind: Tk::TkLparen(_), .. }] [Token { kind: Tk::TkLparen(_), .. }] args: call_list() [Token { kind: Tk::TkRparen(_), .. }] r: call_suffix(crate::root::resource::cst::Expr::MethodCall { obj: Box::new(lhs.clone()), name: Box::new(name), args}) {r}
    / [Token { kind: Tk::TkLparen(_), .. }] args: call_list() [Token { kind: Tk::TkRparen(_), .. }] r: call_suffix(crate::root::resource::cst::Expr::Call { name: Box::new(lhs.clone()), args}) {r}

    / { lhs }  // Default case: no more calls

    // rule call() -> crate::root::resource::ast::Expr
    //     = name: atom() [Token { kind: Tk::TkLparen, .. }] args: call_list() [Token { kind: Tk::TkRparen, .. }] { crate::root::resource::ast::Expr::Call { name: Box::new(name.clone()), args}}
    //     / name: simplesymbol() [Token { kind: Tk::TkDoubleColon, .. }]  method: simplesymbol() [Token { kind: Tk::TkLparen, .. }] args: call_list() [Token { kind: Tk::TkRparen, .. }] { crate::root::resource::ast::Expr::ModuleCall {module: Box::new(name.clone()), name: Box::new(method.clone()), args}}


    rule call_list() -> Vec<crate::root::resource::cst::Expr>
        = expr() ** [Token { kind: Tk::TkComma(_), .. }] //{ expr }

    // rule field_access() -> crate::root::resource::ast::Expr
    //     = n: simplesymbol() [Token { kind: Tk::TkDot(_), .. }] s: symbol() {crate::root::resource::ast::Expr::FieldAccess(Box::new(n), s.get_symbol_name())}

    rule symbol() -> crate::root::resource::cst::Expr
        // quiet! {f: field_access() {f}}
        = quiet! {s: simplesymbol() {s}}
        / quiet! {[Token { kind: Tk::TkKwSelf(_),.. }] {crate::root::resource::cst::Expr::Selff}}

        / expected!("an identifier or self")

    rule simplesymbol() -> crate::root::resource::cst::Expr
        = quiet! {[Token { kind: Tk::TkSymbol(_), lit: n,.. }] { crate::root::resource::cst::Expr::Symbol(n.to_string())}}
        / expected!("an identifier")

    rule atype() -> crate::root::resource::cst::SymbolType
        = [Token { kind: Tk::TkKwInt(_), .. }] {crate::root::resource::cst::SymbolType::Int}
        / [Token { kind: Tk::TkKwUsize(_), .. }] {crate::root::resource::cst::SymbolType::Usize}
        / [Token { kind: Tk::TkKwWord(_), .. }] {crate::root::resource::cst::SymbolType::Word}
        / [Token { kind: Tk::TkKwByte(_), .. }] {crate::root::resource::cst::SymbolType::Byte}
        / [Token { kind: Tk::TkKwFlt(_), .. }] {crate::root::resource::cst::SymbolType::Flt}
        / [Token { kind: Tk::TkKwStr(_), .. }] {crate::root::resource::cst::SymbolType::Str}
        / [Token { kind: Tk::TkKwChar(_), .. }] {crate::root::resource::cst::SymbolType::Char}
        / [Token { kind: Tk::TkKwBool(_), .. }] {crate::root::resource::cst::SymbolType::Bool}
        / [Token { kind: Tk::TkKwFnTy(_), .. }] {crate::root::resource::cst::SymbolType::Fn(vec![], crate::root::resource::cst::SymbolType::Unknown.into())}
        / [Token { kind: Tk::TkKwUnit(_), .. }] {crate::root::resource::cst::SymbolType::Unit}
        / s: symbol() a: (generic_brackets())? {crate::root::resource::cst::SymbolType::Custom(s.get_symbol_name().unwrap(), if a.is_some() {a.unwrap()} else {vec![]})}
        / [Token { kind: Tk::TkQuestion(_), .. }] s: simplesymbol() start:position!() {crate::root::resource::cst::SymbolType::Generic(crate::root::passes::midend::environment::GenericValue::Ref(format!("?_{}", s.get_symbol_name().unwrap())))}
        / [Token { kind: Tk::TkPtrArr(_), .. }] t: atype() {crate::root::resource::cst::SymbolType::Pointer(Box::new(t))}
        / expected!("a type")

    rule generic_brackets() -> Vec<crate::root::resource::cst::SymbolType>
        = [Token { kind: Tk::TkLbracket(_), .. }] a: atype() ** [Token { kind: Tk::TkComma(_), .. }] [Token { kind: Tk::TkRbracket(_), .. }]{a}

    rule group() -> crate::root::resource::cst::Expr
        = quiet! { [Token { kind: Tk::TkLparen(_), .. }] v:expr() [Token { kind: Tk::TkRparen(_), .. }] { v } }
        / expected!("a grouping expression")

});

use lang::module;

use crate::root::resource::{
    cst::Cst,
    errors::{ParsingError, ParsingFailError},
    tk::{Tk, Token},
};

pub fn parse(tokens: &[Token], modname: &String) -> anyhow::Result<Cst, ParsingError> {
    let p = module(&SliceByRef(tokens), modname);
    match p {
        Ok(file_module) => Ok(file_module),
        Err(e) => {
            Err(ParsingError::ParseFail(ParsingFailError {modulename: modname.to_string(), source: e}))
        }
    }
}
