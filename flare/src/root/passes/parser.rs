use reporting::*;

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
    pub rule program() -> crate::root::resource::ast::FileModule
        = a: clause()* {crate::root::resource::ast::FileModule {body: a, name: "".to_string()}}

    rule clause() -> crate::root::resource::ast::Ast
        = funcdef()
        / methoddef()
        / typedef()
        / with_clause()
        // prop_def()

    rule typedef() -> crate::root::resource::ast::Ast
        = quiet! {[Token { kind: Tk::TkKwType(_), .. }] name: simplesymbol() t: generic_brackets()? [Token { kind: Tk::TkAssign(_), .. }] v: typedef_choice(name.get_symbol_name().to_string())  {v}}
        / expected!("a type definition")

    rule typedef_choice(name: String) -> crate::root::resource::ast::Ast
        = structdef(name.clone())
        / enumdef(name.clone())
        / type_alias(name.clone())

    rule structdef(name: String) -> crate::root::resource::ast::Ast
        = [Token { kind: Tk::TkKwStruct(_), .. }] [Token { kind: Tk::TkKwOf(_), .. }] m: memberlist(name.clone()) [Token { kind: Tk::TkKwEnd(_), .. }] { crate::root::resource::ast::Ast::Struct { name: name.to_string(), members: m }}

    rule memberlist(parent: String) -> Vec<(String, crate::root::resource::ast::SymbolType)>
        = v: ([Token { kind: Tk::TkSymbol(_), lit: name,.. }] [Token { kind: Tk::TkColon(_), .. }] a: atype() {(name.to_string(), a)}) ** [Token { kind: Tk::TkComma(_), .. }]

    rule enumdef(name: String) -> crate::root::resource::ast::Ast
        = [Token { kind: Tk::TkKwEnum(_), .. }] [Token { kind: Tk::TkKwOf(_), .. }] m: variantlist() [Token { kind: Tk::TkKwEnd(_), .. }] { crate::root::resource::ast::Ast::Enum { name: name.to_string(), members: m}}

    rule variantlist() -> Vec<(crate::root::resource::ast::SymbolType)>
        = v: ([Token { kind: Tk::TkSymbol(_), lit: name,.. }] a: varianttype()? {crate::root::resource::ast::SymbolType::Variant(name.to_string(), if a.is_some() {a.unwrap()} else {vec![]})}) ** [Token { kind: Tk::TkComma(_), .. }]

    rule varianttype() -> Vec<crate::root::resource::ast::SymbolType>
        = [Token { kind: Tk::TkLparen(_), .. }] a: atype() ** [Token { kind: Tk::TkComma(_), .. }] [Token { kind: Tk::TkRparen(_), .. }] {a}

    rule type_alias(name: String) -> crate::root::resource::ast::Ast
        = a: atype() {crate::root::resource::ast::Ast::TypeAlias{name, is: a}}

    rule with_clause() -> crate::root::resource::ast::Ast
        = quiet! {[Token { kind: Tk::TkKwWith(_), .. }] s: expr() ** [Token { kind: Tk::TkComma(_), .. }] {crate::root::resource::ast::Ast::WithClause { include: s }}}
        / expected!("a 'with' clause")

    rule funcdef() -> crate::root::resource::ast::Ast
        = [Token { kind: Tk::TkKwLet(_), .. }] name: simplesymbol() args: func_args() r: func_ret_type()? limits: where_limit() [Token { kind: Tk::TkAssign(_), .. }] start: position!() body: expr()+ { crate::root::resource::ast::Ast::FnDef { name: name.get_symbol_name().to_string(), args: if args.is_some() {args.unwrap()} else {vec![]}, rettype: if r.is_some() {r.unwrap()} else {crate::root::resource::ast::SymbolType::Generic(crate::root::passes::midend::environment::GenericValue::Ref(format!("?_{}", crate::root::resource::ast::calculate_hash::<String>(&name.get_symbol_name()) )))}, limits, body } }
        / [Token { kind: Tk::TkKwLet(_), .. }] name: simplesymbol() [Token { kind: Tk::TkAssign(_), .. }] body: expr()+ { crate::root::resource::ast::Ast::FnDef { name: name.get_symbol_name().to_string(), args: vec![], rettype: crate::root::resource::ast::SymbolType::Naught, limits: None, body } }

    rule methoddef() -> crate::root::resource::ast::Ast
        = quiet!{[Token { kind: Tk::TkKwLet(_), .. }] name: simplesymbol() [Token { kind: Tk::TkKwFor(_), .. }] parent: atype() args: func_args() r: func_ret_type()? limits: where_limit() [Token { kind: Tk::TkAssign(_), .. }] start: position!() body: expr()+ { crate::root::resource::ast::Ast::MethodDef { parent: parent.get_custom_name().to_string(), name: name.get_symbol_name().to_string(), args: if args.is_some() {args.unwrap()} else {vec![]}, rettype: if r.is_some() {r.unwrap()} else {crate::root::resource::ast::SymbolType::Generic(crate::root::passes::midend::environment::GenericValue::Ref(format!("?_{}", crate::root::resource::ast::calculate_hash::<String>(&name.get_symbol_name()) )))}, limits, body } }}
        / quiet!{[Token { kind: Tk::TkKwLet(_), .. }] name: simplesymbol() [Token { kind: Tk::TkKwFor(_), .. }] parent: atype() [Token { kind: Tk::TkAssign(_), .. }] body: expr()+ { crate::root::resource::ast::Ast::MethodDef {parent: parent.get_custom_name().to_string(), name: name.get_symbol_name().to_string(), args: vec![], rettype: crate::root::resource::ast::SymbolType::Naught, limits: None, body } }}
        / expected!("a function definition")

    rule func_ret_type() -> crate::root::resource::ast::SymbolType
        = [Token { kind: Tk::TkArr(_), .. }] r: atype() {r}

    rule func_args() -> Option<Vec<(String, crate::root::resource::ast::SymbolType)>>
        = ([Token { kind: Tk::TkKwOf(_), .. }] l: func_args_list() {l})?

    rule where_limit() -> Option<Vec<crate::root::resource::ast::Expr>> =
        (([Token { kind: Tk::TkKwWhere(_), .. }] e: expr() {e}) ** [Token { kind: Tk::TkComma(_), .. }])?

    rule func_args_list() -> Vec<(String, crate::root::resource::ast::SymbolType)>
        = a: type_arg() ** [Token { kind: Tk::TkComma(_), .. }] {a}

    rule type_arg() -> (String, crate::root::resource::ast::SymbolType)
        = t: simplesymbol() k: arg_type()? start: position!() {let r = if k.is_some()  {k.unwrap()} else {crate::root::resource::ast::SymbolType::Generic(crate::root::passes::midend::environment::GenericValue::Ref(format!("?_{}", crate::root::resource::ast::calculate_hash::<String>(&t.get_symbol_name()) )))}; return (t.get_symbol_name(), r)}

    rule arg_type() -> crate::root::resource::ast::SymbolType
        = [Token { kind: Tk::TkColon(_), .. }] k: atype() {k}

    rule prop_def() -> crate::root::resource::ast::Ast
        = [Token { kind: Tk::TkKwProp(_), .. }] n: simplesymbol() [Token { kind: Tk::TkKwFor(_), .. }] t: atype() [Token { kind: Tk::TkAssign(_), .. }] f: fn_sig() ** [Token { kind: Tk::TkComma(_), .. }] {crate::root::resource::ast::Ast::Propdef { p: crate::root::resource::ast::Property {name: n.get_symbol_name(), req: f}  }}

    rule fn_sig() -> crate::root::resource::ast::FnSignature
        = n: simplesymbol() args: sig_args() r: func_ret_type()? limits: where_limit() {crate::root::resource::ast::FnSignature {name: n.get_symbol_name(), args, rettype: if r.is_some() {r.unwrap()} else {crate::root::resource::ast::SymbolType::Generic(crate::root::passes::midend::environment::GenericValue::Ref(format!("?_{}", crate::root::resource::ast::calculate_hash::<String>(&n.get_symbol_name()) )))}, limits}}

    rule sig_args() -> Vec<crate::root::resource::ast::SymbolType>
        = a: ([Token { kind: Tk::TkKwOf(_), .. }] l: sig_arg_list() {l})? {if a.is_some() {a.unwrap()} else {vec![]}}

    rule sig_arg_list() -> Vec<(crate::root::resource::ast::SymbolType)>
        = a: sig_arg_type() ** [Token { kind: Tk::TkComma(_), .. }] {a}

    rule sig_arg_type() -> crate::root::resource::ast::SymbolType
        = k: atype() start: position!() {k}

    rule expr() -> crate::root::resource::ast::Expr
        = assignment()
        / closure()
        / structinstance()
        / ifexpr()
        / matchexpr()
        / r#return()
        / binary_op()

    rule assignment() -> crate::root::resource::ast::Expr
        = quiet! {n: simplesymbol() [Token { kind: Tk::TkAssign(_), .. }] v: expr()  { crate::root::resource::ast::Expr::Assignment { name: Box::new(n), value: Box::new(v) } }}
        / expected!("assignment")

    rule closure() -> crate::root::resource::ast::Expr
        = quiet! {[Token { kind: Tk::TkKwFn(_), .. }] args: func_args() [Token { kind: Tk::TkArr(_), .. }] body: expr()+ { crate::root::resource::ast::Expr::Closure { args: if args.is_some() {args.unwrap()} else {vec![]}, body } }}
        / expected!("a closure")

    rule r#return() -> crate::root::resource::ast::Expr
        = quiet! {[Token { kind: Tk::TkKwReturn(_), .. }] v: expr() { crate::root::resource::ast::Expr::Return { value: Box::new(v) } }}
        / expected!("a return expression")

    rule binary_op() -> crate::root::resource::ast::Expr =
    quiet! {precedence!{


        l: (@) op: [Token { kind: Tk::TkFuncComp(_), .. }] r: @ {crate::root::resource::ast::Expr::Composition { l: Box::new(l), r: Box::new(r) }}
        --
        l: (@) op: [Token { kind: Tk::TkKwIs(_), .. }] r: @ {crate::root::resource::ast::Expr::Logical { l: Box::new(l), op: crate::root::resource::ast::LogicOp::Is, r: Box::new(r) }}
        l: (@) op: [Token { kind: Tk::TkKwOr(_), .. }] r: @ {crate::root::resource::ast::Expr::Logical { l: Box::new(l), op: crate::root::resource::ast::LogicOp::Is, r: Box::new(r) }}
        l: (@) op: [Token { kind: Tk::TkKwAnd(_), .. }] r: @ {crate::root::resource::ast::Expr::Logical { l: Box::new(l), op: crate::root::resource::ast::LogicOp::Is, r: Box::new(r) }}

        l: (@) op: [Token { kind: Tk::TkCEQ(_), .. }] r: @ {crate::root::resource::ast::Expr::Logical { l: Box::new(l), op: crate::root::resource::ast::LogicOp::CEQ, r: Box::new(r) }}
        l: (@) op: [Token { kind: Tk::TkCLT(_), .. }] r: @ {crate::root::resource::ast::Expr::Logical { l: Box::new(l), op: crate::root::resource::ast::LogicOp::CLT, r: Box::new(r) }}
        l: (@) op: [Token { kind: Tk::TkCLE(_), .. }] r: @ {crate::root::resource::ast::Expr::Logical { l: Box::new(l), op: crate::root::resource::ast::LogicOp::CLE, r: Box::new(r) }}
        l: (@) op: [Token { kind: Tk::TkCGT(_), .. }] r: @ {crate::root::resource::ast::Expr::Logical { l: Box::new(l), op: crate::root::resource::ast::LogicOp::CGT, r: Box::new(r) }}
        l: (@) op: [Token { kind: Tk::TkCGE(_), .. }] r: @ {crate::root::resource::ast::Expr::Logical { l: Box::new(l), op: crate::root::resource::ast::LogicOp::CGE, r: Box::new(r) }}
        --
        l: (@) [Token { kind: Tk::TkPlus(_), .. }] r: @  { crate::root::resource::ast::Expr::BinAdd { l: Box::new(l), r: Box::new(r) } }
        l: (@) [Token { kind: Tk::TkMinus(_), .. }] r: @  { crate::root::resource::ast::Expr::BinSub { l: Box::new(l), r: Box::new(r) } }
        --
        l: (@) [Token { kind: Tk::TkStar(_), .. }] r: @  { crate::root::resource::ast::Expr::BinMul { l: Box::new(l), r: Box::new(r) } }
        l: (@) [Token { kind: Tk::TkSlash(_), .. }] r: @  { crate::root::resource::ast::Expr::BinDiv { l: Box::new(l), r: Box::new(r) } }
        --
        a: atom() {a}
        --
        l: (@) op: [Token { kind: Tk::TkDot(_), .. }] r: @ {crate::root::resource::ast::Expr::FieldAccess(Box::new(l), r.get_symbol_name())}

    }}
    / expected!("an arithmetic or comparison operator")


    #[cache_left_rec]
    rule atom() -> crate::root::resource::ast::Expr
        = intrinsic()
        / call()
        / symbol()
        / group()

    rule ifexpr() -> crate::root::resource::ast::Expr
        = quiet! {[Token { kind: Tk::TkKwIf(_), .. }] c: expr() [Token { kind: Tk::TkKwThen(_), .. }] t: expr() e: else_branch() {crate::root::resource::ast::Expr::If { condition: Box::new(c), then: Box::new(t), otherwise: e }}}
        / expected!("an if/else expression")

    rule else_branch() -> Box<crate::root::resource::ast::Expr>
        = quiet! {([Token { kind: Tk::TkKwElse(_), .. }] o: expr() {Box::new(o)})}
        / expected!("an else clause")

    rule matchexpr() -> crate::root::resource::ast::Expr
        = quiet! {[Token { kind: Tk::TkKwMatch(_), .. }] e: expr() [Token { kind: Tk::TkAssign(_), .. }] match_arm()+ {e}}
        / expected!("a match expression")

    rule match_arm() -> (crate::root::resource::ast::Predicate, crate::root::resource::ast::Expr)
        = p: predicate()  e: expr() {(p, e)}

    rule predicate() -> crate::root::resource::ast::Predicate
        = logic_predicate()

    rule logic_predicate() -> crate::root::resource::ast::Predicate
        = quiet! {precedence! {
            op: [Token { kind: Tk::TkCEQ(_), .. }] r: expr() [Token { kind: Tk::TkKwThen(_), .. }] {crate::root::resource::ast::Predicate::Comparison { op: crate::root::resource::ast::LogicOp::CEQ, rhs: Box::new(r) }}
            op: [Token { kind: Tk::TkCLT(_), .. }] r: expr() [Token { kind: Tk::TkKwThen(_), .. }] {crate::root::resource::ast::Predicate::Comparison { op: crate::root::resource::ast::LogicOp::CLT, rhs: Box::new(r) }}
            op: [Token { kind: Tk::TkCLE(_), .. }] r: expr() [Token { kind: Tk::TkKwThen(_), .. }] {crate::root::resource::ast::Predicate::Comparison { op: crate::root::resource::ast::LogicOp::CLE, rhs: Box::new(r) }}
            op: [Token { kind: Tk::TkCGT(_), .. }] r: expr() [Token { kind: Tk::TkKwThen(_), .. }] {crate::root::resource::ast::Predicate::Comparison { op: crate::root::resource::ast::LogicOp::CGT, rhs: Box::new(r) }}
            op: [Token { kind: Tk::TkCGE(_), .. }] r: expr() [Token { kind: Tk::TkKwThen(_), .. }] {crate::root::resource::ast::Predicate::Comparison { op: crate::root::resource::ast::LogicOp::CGE, rhs: Box::new(r) }}
        }}
        / expected!("a predicate")


    rule intrinsic() -> crate::root::resource::ast::Expr
        = quiet! {[Token { kind: Tk::TkFlt(_), lit: n,.. }] { crate::root::resource::ast::Expr::Flt(n.parse().unwrap() )}}
        / quiet! {[Token { kind: Tk::TkInt(_), lit: n,.. }] { crate::root::resource::ast::Expr::Int(n.parse().unwrap() )}}
        / quiet! {[Token { kind: Tk::TkStrLit(_), lit: n,.. }] { crate::root::resource::ast::Expr::Str(n.parse().unwrap() )}}
        / quiet! {[Token { kind: Tk::TkFalse(_), lit: n,.. }] { crate::root::resource::ast::Expr::Bool(false)}}
        / quiet! {[Token { kind: Tk::TkTrue(_), lit: n,.. }] { crate::root::resource::ast::Expr::Bool(true)}}
        / quiet! {[Token { kind: Tk::TkPtrInit(_), .. }] e: expr() {crate::root::resource::ast::Expr::AddressOf(Box::new(e))}}
        / expected!("a value")

    #[cache_left_rec]
    rule call() -> crate::root::resource::ast::Expr
        = primary: simplesymbol() r: call_suffix(primary) {r}

rule call_suffix(lhs: crate::root::resource::ast::Expr) -> crate::root::resource::ast::Expr
    = [Token { kind: Tk::TkDot(_), .. }] name: simplesymbol() &[Token { kind: Tk::TkLparen(_), .. }] [Token { kind: Tk::TkLparen(_), .. }] args: call_list() [Token { kind: Tk::TkRparen(_), .. }] r: call_suffix(crate::root::resource::ast::Expr::MethodCall { obj: Box::new(lhs.clone()), name: Box::new(name), args}) {r}
    / [Token { kind: Tk::TkLparen(_), .. }] args: call_list() [Token { kind: Tk::TkRparen(_), .. }] r: call_suffix(crate::root::resource::ast::Expr::Call { name: Box::new(lhs.clone()), args}) {r}

    / { lhs }  // Default case: no more calls

    // rule call() -> crate::root::resource::ast::Expr
    //     = name: atom() [Token { kind: Tk::TkLparen, .. }] args: call_list() [Token { kind: Tk::TkRparen, .. }] { crate::root::resource::ast::Expr::Call { name: Box::new(name.clone()), args}}
    //     / name: simplesymbol() [Token { kind: Tk::TkDoubleColon, .. }]  method: simplesymbol() [Token { kind: Tk::TkLparen, .. }] args: call_list() [Token { kind: Tk::TkRparen, .. }] { crate::root::resource::ast::Expr::ModuleCall {module: Box::new(name.clone()), name: Box::new(method.clone()), args}}


    rule call_list() -> Vec<crate::root::resource::ast::Expr>
        = expr() ** [Token { kind: Tk::TkComma(_), .. }] //{ expr }

    rule structinstance() -> crate::root::resource::ast::Expr
        = n: simplesymbol() [Token { kind: Tk::TkLbrace(_), .. }] f: fieldinit() ** [Token { kind: Tk::TkComma(_), .. }] [Token { kind: Tk::TkRbrace(_), .. }] {crate::root::resource::ast::Expr::StructInstance { name: Box::new(n), fields: f }}

    rule fieldinit() -> (String, crate::root::resource::ast::Expr)
        = n: simplesymbol() [Token { kind: Tk::TkColon(_), .. }] e: expr() {(n.get_symbol_name(), e)}

    // rule field_access() -> crate::root::resource::ast::Expr
    //     = n: simplesymbol() [Token { kind: Tk::TkDot(_), .. }] s: symbol() {crate::root::resource::ast::Expr::FieldAccess(Box::new(n), s.get_symbol_name())}

    rule symbol() -> crate::root::resource::ast::Expr
        // quiet! {f: field_access() {f}}
        = quiet! {s: simplesymbol() {s}}
        / expected!("an identifier")

        // namespace()


    rule simplesymbol() -> crate::root::resource::ast::Expr
        = quiet! {[Token { kind: Tk::TkSymbol(_), lit: n,.. }] { crate::root::resource::ast::Expr::Symbol(n.to_string())}}
        / expected!("an identifier")

    rule atype() -> crate::root::resource::ast::SymbolType
        = [Token { kind: Tk::TkKwInt(_), .. }] {crate::root::resource::ast::SymbolType::Int}
        / [Token { kind: Tk::TkKwUint(_), .. }] {crate::root::resource::ast::SymbolType::Uint}
        / [Token { kind: Tk::TkKwWord(_), .. }] {crate::root::resource::ast::SymbolType::Word}
        / [Token { kind: Tk::TkKwByte(_), .. }] {crate::root::resource::ast::SymbolType::Byte}
        / [Token { kind: Tk::TkKwFlt(_), .. }] {crate::root::resource::ast::SymbolType::Flt}
        / [Token { kind: Tk::TkKwStr(_), .. }] {crate::root::resource::ast::SymbolType::Str}
        / [Token { kind: Tk::TkKwChar(_), .. }] {crate::root::resource::ast::SymbolType::Char}
        / [Token { kind: Tk::TkKwBool(_), .. }] {crate::root::resource::ast::SymbolType::Bool}
        / [Token { kind: Tk::TkKwFnTy(_), .. }] {crate::root::resource::ast::SymbolType::Fn(vec![], crate::root::resource::ast::SymbolType::Unknown.into(), false)}
        / [Token { kind: Tk::TkKwNaught(_), .. }] {crate::root::resource::ast::SymbolType::Naught}
        / s: symbol() a: (generic_brackets())? {crate::root::resource::ast::SymbolType::Custom(s.get_symbol_name(), if a.is_some() {a.unwrap()} else {vec![]})}
        / [Token { kind: Tk::TkQuestion(_), .. }] s: simplesymbol() start:position!() {crate::root::resource::ast::SymbolType::Generic(crate::root::passes::midend::environment::GenericValue::Ref(format!("?_{}", s.get_symbol_name())))}
        / [Token { kind: Tk::TkPtrArr(_), .. }] t: atype() {crate::root::resource::ast::SymbolType::Pointer(Box::new(t))}
        / expected!("a type")

    rule generic_brackets() -> Vec<crate::root::resource::ast::SymbolType>
        = [Token { kind: Tk::TkCLT(_), .. }] a: atype() ** [Token { kind: Tk::TkComma(_), .. }] [Token { kind: Tk::TkCGT(_), .. }]{a}

    // rule namespace() -> crate::root::resource::ast::Expr
    //     = s: simplesymbol() ** [Token { kind: Tk::TkDoubleColon(_), .. }] {crate::root::resource::ast::Expr::Namespace(s)}

    rule group() -> crate::root::resource::ast::Expr
        = quiet! { [Token { kind: Tk::TkLparen(_), .. }] v:expr() [Token { kind: Tk::TkRparen(_), .. }] { v } }
        / expected!("a grouping expression")

});

use lang::program;
use reporting::Location;

use crate::root::resource::{
    ast::FileModule,
    errors::ParsingError,
    tk::{Tk, Token},
};

pub fn parse(tokens: &[Token], filename: String, src: String) -> anyhow::Result<FileModule> {
    let p = program(&SliceByRef(tokens));
    match p {
        Ok(file_module) => Ok(file_module),
        Err(e) => {
            let the_tok: &Token = tokens.get(e.location).or_else(|| tokens.last()).unwrap();
            let expected: String = e
                .expected
                .tokens()
                .map(|x| x.to_string())
                .collect::<Vec<String>>()
                .join(", ");
            let styles = Styles::styled();
            let file = File::new(filename.clone(), src);
            let msg = Renderer::new(
                &styles,
                &[
                    error!("Could not parse {filename}")
                        .location(Location::new(file.clone(), the_tok.get_span().start)),
                    expected!("{}", expected),
                ],
            )
            .to_string();

            Err(ParsingError::NonspecificParsingError { msg }.into())
        }
    }
}
