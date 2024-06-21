use pomelo::pomelo;

pomelo! {
    //%verbose;
    %include {
        use crate::root::resource::ast::*;
    }
    %token #[derive(Debug)] pub enum Token {};
    %extra_argument Program;
    %type Ident String;
    %type Vtype VTypeKind;
    %type Scalar crate::root::resource::itypes::Itype;
    %type Pair Pair;
    %type expr Expr;
    %type expr_list Vec<Expr>;
    %type stmt Stmt;
    %type block Vec<Stmt>;
    %type stmt_list Vec<Stmt>;
    %type arg_list Vec<Pair>;
    %type f_decl Function;
    %type v_decl Variable;

    %left Else;
    %right Assign;
    %right Arrow;
    %left Or;
    %left And;
    %nonassoc Equal NotEqual;
    %nonassoc Less LessEq Greater GreaterEq;
    %left Plus Minus;
    %left Mult Div;
    %nonassoc Not;

    input ::= decl_list?;

    decl_list ::= decl;
    decl_list ::= decl_list decl;

    decl ::= f_decl(f) { extra.add_function(f); }

    //f_decl ::= Let Ident(name) Of arg_list?(args) for_clause(t)Assign stmt_list(code) { Function::new(name, args.unwrap_or_else(Vec::new), code, None) }
    f_decl ::= Let Ident(name) Of arg_list?(args) Assign stmt_list(code) { Function::new(name, args.unwrap_or_else(Vec::new), code, None) }
    f_decl ::= Let Ident(name) Assign stmt_list(code) { Function::new(name, vec![], code, None) }
    f_decl ::= Fn Of arg_list?(args) Arrow stmt_list(code) {
        use std::hash::Hasher;
        let mut h = std::hash::DefaultHasher::new();
        let args = args.clone().unwrap_or_else(Vec::new);
        h.write(args.clone().iter().map(|f| f.name.chars().collect::<Vec<char>>()[0] as u8).collect::<Vec<u8>>().as_slice());
        let hashval = h.finish();
        Function::new(format!("anon_{}", hashval), args, code, None) }

    // for_clause ::= For type_decl(t) {t}
    
    // type_decl ::= Ident(n) {VType}
    // type_decl ::= Bang Ident(n) {n}
    // type_decl ::= Question Ident(n) {n}
    // type_decl ::= Bang Question Ident(n) {n}

    
    arg_list ::= Ident(n) Colon Vtype(t) { vec![Pair {name: n, value: t}] }
    arg_list ::= arg_list(mut args) Comma Ident(n) Colon Bang? Question? Vtype(v) { args.push(Pair {name: n, value: v}); args }

    block ::= Do stmt_list(ss) End { ss }

    stmt_list ::= StatementEnd? stmt(s) { vec![s] }
    stmt_list ::= stmt_list(mut ss) stmt(s) { ss.push(s); ss }

    stmt ::= block(ss) { Stmt::Block(ss) }

    stmt ::= If expr(e) block(s1) [Else] { Stmt::If(e, Box::new((Stmt::Block(s1), None))) }
    stmt ::= If expr(e) block(s1) Else block(s2) {Stmt::If(e, Box::new((Stmt::Block(s1), Some(Stmt::Block(s2)))))  }
    stmt ::= While  expr(e) block(s) { Stmt::While(e, Box::new(Stmt::Block(s))) }
    stmt ::= Return expr(e) StatementEnd { Stmt::Return(e) }
    stmt ::= Break  { Stmt::Break }
    stmt ::= Continue  {Stmt::Continue }
    stmt ::= For Ident(i) In Ident(j) block(s) {Stmt::ForEach(i, j, Box::new(Stmt::Block(s)))}
    stmt ::= For Ident(i) In Scalar(b) Thru Scalar(t) block(s) {Stmt::ForRange(i, b, t, Box::new(Stmt::Block(s)))}
    stmt ::= expr(e) StatementEnd {Stmt::Expr(e) }


    //expr ::= Number(n) { Expr::Number(n) }
    //expr ::= String(s) { Expr::String(s) }
    expr ::= Ident(n) { Expr::Variable(n) }
    expr ::= Bang? Question? Scalar(s) { Expr::Scalar(s) }

    expr ::= Ident(n) LParen expr_list?(es) RParen { Expr::Call(n, es.unwrap_or(Vec::new())) }
    expr ::= LParen expr(e) RParen { e }

    expr ::= expr(a) Plus expr(b) { Expr::BinaryOp(BinOp::Plus, Box::new((a, b))) }
    expr ::= expr(a) Minus expr(b) { Expr::BinaryOp(BinOp::Minus, Box::new((a, b))) }
    expr ::= expr(a) Mult expr(b) { Expr::BinaryOp(BinOp::Mult, Box::new((a, b))) }
    expr ::= expr(a) Div expr(b) { Expr::BinaryOp(BinOp::Div, Box::new((a, b))) }
    expr ::= Minus expr(a) [Not] { Expr::UnaryOp(UnaOp::Neg, Box::new(a)) }

    expr ::= expr(a) Equal expr(b) { Expr::BinaryOp(BinOp::Equal, Box::new((a, b))) }
    expr ::= expr(a) NotEqual expr(b) { Expr::BinaryOp(BinOp::NotEqual, Box::new((a, b))) }

    expr ::= expr(a) And expr(b) { Expr::BinaryOp(BinOp::And, Box::new((a, b))) }
    expr ::= expr(a) Or expr(b) { Expr::BinaryOp(BinOp::Or, Box::new((a, b))) }
    expr ::= Not expr(a) { Expr::UnaryOp(UnaOp::Not, Box::new(a)) }

    expr ::= expr(a) Less expr(b) { Expr::BinaryOp(BinOp::Less, Box::new((a, b))) }
    expr ::= expr(a) Greater expr(b) { Expr::BinaryOp(BinOp::Greater, Box::new((a, b))) }
    expr ::= expr(a) LessEq expr(b) { Expr::BinaryOp(BinOp::LessEq, Box::new((a, b))) }
    expr ::= expr(a) GreaterEq expr(b) { Expr::BinaryOp(BinOp::GreaterEq, Box::new((a, b))) }

    expr ::= Ident(n) Assign expr(b) { Expr::Assign(Variable::new(n, b)) }

    expr_list ::= expr(e) { vec![e] }
    expr_list ::= expr_list(mut es) Comma expr(e) { es.push(e); es }
}

pub use parser::*;
