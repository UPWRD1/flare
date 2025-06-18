use anyhow::{format_err, Result};
use qbe::{DataDef, Function, Instr, Linkage, Type, TypeDef, Value};
use std::{cmp, collections::HashMap};

use crate::{
    quantifier,
    root::{
        passes::midend::environment::{Entry, Environment, FunctionTableEntry, UserTypeTableEntry},
        resource::cst::{Expr, SymbolType},
        Quantifier,
    },
};

#[derive(Debug, Clone)]
pub struct Generator<'a> {
    env: Environment,
    tmp_counter: usize,
    //module: qbe::Module<'a>,
    buf: String,
    scopes: Vec<HashMap<String, (Type<'a>, Value)>>,
    typedefs: Vec<qbe::TypeDef<'a>>,
    datadefs: Vec<qbe::DataDef<'a>>,
    struct_map: HashMap<String, (qbe::Type<'a>, StructMeta<'a>, u64)>,
}

type StructMeta<'a> = HashMap<String, (qbe::Type<'a>, u64)>;

impl<'a> Generator<'a> {
    pub fn new(env: Environment) -> Self {
        Self {
            env,
            tmp_counter: 0,
            //module: Module::new(),
            buf: String::new(),
            scopes: vec![],
            typedefs: vec![],
            datadefs: vec![],
            struct_map: HashMap::new(),
        }
    }

    fn new_temporary(&mut self) -> Value {
        self.tmp_counter += 1;
        qbe::Value::Temporary(format!("tmp.{}", self.tmp_counter))
    }

    fn new_var(&mut self, ty: Type<'a>, name: &str) -> Result<Value> {
        if self.get_var(name.to_owned()).is_ok() {
            return Err(format_err!("Re-declaration of variable '{}'", name));
        }

        let tmp = self.new_temporary();

        let scope = self
            .scopes
            .last_mut()
            .expect("expected last scope to be present");
        scope.insert(name.to_owned(), (ty.to_owned(), tmp.to_owned()));

        Ok(tmp)
    }
    fn get_var(&self, name: String) -> Result<&(qbe::Type<'a>, qbe::Value)> {
        self.scopes
            .iter()
            .rev()
            .filter_map(|s| s.get(&name))
            .next()
            .ok_or_else(|| format_err!("Undefined variable '{}'", name))
    }

    fn convert_symboltype(&mut self, t: &SymbolType) -> Type<'a> {
        match t {
            SymbolType::Int => Type::Word,
            SymbolType::Usize => Type::Word,

            SymbolType::Flt => Type::Double,
            SymbolType::Quant(name) => {
                //dbg!(t);
                //dbg!(self.struct_map.clone());
                let name = self.env.get_q(name).unwrap().to_ty().name;
                let (ty, ..) = self
                    .struct_map
                    .get(&name)
                    .ok_or_else(|| format!("Use of undeclared type '{}'", name))
                    .unwrap()
                    .to_owned();
                ty
            }
            SymbolType::Unit => Type::Word,
            SymbolType::Str => Type::Long,
            SymbolType::Generic(_) => Type::Long,
            SymbolType::Pointer(_) => Type::Long,
            //SymbolType::Variant(id, fields) => Type::Long,
            _ => todo!("{t:?}"),
        }
    }

    fn generate_expr(&mut self, func: &mut Function<'a>, ex: &Expr) -> Result<(Type<'a>, Value)> {
        //dbg!(ex.clone());
        match ex {
            Expr::Int(v) => {
                let tmp = self.new_temporary();
                func.assign_instr(
                    tmp.clone(),
                    Type::Word,
                    Instr::Copy(Value::Const(*v as u64)),
                );
                Ok((Type::Word, tmp))
            }
            Expr::Naught => {
                let tmp = self.new_temporary();
                func.assign_instr(tmp.clone(), Type::Word, Instr::Copy(Value::Const(0)));
                Ok((Type::Word, tmp))
            }
            Expr::Flt(v) => {
                let tmp = self.new_temporary();
                func.assign_instr(
                    tmp.clone(),
                    Type::Double,
                    Instr::Copy(Value::Const(v.0 as f64 as u64)),
                );
                Ok((Type::Double, tmp))
            }
            Expr::Str(v) => self.generate_string(&v),

            Expr::BinAdd { l, r } => {
                let (lty, lhs_val) = self.generate_expr(func, l)?;
                let (rty, rhs_val) = self.generate_expr(func, r)?;
                let tmp = self.new_temporary();
                assert!(lty == rty);
                func.assign_instr(tmp.clone(), lty.clone(), Instr::Add(lhs_val, rhs_val));
                Ok((lty, tmp))
            }
            Expr::BinSub { l, r } => {
                let (lty, lhs_val) = self.generate_expr(func, l)?;
                let (rty, rhs_val) = self.generate_expr(func, r)?;
                let tmp = self.new_temporary();
                assert!(lty == rty);
                func.assign_instr(tmp.clone(), lty.clone(), Instr::Sub(lhs_val, rhs_val));
                Ok((lty, tmp))
            }
            Expr::BinMul { l, r } => {
                let (lty, lhs_val) = self.generate_expr(func, l)?;
                let (rty, rhs_val) = self.generate_expr(func, r)?;
                let tmp = self.new_temporary();
                assert!(lty == rty);
                func.assign_instr(tmp.clone(), lty.clone(), Instr::Mul(lhs_val, rhs_val));
                Ok((lty, tmp))
            }
            Expr::BinDiv { l, r } => {
                let (lty, lhs_val) = self.generate_expr(func, l)?;
                let (rty, rhs_val) = self.generate_expr(func, r)?;
                let tmp = self.new_temporary();
                assert!(lty == rty);
                func.assign_instr(tmp.clone(), lty.clone(), Instr::Div(lhs_val, rhs_val));
                Ok((lty, tmp))
            }
            Expr::Logical { l, op, r } => {
                let (lty, lhs_val) = self.generate_expr(func, l)?;
                let (rty, rhs_val) = self.generate_expr(func, r)?;
                let tmp = self.new_temporary();
                let ty = Type::Word;
                assert!(lty == rty);

                func.assign_instr(
                    tmp.clone(),
                    lty.clone(),
                    qbe::Instr::Cmp(
                        ty.clone(),
                        match op {
                            crate::root::resource::cst::LogicOp::CEQ => qbe::Cmp::Eq,
                            crate::root::resource::cst::LogicOp::CLT => qbe::Cmp::Slt,
                            crate::root::resource::cst::LogicOp::CLE => qbe::Cmp::Sle,
                            crate::root::resource::cst::LogicOp::CGT => qbe::Cmp::Sgt,
                            crate::root::resource::cst::LogicOp::CGE => qbe::Cmp::Sge,
                            _ => todo!(),
                        },
                        lhs_val,
                        rhs_val,
                    ),
                );
                Ok((ty, tmp))
            }
            Expr::Symbol(name) => self.get_var(name.to_string()).cloned(),
            Expr::StructInstance { name, fields } => {
                self.generate_struct_init(func, &name.get_symbol_name().unwrap(), fields.clone())
            }
            Expr::FieldAccess(parent, field) => {
                self.generate_field_access(func, *parent.clone(), field)
            }
            Expr::Call { name, args } => {
                let mut new_args: Vec<(Type<'a>, Value)> = Vec::new();
                for arg in args.iter() {
                    let arg_ty = self.generate_expr(func, arg)?;
                    if matches!(arg_ty.0, Type::Aggregate(_)) {
                        new_args.push((Type::Long, arg_ty.1));
                    } else {
                        new_args.push(arg_ty);
                    }
                }
                let the_ty = self.convert_symboltype(
                    &self
                        .env
                        .items
                        .get(&quantifier!(
                            Root,
                            Func(name.get_symbol_name().unwrap()),
                            End
                        ))
                        .ok_or(format_err!("{name:?}"))?
                        .to_func()
                        .return_type,
                );
                let tmp = self.new_temporary();
                func.assign_instr(
                    tmp.clone(),
                    // TODO: get that type properly
                    the_ty.clone(),
                    Instr::Call(name.get_symbol_name().unwrap().clone(), new_args, None),
                );

                Ok((the_ty.clone(), tmp))
            }
            Expr::MethodCall { obj, name, args } => {
                let mut new_args: Vec<(Type<'a>, Value)> = Vec::new();

                let the_obj: (Type<'a>, Value) = match self.generate_expr(func, obj) {
                    Ok(e) => {
                        new_args.push((Type::Long, e.1.clone()));
                        e
                    }
                    Err(_) => panic!(),
                };

                self.generate_methodcall(func, new_args, the_obj, name, args)
            }
            Expr::Path(l, r) => {
                let new_args: Vec<(Type<'a>, Value)> = Vec::new();
                let the_obj = (
                    self.struct_map
                        .iter()
                        .filter(|t| *t.0 == l.get_symbol_name().unwrap())
                        .nth(0)
                        .unwrap()
                        .1
                         .0
                        .clone(),
                    Value::Temporary(l.get_symbol_name().unwrap()),
                );
                //self.new_var(the_obj.0.clone(), &l.get_symbol_name()?);
                match &**r {
                    Expr::Call { name, args } => {
                        self.generate_methodcall(func, new_args, the_obj, name, args)
                    }
                    _ => panic!(),
                }
            }
            Expr::If {
                condition,
                then,
                otherwise,
            } => self.generate_if(func, &condition, &then, &otherwise),
            Expr::AddressOf(e) => {
                let expr_val = self.generate_expr(func, e)?;
                let temp = self.new_temporary();
                func.assign_instr(
                    temp.clone(),
                    qbe::Type::Long,
                    // XXX: Always align to 8 bytes?
                    qbe::Instr::Alloc8(expr_val.0.size()),
                );

                Ok((Type::Long, temp))
            }

            _ => todo!("{ex:?}"),
        }
    }

    fn generate_methodcall(
        &mut self,
        func: &mut Function<'a>,
        new_args: Vec<(Type<'a>, Value)>,
        the_obj: (Type<'a>, Value),
        name: &Box<Expr>,
        args: &Vec<Expr>,
    ) -> std::result::Result<(Type<'a>, Value), anyhow::Error> {
        let mut new_args: Vec<(Type<'_>, Value)> = new_args.clone();

        // let the_obj_name: String = obj.get_parent_name();
        // dbg!(&the_obj_name);
        //let obj_type = self.env.usertype_table.get(&func.name).unwrap();
        //dbg!(obj_type);
        //new_args.push(the_obj);

        for arg in args.iter() {
            new_args.push(self.generate_expr(func, arg)?);
        }
        let tmp = self.new_temporary();

        func.assign_instr(
            tmp.clone(),
            // TODO: get that type properly
            the_obj.0.clone(), //self.convert_symboltype(&self.env.usertype_table.get(&obj.get_symbol_name()?).unwrap().methods.get(&name.get_symbol_name().unwrap()).unwrap().return_type),
            Instr::Call(name.get_symbol_name().unwrap().clone(), new_args, None),
        );

        Ok((the_obj.0, tmp))
    }

    fn generate_string(&mut self, string: &str) -> Result<(Type<'a>, Value)> {
        self.tmp_counter += 1;
        let name = format!("string.{}", self.tmp_counter);
        let mut items: Vec<(Type<'a>, qbe::DataItem)> = Vec::new();
        let mut buf = String::new();
        let string = string
            .strip_prefix("\"")
            .unwrap()
            .strip_suffix("\"")
            .unwrap();
        for ch in string.chars() {
            if ch.is_ascii() && !ch.is_ascii_control() && ch != '"' {
                buf.push(ch)
            } else {
                if !buf.is_empty() {
                    items.push((qbe::Type::Byte, qbe::DataItem::Str(buf.clone())));
                    buf.clear();
                }

                let mut buf = [0; 4];
                let len = ch.encode_utf8(&mut buf).len();

                for b in buf.iter().take(len) {
                    items.push((qbe::Type::Byte, qbe::DataItem::Const(*b as u64)));
                }
                continue;
            }
        }
        if !buf.is_empty() {
            items.push((qbe::Type::Byte, qbe::DataItem::Str(buf)));
        }
        // NUL terminator
        items.push((qbe::Type::Byte, qbe::DataItem::Const(0)));

        self.datadefs.push(DataDef {
            linkage: Linkage::public(),
            name: name.clone(),
            align: None,
            items,
        });

        Ok((qbe::Type::Long, qbe::Value::Global(name)))
    }

    fn generate_struct_init(
        &mut self,
        func: &mut Function<'a>,
        name: &str,
        fields: Vec<(String, Expr)>,
    ) -> Result<(Type<'a>, Value)> {
        let base = self.new_temporary();
        let (ty, meta, size) = self
            .struct_map
            .get(name)
            .ok_or_else(|| format_err!("Initialization of undeclared struct '{}'", name))?
            .to_owned();

        func.assign_instr(
            base.clone(),
            qbe::Type::Long,
            // XXX: Always align to 8 bytes?
            qbe::Instr::Alloc8(size),
        );

        for (name, expr) in fields {
            let (_, offset) = meta
                .get(&name)
                .ok_or_else(|| format_err!("Unknown field '{}'", name))?;

            let (ty, expr_tmp) = self.generate_expr(func, &expr)?;
            match ty {
                qbe::Type::Aggregate(_) => {
                    let field_tmp = self.new_temporary();
                    func.assign_instr(
                        field_tmp.clone(),
                        qbe::Type::Long,
                        qbe::Instr::Add(base.clone(), qbe::Value::Const(*offset)),
                    );
                    let sz = ty.size();
                    // TODO: avoid memcpy here
                    func.add_instr(Instr::Call(
                        "memcpy".into(),
                        vec![
                            (qbe::Type::Long, field_tmp),
                            (qbe::Type::Long, expr_tmp),
                            (qbe::Type::Long, qbe::Value::Const(sz)),
                        ],
                        None,
                    ));
                }
                _ => {
                    let field_tmp = self.new_temporary();
                    func.assign_instr(
                        field_tmp.clone(),
                        qbe::Type::Long,
                        qbe::Instr::Add(base.clone(), qbe::Value::Const(*offset)),
                    );

                    func.add_instr(qbe::Instr::Store(ty, field_tmp, expr_tmp));
                }
            }
        }

        Ok((ty, base))
    }

    fn generate_field_access(
        &mut self,
        func: &mut Function<'a>,
        obj: Expr,
        field: &Expr,
    ) -> Result<(qbe::Type<'a>, qbe::Value)> {
        let (src, ty, offset) = self.resolve_field_access(&obj, &field)?;

        let field_ptr = self.new_temporary();
        func.assign_instr(
            field_ptr.clone(),
            Type::Long,
            Instr::Add(src, Value::Const(offset)),
        );

        let tmp = self.new_temporary();
        func.assign_instr(tmp.clone(), ty.clone(), Instr::Load(ty.clone(), field_ptr));

        Ok((ty, tmp))
    }

    fn resolve_field_access(&mut self, obj: &Expr, field: &Expr) -> Result<(Value, Type<'a>, u64)> {
        let (src, ty, off) = match obj {
            Expr::Symbol(var) => {
                let (ty, src) = self.get_var(var.to_string())?.to_owned();
                (src, ty, 0)
            }

            Expr::FieldAccess(expr, field) => self.resolve_field_access(expr, field)?,
            other => {
                return Err(format_err!(
                    "Invalid field access type: expected variable, field access or 'self', got {:?}",
                    other,
                ));
            }
        };

        // XXX: this is very hacky and inefficient
        let (name, meta) = self
            .struct_map
            .iter()
            .filter_map(
                |(name, (sty, meta, _))| {
                    if ty == *sty {
                        Some((name, meta))
                    } else {
                        None
                    }
                },
            )
            .next()
            .unwrap();

        let (ty, offset) = meta
            .get(&field.get_symbol_name().unwrap())
            .ok_or_else(|| {
                format_err!(
                    "No field '{}' on struct {}",
                    field.get_symbol_name().unwrap(),
                    name
                )
            })?
            .to_owned();

        Ok((src, ty, offset + off))
    }

    fn generate_assignment(
        &mut self,
        func: &mut Function<'a>,
        lhs: &Expr,
        rhs: &Expr,
    ) -> Result<()> {
        match lhs.clone() {
            Expr::Symbol(name) => {
                let ty = self.convert_symboltype(
                    &&self
                        .env
                        .items
                        .get(&quantifier!(
                            Root,
                            Func(func.name.clone()),
                            Variable(name.clone())
                        ))
                        .unwrap()
                        .to_variable()
                        .mytype
                        .clone(),
                );
                let tmp = self.new_var(ty, &name)?;
                let (ty, result) = self.generate_expr(func, &rhs)?;
                func.assign_instr(tmp, ty, qbe::Instr::Copy(result));
            }
            _ => todo!(),
        }

        Ok(())
    }

    fn generate_if(
        &mut self,
        func: &mut qbe::Function<'a>,
        cond: &Expr,
        if_clause: &Expr,
        else_clause: &Expr,
    ) -> Result<(Type<'a>, Value)> {
        let (_, result) = self.generate_expr(func, cond)?;

        self.tmp_counter += 1;
        let if_label = format!("cond.{}.if", self.tmp_counter);
        let else_label = format!("cond.{}.else", self.tmp_counter);
        let end_label = format!("cond.{}.end", self.tmp_counter);

        func.add_instr(qbe::Instr::Jnz(
            result,
            if_label.clone(),
            else_label.clone(),
        ));

        func.add_block(if_label);
        let res = self.generate_expr(func, if_clause)?;

        // Jump over to the end to prevent fallthrough into else
        // clause, unless the last block already jumps
        if !func.blocks.last().map_or(false, |b| b.jumps()) {
            func.add_instr(qbe::Instr::Jmp(end_label.clone()));
        }

        func.add_block(else_label);
        self.generate_expr(func, else_clause)?;

        func.add_block(end_label);

        Ok(res)
    }

    fn generate_stmt(&mut self, func: &mut Function<'a>, ex: &Expr) -> Result<()> {
        match ex {
            Expr::Assignment {
                name,
                value,
                and_in,
            } => {
                self.generate_assignment(func, name, value)?;
                self.generate_stmt(func, and_in)?;
            }
            _ => {
                let val = self.generate_expr(func, &ex)?.1;
                func.add_instr(Instr::Ret(Some(val)));
            }
        }
        Ok(())
    }

    fn align_offset(&self, offset: u64, alignment: u64) -> u64 {
        (offset + alignment - 1) & !(alignment - 1)
    }

    fn type_alignment(&self, ty: &qbe::Type<'a>) -> u64 {
        match ty {
            qbe::Type::Zero => 1,
            qbe::Type::Byte => 1,
            qbe::Type::Halfword => 2,
            qbe::Type::Word | qbe::Type::Single => 4,
            qbe::Type::Long | qbe::Type::Double => 8,
            qbe::Type::Aggregate(name) => {
                let td = self
                    .typedefs
                    .iter()
                    .find(|&x| x.name.eq(&name.name))
                    .unwrap();
                // Aggregate type's alignment is the maximum alignment of its fields
                td.items
                    .iter()
                    .map(|(ty, _)| self.type_alignment(ty))
                    .max()
                    .unwrap_or(1)
            }
            _ => panic!("YOU SHOULD NOT BE HERE"),
        }
    }

    fn generate_function(&mut self, f: &FunctionTableEntry) -> Result<()> {
        self.scopes.push(HashMap::new());

        let mut nargs: Vec<(Type<'a>, Value)> = vec![];
        for arg in &f.args {
            let arg_t = self.convert_symboltype(&arg.1);
            let tmp = self.new_var(arg_t.clone(), &arg.0)?;

            nargs.push((arg_t, tmp));
        }
        let func_rt = if f.return_type.is_unit() {
            None
        } else {
            Some(self.convert_symboltype(&f.return_type))
        };
        let mut func: Function<'a> =
            Function::new(Linkage::public(), f.name.clone(), nargs, func_rt);
        // if let Some(p) = &f.method_parent {
        //     func.name = format!("{}.{}", f.name, f.method_parent.clone().unwrap())
        //}

        func.add_block("start");

        self.generate_stmt(&mut func, &f.body)?;

        // let returns = func.blocks.last().unwrap().items.last().map_or(
        //     false,
        //     |i: &qbe::BlockItem<'a>| match i {
        //         qbe::BlockItem::Statement(statement) => {
        //             matches!(statement, qbe::Statement::Volatile(qbe::Instr::Ret()))
        //         }
        //         qbe::BlockItem::Comment(_) => todo!(),
        //     },
        // );
        // // Automatically add return in void functions unless it already returns,
        // // non-void functions raise an error
        // if !returns {
        //     if f.return_type.is_unit() {
        //         func.add_instr(qbe::Instr::Ret(func.blocks.last().unwrap().items.last().unwrap().));
        //     } else {
        //         func.add_instr(Instr::Ret(self.scopes));
        //     //     return Err(format_err!(
        //     //         "Function '{}' does not return in all code paths",
        //     //         &func.name
        //     //     ));
        //     }
        // }
        // if f.method_parent.is_some() {
        //     func.name = format!("{}_{}", f.name, f.method_parent.clone().unwrap());
        // }
        self.buf.push_str(&format!("{}\n", func));

        self.scopes.pop();
        Ok(())
    }

    fn generate_typedef(&mut self, t: &UserTypeTableEntry) -> Result<()> {
        self.tmp_counter += 1;
        let mut typedef: TypeDef<'a> = TypeDef {
            name: t.name.clone(),
            align: None, // We'll set this after calculating max alignment
            items: Vec::new(),
        };
        let mut meta: StructMeta<'a> = StructMeta::new();
        let mut offset = 0_u64;
        let mut max_align = 1_u64;

        for field in &t.kind.get_fields()? {
            let ty = self.convert_symboltype(&field.1);

            let field_align = self.type_alignment(&ty);
            max_align = cmp::max(max_align, field_align);

            // Align the current offset for this field
            offset = self.align_offset(offset, field_align);

            meta.insert(field.0.clone(), (ty.clone(), offset));
            typedef.items.push((ty.clone(), 1));

            offset += &ty.size();
        }

        // Final size needs to be aligned to the struct's alignment
        offset = self.align_offset(offset, max_align);

        // Set the typedef's alignment
        typedef.align = Some(max_align);
        self.typedefs.push(typedef.clone());
        self.struct_map.insert(
            t.name.clone(),
            (
                qbe::Type::Aggregate(Box::leak(Box::new(typedef.clone()))),
                meta,
                offset,
            ),
        );
        self.buf.push_str(&format!("{}\n", typedef));

        Ok(())
    }

    pub fn generate(&'a mut self) -> Result<String> {
        for (_, t) in self.env.items.clone().iter() {
            match t {
                Entry::Type(ty) => {
                    self.generate_typedef(&ty)?;
                }
                Entry::Func(f) => {
                    if f.is_checked {
                        self.generate_function(&f)?;
                    }
                }
                _ => {}
            }
        }
        //self.generate_function(&self.env.items.get(&quantifier!(Root, Func("main"), End)).unwrap().to_func())?;

        for def in &self.datadefs {
            //self.module.add_data(def.clone());
            self.buf.push_str(&format!("{}\n", def));
        }

        Ok(self.buf.clone())
    }
}
