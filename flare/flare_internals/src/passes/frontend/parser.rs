use std::{marker::PhantomData, num::NonZero};

use crate::resource::{
    errors::{CompResult, CompilerErr, DynamicErr, ErrorCollection},
    rep::{
        common::{FlareSpan, Spanned, Syntax},
        frontend::{
            ast::{BinOp, Label, Untyped},
            cst::{
                CstExpr, FieldDef, Macro, MatchArm, Package, PackageCollection, Pattern, UntypedCst,
            },
            csttypes::CstType,
            files::FileSource,
        },
    },
};

use rustc_hash::FxHashMap;

use internment::Intern;
use ordered_float::OrderedFloat;

use tree_sitter::{Language, Node, Parser, Tree};

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
#[repr(usize)]
pub enum NK {
    // Declarations
    SourceFile,
    FieldAssignment,
    FieldedConstructor,
    // Expressions
    CallExpression,
    Lambda,
    LetExpression,
    IfExpression,
    MatchExpression,
    PropAccess,
    PropQualifier,
    FieldAccess,
    SumConstructor,
    MulExpression,
    DivExpression,
    AddExpression,
    SubExpression,
    CmpExpression,
    ParenthesizedExpression,
    // Primaries
    Identifier,
    Path,
    Number,
    StringNode,
    Boolean,
    UnitExpr,
    // Patterns
    PatternProduct,
    PatternVariable,
    PatternVariant,
    PatternPath,
    PatternAlias,
    PatternAtom,
    // Macros
    UseMacro,
    ExternMacro,
    TypeMacro,
    ExtendMacro,
    // Types
    ArrowType,
    ProductType,
    SumType,
    GenericType,
    UserType,
    PrimitiveType,
    SelfType,
    GroupedType,
    // sentinel — must be last
    COUNT,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
#[repr(usize)]
pub enum FK {
    Name,
    Arg,
    Value,
    Body,
    Expr,
    Type,
    Parameter,
    Return,
    Condition,
    Consequence,
    Alternative,
    Pattern,
    Func,
    Field,
    Macro,
    Assignment,
    Callee,
    Left,
    Right,
    Operator,
    Generics,
    Data,
    Import,
    Implementor,
    Spec,
    TheType,
    Sigil,
    VariantName,
    // sentinel
    COUNT,
}
#[derive(Debug)]
pub struct LangIds {
    kinds: [u16; NK::COUNT as usize],
    fields: [u16; FK::COUNT as usize],
}

impl LangIds {
    pub fn new(lang: &Language) -> Self {
        use NK::*;
        let mut kinds = [0u16; NK::COUNT as usize];

        macro_rules! k {
            ($variant:expr, $name:expr) => {
                kinds[$variant as usize] = lang.id_for_node_kind($name, true);
            };
        }

        k!(SourceFile, "source_file");
        k!(FieldAssignment, "field_assignment");
        k!(FieldedConstructor, "fielded_constructor");
        k!(CallExpression, "call_expression");
        k!(Lambda, "lambda");
        k!(LetExpression, "let_expression");
        k!(IfExpression, "if_expression");
        k!(MatchExpression, "match_expression");
        k!(PropAccess, "prop_access");
        k!(PropQualifier, "prop_qualifier");
        k!(FieldAccess, "field_access");
        k!(SumConstructor, "sum_constructor");
        k!(MulExpression, "mul_expression");
        k!(DivExpression, "div_expression");
        k!(AddExpression, "add_expression");
        k!(SubExpression, "sub_expression");
        k!(CmpExpression, "cmp_expression");
        k!(ParenthesizedExpression, "parenthesized_expression");
        k!(Identifier, "identifier");
        k!(Path, "path");
        k!(Number, "number");
        k!(StringNode, "string");
        k!(Boolean, "boolean");
        k!(UnitExpr, "unit_expr");
        k!(PatternProduct, "pattern_product");
        k!(PatternVariable, "pattern_variable");
        k!(PatternVariant, "pattern_variant");
        k!(PatternPath, "pattern_path");
        k!(PatternAlias, "pattern_alias");
        k!(PatternAtom, "pattern_atom");
        k!(UseMacro, "use_macro");
        k!(ExternMacro, "extern_macro");
        k!(TypeMacro, "type_macro");
        k!(ExtendMacro, "extend_macro");
        k!(ArrowType, "arrow_type");
        k!(ProductType, "product_type");
        k!(SumType, "sum_type");
        k!(GenericType, "generic_type");
        k!(UserType, "user_type");
        k!(PrimitiveType, "primitive_type");
        k!(SelfType, "self_type");
        k!(GroupedType, "grouped_type");

        use FK::*;
        let mut fields = [0u16; FK::COUNT as usize];
        macro_rules! f {
            ($variant:expr, $name:expr) => {
                fields[$variant as usize] = lang
                    .field_id_for_name($name)
                    .unwrap_or_else(|| panic!("missing field '{}'", $name))
                    .into();
            };
        }
        // dbg!(lang.field_id_for_name("function_field"));

        f!(Name, "name");
        f!(Arg, "arg");
        f!(Type, "type");
        f!(Expr, "expr");
        // f!(FunctionField, "function_field");
        // f!(ValField, "val_field");
        f!(Body, "body");
        f!(Value, "value");
        f!(Parameter, "parameter");
        f!(Return, "return");
        f!(Condition, "condition");
        f!(Consequence, "consequence");
        f!(Alternative, "alternative");
        f!(Pattern, "pattern");
        f!(Field, "field");
        f!(Callee, "callee");
        f!(Func, "func");
        f!(Left, "left");
        f!(Right, "right");
        f!(Operator, "operator");
        f!(Generics, "generics");
        f!(Data, "data");
        f!(Import, "import");
        f!(Implementor, "implementor");
        f!(Spec, "spec");
        f!(TheType, "the_type");
        f!(Sigil, "sigil");
        f!(VariantName, "variant_name");
        Self { kinds, fields }
    }

    #[inline(always)]
    pub const fn k(&self, nk: NK) -> u16 {
        self.kinds[nk as usize]
    }

    #[inline(always)]
    pub const fn f(&self, fk: FK) -> u16 {
        self.fields[fk as usize]
    }
}

pub struct Translate<'src> {
    file: &'src FileSource,
    defs: FxHashMap<CstExpr<UntypedCst>, Vec<Macro<UntypedCst>>>,
    _phantom: PhantomData<Node<'src>>,
    ids: &'src LangIds, // shared, constructed once per process
    errors: FxHashMap<FlareSpan, String>,
    current_path: Vec<<UntypedCst as Syntax>::Name>,
}

impl<'src> Translate<'src> {
    fn new(ids: &'src LangIds, file: &'src FileSource) -> Self {
        Self {
            file,
            defs: Default::default(),
            _phantom: PhantomData,
            ids,
            errors: FxHashMap::default(),
            current_path: Vec::new(),
        }
    }

    fn raw_name(&self, node: &Node<'src>) -> &'src str {
        node.utf8_text(self.file.source.as_bytes()).unwrap()
    }

    fn name(&self, node: &Node<'src>) -> Spanned<Intern<String>> {
        let name = {
            node.utf8_text(self.file.source.as_bytes())
                .unwrap()
                .to_string()
        };

        self.si(*node, |_| name)
    }

    // /// Helper: wrap a node's lowered value in its span
    // fn s<T>(&self, node: Node, f: impl FnOnce(Node) -> T) -> Spanned<T> {
    //     Spanned(f(node), self.span(node))
    // }

    /// Helper: wrap a node's lowered value in its span
    fn si<T: Sync + Send + std::hash::Hash + Eq>(
        &self,
        node: Node,
        f: impl FnOnce(Node) -> T,
    ) -> Spanned<Intern<T>> {
        Spanned(Intern::new(f(node)), self.span(node))
    }

    fn span(&self, node: Node) -> FlareSpan {
        FlareSpan(node.start_byte(), node.end_byte(), self.file.id)
    }

    fn get_field(&self, node: &Node<'src>, fk: FK) -> Option<Node<'src>> {
        node.child_by_field_id(self.ids.f(fk))
    }

    fn get_children_by(&self, node: Node<'src>, fk: FK) -> Vec<Node<'src>> {
        let mut cur = node.walk();
        node.children_by_field_id(NonZero::new(self.ids.f(fk)).unwrap(), &mut cur)
            // .map(|n| self.intern(n))
            .collect::<Vec<_>>()
    }

    fn get_child(&self, node: Node<'src>, fk: FK) -> Option<Node<'src>> {
        node.child_by_field_id(self.ids.f(fk))
    }

    fn error(&mut self, node: Node<'src>, msg: &(impl ToString + ?Sized)) {
        let span = self.span(node);
        self.errors.insert(span, msg.to_string());
    }

    fn translate_module(&mut self, root: Node<'src>) -> Vec<Package<UntypedCst>> {
        let mut cursor = root.walk();
        root.named_children(&mut cursor)
            .filter(|n| n.kind_id() == self.ids.k(NK::FieldAssignment))
            .map(|n| {
                let def = self.translate_field_assignment(n);

                Package {
                    macros: std::mem::take(&mut self.defs),
                    root_node: def,
                }
            })
            .collect()
    }

    fn lower_expr(&mut self, node: Node<'src>) -> Spanned<Intern<CstExpr<UntypedCst>>> {
        let expr = {
            let k = node.kind_id();
            match k {
                // ── Control flow ─────────────────────────────────────────────────
                // k if k == self.ids.k(NK::LetExpression) => self.lower_let(node),
                // k if k == self.ids.k(NK::IfExpression) => self.lower_if(node),
                k if k == self.ids.k(NK::MatchExpression) => self.lower_match(node),

                // // ── Abstraction / application ─────────────────────────────────────
                // k if k == self.ids.k(NK::Lambda) => self.lower_lambda(node),
                k if k == self.ids.k(NK::CallExpression) => self.lower_call(node),

                // // ── Row / nominal dispatch ────────────────────────────────────────
                // k if k == self.ids.k(NK::PropAccess) => self.lower_prop_access(node),
                k if k == self.ids.k(NK::FieldAccess) => self.lower_field_access(node),

                // // ── Binary ops ───────────────────────────────────────────────────
                // k if k == self.ids.k(NK::MulExpression) => self.lower_binop(node, BinOp::Mul),
                // k if k == self.ids.k(NK::DivExpression) => self.lower_binop(node, BinOp::Div),
                // k if k == self.ids.k(NK::AddExpression) => self.lower_binop(node, BinOp::Add),
                // k if k == self.ids.k(NK::SubExpression) => self.lower_binop(node, BinOp::Sub),
                // k if k == self.ids.k(NK::CmpExpression) => self.lower_cmp(node),

                // ── Constructors ─────────────────────────────────────────────────
                k if k == self.ids.k(NK::FieldedConstructor) => self.lower_record(node),
                // k if k == self.ids.k(NK::SumConstructor) => self.lower_variant(node),

                // ── Transparent wrapper ───────────────────────────────────────────
                k if k == self.ids.k(NK::ParenthesizedExpression) => {
                    // Strip the parens — span of the inner expr is more useful
                    // than the span of the parens for diagnostics
                    *self.lower_expr(node.named_child(0).unwrap()).0
                }

                // ── Primaries ────────────────────────────────────────────────────
                k if k == self.ids.k(NK::Identifier) => CstExpr::Ident(Untyped(self.name(&node))),
                // k if k == self.ids.k(NK::Path) => self.lower_path(node),
                k if k == self.ids.k(NK::Number) => {
                    let text = self.raw_name(&node);
                    match text.parse::<f32>() {
                        Ok(v) => CstExpr::Number(OrderedFloat::from(v)),
                        Err(_) => {
                            self.error(node, "malformed number literal");
                            CstExpr::Hole(Untyped(self.si(node, |_| text.to_string())))
                        }
                    }
                }
                k if k == self.ids.k(NK::StringNode) => CstExpr::String(self.name(&node)),
                k if k == self.ids.k(NK::Boolean) => CstExpr::Bool(self.raw_name(&node) == "true"),
                k if k == self.ids.k(NK::UnitExpr) => CstExpr::Unit,

                _ => {
                    self.error(
                        node,
                        &format!("unexpected node '{}' in expression position", node.kind()),
                    );
                    CstExpr::Hole(Untyped(self.name(&node)))
                }
            }
        };
        self.si(node, |_| expr)
    }

    fn lower_record(&mut self, node: Node<'src>) -> CstExpr<UntypedCst> {
        let mut cursor = node.walk();
        let children = node.children(&mut cursor);
        // dbg!(&children);
        let fields: Vec<FieldDef<UntypedCst>> = children
            .into_iter()
            .filter_map(|child| {
                if !child.is_named() {
                    return None;
                }
                // dbg!(child.to_sexp());
                let k = child.kind_id();
                match k {
                    k if k == self.ids.k(NK::FieldAssignment) => {
                        Some(self.translate_field_assignment(child))
                    }
                    k if k == self.ids.k(NK::UseMacro) => {
                        self.add_use_macro(child);
                        None
                    }
                    k if k == self.ids.k(NK::TypeMacro) => {
                        self.add_type_macro(child);
                        None
                    }
                    _ => {
                        println!("extra: {}", child.to_sexp());
                        None
                    }
                }
            })
            .collect();

        CstExpr::ProductConstructor {
            fields: fields.as_slice().into(),
        }
    }

    fn translate_field_assignment(&mut self, node: Node<'src>) -> FieldDef<UntypedCst> {
        // Determine name and params from the two structural variants
        let (name, params) = {
            let name = self.get_child(node, FK::Name).unwrap();
            let args = self.get_children_by(node, FK::Arg);
            (self.name(&name), args)
        };
        self.current_path.push(name);

        let ty = self
            .get_child(node, FK::Type)
            .map(|node| self.lower_type(node));
        let value = self
            .get_child(node, FK::Expr)
            .map(|node| self.lower_expr(node))
            .unwrap();

        // Desugar function sugar immediately
        let value = if !params.is_empty() {
            params
                .into_iter()
                .map(|n| (n, self.raw_name(&n)))
                .fold(value, |body, (node, name)| {
                    value.map(|value| {
                        CstExpr::Lambda(Untyped(self.si(node, |_| name.to_string())), body)
                    })
                })
        } else {
            value
        };

        FieldDef {
            name,
            // params: (&[]).into(),
            ty,
            value,
        }
    }

    fn collapse_current_path(&self) -> CstExpr<UntypedCst> {
        let first = *self.current_path.last().unwrap();
        let init = first.convert(CstExpr::Ident(Untyped(first)));
        let expr = self
            .current_path
            .iter()
            .fold(init, |l, r| r.convert(CstExpr::FieldAccess(l, Label(*r))));
        // dbg!(expr);
        *expr.0
    }

    fn add_use_macro(&mut self, node: Node<'src>) {
        let path = self.collapse_current_path();
        let import_node = self.get_field(&node, FK::Import).unwrap();
        let expr = self.lower_pattern(import_node);
        let the_macro = Macro::Import(expr);
        self.defs
            .entry(path)
            .and_modify(|v| v.push(the_macro.clone()))
            .or_insert(vec![the_macro]);
    }

    fn add_type_macro(&mut self, node: Node<'src>) {
        let path = self.collapse_current_path();
        let key_ty_node = self.get_field(&node, FK::Name).unwrap();
        let key_ty = self.lower_type(key_ty_node);

        let defn_ty_node = self.get_field(&node, FK::TheType).unwrap();
        let defn_ty = self.lower_type(defn_ty_node);
        let the_macro = Macro::Type(key_ty, defn_ty);
        self.defs
            .entry(path)
            .and_modify(|v| v.push(the_macro.clone()))
            .or_insert(vec![the_macro]);
    }

    fn lower_type(&mut self, node: Node<'src>) -> Spanned<Intern<CstType>> {
        let ty = {
            let k = node.kind_id();
            match k {
                k if k == self.ids.k(NK::PrimitiveType) => self.lower_primitive_type(&node),
                k if k == self.ids.k(NK::UserType) => self.lower_user_type(node),
                k if k == self.ids.k(NK::GenericType) => {
                    // ?a — the identifier is the only named child
                    CstType::Generic(
                        self.name(&node.child_by_field_id(self.ids.f(FK::Name)).unwrap()),
                    )
                }
                k if k == self.ids.k(NK::ArrowType) => {
                    let param =
                        self.lower_type(node.child_by_field_id(self.ids.f(FK::Parameter)).unwrap());
                    let result =
                        self.lower_type(node.child_by_field_id(self.ids.f(FK::Return)).unwrap());
                    CstType::Func(param, result)
                }
                // k if k == self.k_product_type => self.lower_product_type(node),
                // k if k == self.k_sum_type => self.lower_sum_type(node),
                // k if k == self.k_self_type => CstType::SelfType,
                // k if k == self.k_grouped_type => {
                //     // transparent paren — unwrap
                //     self.lower_type(node.named_child(0).unwrap()).node
                // }
                _ => {
                    self.error(node, "unexpected type node");
                    CstType::Hole
                }
            }
        };
        self.si(node, |node| ty)
    }

    fn lower_primitive_type(&self, node: &Node<'src>) -> CstType {
        let t = self.raw_name(node);
        match t {
            "num" => CstType::Num,
            "bool" => CstType::Bool,
            "str" => CstType::String,
            "unit" => CstType::Unit,
            _ => panic!("Invalid primitive type: {t}"),
        }
    }

    fn lower_user_type(&mut self, node: Node<'src>) -> CstType {
        let name_node = self.get_child(node, FK::Name).unwrap();
        let name = self.name(&name_node);
        let generics: Vec<_> = self
            .get_children_by(node, FK::Generics)
            .into_iter()
            .map(|node| self.lower_type(node))
            .collect();
        let generics: &[Spanned<Intern<CstType>>] = generics.leak();
        CstType::User(name, generics)
    }

    // fn lower_let(&mut self, node: Node<'src>) -> CstExpr<UntypedCst> {
    //     let pattern = self.lower_pattern(self.get_child(node, FK::Pattern).unwrap());
    //     let value = self.lower_expr(self.get_child(node, FK::Value).unwrap());
    //     let body = self.lower_expr(self.get_child(node, FK::Value).unwrap());
    //     // CstExpr::Let(pattern, value, body)
    // }

    // fn lower_if(&mut self, node: Node<'src>) -> CstExpr<UntypedCst> {
    //     let cond = self.lower_expr(self.get_child(node, FK::Condition).unwrap());
    //     let cons = self.lower_expr(self.get_child(node, FK::Consequence).unwrap());
    //     let altr = self.lower_expr(self.get_child(node, FK::Alternative).unwrap());
    //     CstExpr::If(cond, cons, altr)
    // }

    fn lower_match(&mut self, node: Node<'src>) -> CstExpr<UntypedCst> {
        let matchee = self.lower_expr(self.get_child(node, FK::Value).unwrap());
        let patterns = self.get_children_by(node, FK::Pattern);
        let exprs = self.get_children_by(node, FK::Expr);
        let arms: Vec<_> = patterns
            .into_iter()
            .zip(exprs)
            .map(|(pat_node, expr_node)| {
                let pat = self.lower_pattern(pat_node);
                let body = self.lower_expr(expr_node);
                MatchArm { pat, body }
            })
            .collect();

        CstExpr::Match(matchee, arms.leak())
    }

    fn lower_call(&mut self, node: Node<'src>) -> CstExpr<UntypedCst> {
        let func = self.lower_expr(node.child_by_field_id(self.ids.f(FK::Func)).unwrap());
        let arg = self.lower_expr(node.child_by_field_id(self.ids.f(FK::Expr)).unwrap());
        CstExpr::Call(func, arg)
    }

    fn lower_field_access(&mut self, node: Node<'src>) -> CstExpr<UntypedCst> {
        dbg!(node.to_sexp());
        let base = self.lower_expr(self.get_child(node, FK::Expr).unwrap());
        let field = self.name(&self.get_child(node, FK::Field).unwrap());

        CstExpr::FieldAccess(base, Label(field))
    }

    fn lower_pattern(&mut self, node: Node<'src>) -> Spanned<Intern<Pattern<UntypedCst>>> {
        let pat = {
            let k = node.kind_id();
            match k {
                k if k == self.ids.k(NK::PatternVariable) => {
                    Pattern::Var(Untyped(self.name(&node.named_child(0).unwrap())))
                }
                k if k == self.ids.k(NK::PatternProduct) => self.lower_pattern_product(node),
                k if k == self.ids.k(NK::PatternVariant) => {
                    let tag = self.get_field(&node, FK::VariantName).unwrap();
                    let name = self.name(&tag);
                    let label = Label(name);
                    let payload = node
                        .named_children(&mut node.walk())
                        // skip the variant_name child, take the rest as payload
                        .nth(1)
                        .map(|n| self.lower_pattern(n))
                        .unwrap_or(self.si(node, |_| Pattern::Unit));
                    Pattern::Variant(label, payload)

                    // Pattern::Variant { tag, payload }
                }
                // k if k == self.k_pattern_path => {
                //     // pattern "." terminal — two named children
                //     let lhs = self.lower_pattern(node.named_child(0).unwrap());
                //     let rhs = self.lower_pattern(node.named_child(1).unwrap());
                //     Pattern::Path(Box::new(lhs), Box::new(rhs))
                // }
                // k if k == self.k_pattern_alias => {
                //     let pat = self.lower_pattern(node.named_child(0).unwrap());
                //     let name = self.intern(node.named_child(1).unwrap());
                //     Pattern::Alias {
                //         pattern: Box::new(pat),
                //         name,
                //     }
                // }
                // k if k == self.k_pattern_atom => {
                //     let inner = node.named_child(0).unwrap();
                //     if inner.kind_id() == self.k_number {
                //         let v = inner.utf8_text(self.src).unwrap().parse().unwrap();
                //         Pattern::Atom(Atom::Num(v))
                //     } else {
                //         Pattern::Atom(Atom::Str(self.extract_string(inner)))
                //     }
                // }
                _ => {
                    self.error(node, "unexpected pattern node");
                    Pattern::Hole(Untyped(self.name(&node)))
                }
            }
        };
        self.si(node, |_| pat)
    }

    fn lower_pattern_product(&mut self, node: Node<'src>) -> Pattern<UntypedCst> {
        // { p1, p2 } — named children are _pattern_product_elem,
        // each of which wraps exactly one pattern
        let mut cursor = node.walk();
        let pats = node
            .named_children(&mut cursor)
            .map(|elem| {
                // _pattern_product_elem has one named child: the pattern itself
                self.lower_pattern(elem.named_child(0).unwrap())
            })
            .collect::<Vec<_>>();
        todo!()
        // Pattern::Record {
        // fields: pats,
        // open: false,
        // }
    }
}

fn translate_tree_sitter(tree: &Tree, file: &FileSource) -> PackageCollection<UntypedCst> {
    let root = tree.root_node();
    let ids = LangIds::new(&tree_sitter::Language::new(tree_sitter_flare::LANGUAGE));
    let mut lowerer = Translate::new(&ids, file);
    let packages = lowerer
        .translate_module(root)
        .into_iter()
        .map(|p| (p, file.id))
        .collect();
    PackageCollection { packages }
}

/// Public parsing function. Produces a parse tree from a source string.
pub fn parse(file: &FileSource) -> CompResult<PackageCollection<UntypedCst>> {
    let mut parser = Parser::new();
    parser
        .set_language(&tree_sitter_flare::LANGUAGE.into())
        .unwrap();
    let tree = parser.parse(&file.source, None).unwrap();
    Ok(translate_tree_sitter(&tree, file))
}
