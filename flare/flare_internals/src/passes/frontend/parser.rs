use std::{marker::PhantomData, num::NonZero};

use crate::resource::{
    errors::CompResult,
    rep::{
        common::{FlareSpan, Spanned, Syntax},
        frontend::{
            ast::{BinOp, Label, Untyped},
            cst::{
                CstExpr, Field, FieldDef, FieldMacro, MatchArm, Package, PackageCollection,
                Pattern, UntypedCst,
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
    // LetExpression,
    // IfExpression,
    MatchExpression,
    PropAccess,
    PropQualifier,
    FieldAccess,
    SumConstructor,
    BinExpression,
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
    ReturnMacro,
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
#[forbid(dead_code)]
pub enum FK {
    Name,
    Arg,
    Type,
    Expr,
    Value,
    Pattern,
    Field,
    Func,
    Left,
    Right,
    Operator,
    Generics,
    Data,
    Import,
    Sigil,
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
        use FK::{
            Arg, Expr, Field, Func, Generics, Import, Left, Name, Operator, Pattern, Right, Sigil,
            Type, Value,
        };
        use NK::{
            ArrowType, BinExpression, Boolean, CallExpression, ExtendMacro, FieldAccess,
            FieldAssignment, FieldedConstructor, GenericType, GroupedType, Identifier, Lambda,
            MatchExpression, Number, ParenthesizedExpression, Path, PatternAlias, PatternAtom,
            PatternPath, PatternProduct, PatternVariable, PatternVariant, PrimitiveType,
            ProductType, PropAccess, PropQualifier, ReturnMacro, SelfType, SourceFile, StringNode,
            SumConstructor, SumType, UnitExpr, UseMacro, UserType,
        };
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
        k!(MatchExpression, "match_expression");
        k!(PropAccess, "prop_access");
        k!(PropQualifier, "prop_qualifier");
        k!(FieldAccess, "field_access");
        k!(SumConstructor, "sum_constructor");
        k!(BinExpression, "binary_expression");
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
        k!(ReturnMacro, "return_macro");
        k!(ExtendMacro, "extend_macro");
        k!(ArrowType, "arrow_type");
        k!(ProductType, "product_type");
        k!(SumType, "sum_type");
        k!(GenericType, "generic_type");
        k!(UserType, "user_type");
        k!(PrimitiveType, "primitive_type");
        k!(SelfType, "self_type");
        k!(GroupedType, "grouped_type");

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
        f!(Value, "value");
        f!(Pattern, "pattern");
        f!(Field, "field");
        f!(Func, "func");
        f!(Left, "left");
        f!(Right, "right");
        f!(Operator, "op");
        f!(Generics, "generics");
        f!(Import, "import");
        f!(Sigil, "sigil");
        Self { kinds, fields }
    }

    #[inline]
    pub const fn k(&self, nk: NK) -> u16 {
        self.kinds[nk as usize]
    }

    #[inline]
    pub const fn f(&self, fk: FK) -> u16 {
        self.fields[fk as usize]
    }
}

pub struct Translate<'src> {
    file: &'src FileSource,
    defs: FxHashMap<CstExpr<UntypedCst>, Vec<FieldMacro<UntypedCst>>>,
    _phantom: PhantomData<Node<'src>>,
    ids: &'src LangIds, // shared, constructed once per process
    errors: FxHashMap<FlareSpan, String>,
    current_path: Vec<<UntypedCst as Syntax>::Name>,
}

impl<'src> Translate<'src> {
    fn new(ids: &'src LangIds, file: &'src FileSource) -> Self {
        Self {
            file,
            defs: FxHashMap::default(),
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

                Package { root_node: def }
            })
            .collect()
    }

    fn lower_expr(&mut self, node: Node<'src>) -> Spanned<Intern<CstExpr<UntypedCst>>> {
        let expr = {
            let k = node.kind_id();

            match k {
                // ── Control flow ─────────────────────────────────────────────────
                k if k == self.ids.k(NK::MatchExpression) => self.lower_match(node),

                // // ── Abstraction / application ─────────────────────────────────────
                k if k == self.ids.k(NK::Lambda) => self.lower_lambda(node),
                k if k == self.ids.k(NK::CallExpression) => self.lower_call(node),

                // // ── Row / nominal dispatch ────────────────────────────────────────
                // k if k == self.ids.k(NK::PropAccess) => self.lower_prop_access(node),
                k if k == self.ids.k(NK::FieldAccess) => self.lower_field_access(node),

                // // ── Binary ops ───────────────────────────────────────────────────
                k if k == self.ids.k(NK::BinExpression) => self.lower_binop(&node),

                // ── Constructors ─────────────────────────────────────────────────
                k if k == self.ids.k(NK::FieldedConstructor) => self.lower_record(node),
                k if k == self.ids.k(NK::SumConstructor) => self.lower_variant(node),

                // ── Transparent wrapper ───────────────────────────────────────────
                k if k == self.ids.k(NK::ParenthesizedExpression) => {
                    // Strip the parens — span of the inner expr is more useful
                    // than the span of the parens for diagnostics
                    *self.lower_expr(node.named_child(0).unwrap()).0
                }

                // ── Primaries ────────────────────────────────────────────────────
                k if k == self.ids.k(NK::Identifier) => CstExpr::Ident(Untyped(self.name(&node))),

                k if k == self.ids.k(NK::Number) => {
                    let text = self.raw_name(&node);
                    if let Ok(v) = text.parse::<f32>() {
                        CstExpr::Number(OrderedFloat::from(v))
                    } else {
                        self.error(node, "malformed number literal");
                        CstExpr::Hole(Untyped(self.si(node, |_| text.to_string())))
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

    fn lower_variant(&mut self, node: Node<'src>) -> CstExpr<UntypedCst> {
        let name = self.get_child(node, FK::Name).unwrap();
        let name = self.name(&name);
        let value = self.get_child(node, FK::Expr);
        let value = value.map(|value| self.lower_expr(value));
        CstExpr::VariantConstructor { name, value }
    }

    fn lower_record(&mut self, node: Node<'src>) -> CstExpr<UntypedCst> {
        let mut cursor = node.walk();
        let children = node.children(&mut cursor);
        // dbg!(&children);
        let fields: Vec<Field<UntypedCst>> = children
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
                    k if k == self.ids.k(NK::UseMacro) => Some(self.add_use_macro(child)),
                    k if k == self.ids.k(NK::ReturnMacro) => Some(self.add_return_macro(child)),
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

    fn translate_field_assignment(&mut self, node: Node<'src>) -> Field<UntypedCst> {
        // Determine name and params from the two structural variants
        let (name, args) = {
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
        let value = if args.is_empty() {
            value
        } else {
            args.into_iter()
                .map(|n| (n, self.raw_name(&n)))
                .fold(value, |body, (node, name)| {
                    value.map(|value| {
                        CstExpr::Lambda(Untyped(self.si(node, |_| name.to_string())), body)
                    })
                })
        };

        Field::Def(FieldDef { name, ty, value })
    }

    fn collapse_current_path(&self) -> CstExpr<UntypedCst> {
        let first = *self.current_path.last().unwrap();
        let init = first.convert(CstExpr::Ident(Untyped(first)));
        let expr = self
            .current_path
            .iter()
            .fold(init, |l, r| r.convert(CstExpr::FieldAccess(l, Label(*r))));
        *expr.0
    }

    fn add_use_macro(&mut self, node: Node<'src>) -> Field<UntypedCst> {
        let path = self.collapse_current_path();
        let import_node = self.get_field(&node, FK::Import).unwrap();
        let expr = self.lower_pattern(import_node);
        let the_macro = FieldMacro::Import(expr);
        Field::Macro(the_macro)
    }

    fn add_return_macro(&mut self, node: Node<'src>) -> Field<UntypedCst> {
        let path = self.collapse_current_path();
        let mut cursor = node.walk();
        dbg!(node.children(&mut cursor).collect::<Vec<_>>());
        let expr_node = node.child(1).unwrap();
        let expr = self.lower_expr(expr_node);
        let the_macro = FieldMacro::Ret(expr);
        Field::Macro(the_macro)
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
                        self.lower_type(node.child_by_field_id(self.ids.f(FK::Left)).unwrap());
                    let result =
                        self.lower_type(node.child_by_field_id(self.ids.f(FK::Right)).unwrap());
                    CstType::Func(param, result)
                }
                k if k == self.ids.k(NK::ProductType) => self.lower_product_type(node),
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

    fn lower_product_type(&self, node: Node<'src>) -> CstType {
        todo!()
    }

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

    fn lower_lambda(&mut self, node: Node<'src>) -> CstExpr<UntypedCst> {
        let args = self.get_children_by(node, FK::Arg);

        let value = self
            .get_child(node, FK::Expr)
            .map(|node| self.lower_expr(node))
            .unwrap();

        // Desugar function sugar immediately
        let value = if args.is_empty() {
            value
        } else {
            args.into_iter()
                .map(|n| (n, self.raw_name(&n)))
                .fold(value, |body, (node, name)| {
                    value.map(|value| {
                        CstExpr::Lambda(Untyped(self.si(node, |_| name.to_string())), body)
                    })
                })
        };
        *value.0
    }

    fn lower_binop(&mut self, node: &Node<'src>) -> CstExpr<UntypedCst> {
        let left_node = node.child_by_field_id(self.ids.f(FK::Left)).unwrap();
        let op_node = node.child_by_field_id(self.ids.f(FK::Operator)).unwrap();
        let right_node = node.child_by_field_id(self.ids.f(FK::Right)).unwrap();
        let left = self.lower_expr(left_node);
        let right = self.lower_expr(right_node);
        CstExpr::Bin(
            left,
            match op_node.to_string().as_str() {
                "+" => BinOp::Add,
                _ => todo!(),
            },
            right,
        )
    }

    fn lower_call(&mut self, node: Node<'src>) -> CstExpr<UntypedCst> {
        let func = self.lower_expr(node.child_by_field_id(self.ids.f(FK::Func)).unwrap());
        let arg = self.lower_expr(node.child_by_field_id(self.ids.f(FK::Expr)).unwrap());
        CstExpr::Call(func, arg)
    }

    fn lower_field_access(&mut self, node: Node<'src>) -> CstExpr<UntypedCst> {
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
                    let tag = self.get_field(&node, FK::Name).unwrap();
                    let name = self.name(&tag);
                    let label = Label(name);
                    let payload = node
                        .named_children(&mut node.walk())
                        // skip the variant_name child, take the rest as payload
                        .nth(1)
                        .map_or(self.si(node, |_| Pattern::Unit), |n| self.lower_pattern(n));
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
        .expect("Could not set language");
    let tree = parser.parse(&file.source, None).unwrap();
    Ok(translate_tree_sitter(&tree, file))
}
