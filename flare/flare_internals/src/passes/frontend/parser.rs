use std::{marker::PhantomData, num::NonZero};

use crate::resource::{
    errors::{CompResult, CompilerErr, DynamicErr, ErrorCollection},
    rep::{
        common::{FlareSpan, Spanned},
        frontend::{
            ast::{BinOp, Untyped},
            cst::{CstExpr, FieldDef, Macro, Package, PackageCollection, UntypedCst},
            csttypes::CstType,
            files::{FileID, FileSource},
        },
    },
};

use rustc_hash::FxHashMap;
use tree_sitter_flare::NODE_TYPES;

use internment::Intern;
// use lasso::{Interner, Rodeo};
use ordered_float::OrderedFloat;

use tree_sitter::{InputEdit, Language, Node, Parser, Point, Tree};

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
    Type,
    FunctionField,
    ValField,
    Parameter,
    Return,
    Body,
    Condition,
    Consequence,
    Alternative,
    Pattern,
    Function,
    Argument,
    Object,
    Field,
    Callee,
    Func,
    Left,
    Right,
    Operator,
    VariantName,
    VariantData,
    Import,
    Implementor,
    Spec,
    TheType,
    Sigil,
    FieldName,
    FieldTy,
    // sentinel
    COUNT,
}

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

        f!(Name, "name");
        f!(Arg, "arg");
        f!(Value, "value");
        f!(Type, "type");
        f!(FunctionField, "function_field");
        f!(ValField, "val_field");
        f!(Parameter, "parameter");
        f!(Body, "body");
        f!(Condition, "condition");
        f!(Consequence, "consequence");
        f!(Alternative, "alternative");
        f!(Pattern, "pattern");
        f!(Function, "function");
        f!(Argument, "argument");
        f!(Object, "object");
        f!(Field, "field");
        f!(Callee, "callee");
        f!(Func, "func");
        f!(Left, "left");
        f!(Right, "right");
        f!(Operator, "operator");
        f!(VariantName, "variant_name");
        f!(VariantData, "variant_data");
        f!(Import, "import");
        f!(Implementor, "implementor");
        f!(Spec, "spec");
        f!(TheType, "the_type");
        f!(Sigil, "sigil");
        f!(FieldName, "field_name");
        f!(FieldTy, "field_ty");

        Self { kinds, fields }
    }

    #[inline(always)]
    pub fn k(&self, nk: NK) -> u16 {
        self.kinds[nk as usize]
    }

    #[inline(always)]
    pub fn f(&self, fk: FK) -> u16 {
        self.fields[fk as usize]
    }
}

pub struct Translate<'src> {
    file: &'src FileSource,
    defs: FxHashMap<CstExpr<UntypedCst>, Vec<Macro<UntypedCst>>>,
    _phantom: PhantomData<Node<'src>>,
    ids: &'src LangIds, // shared, constructed once per process
}

impl<'src> Translate<'src> {
    fn new(ids: &'src LangIds, file: &'src FileSource) -> Self {
        Self {
            file,
            defs: Default::default(),
            _phantom: PhantomData,
            ids,
        }
    }

    fn name(&self, node: &Node<'src>) -> &'src str {
        node.utf8_text(self.file.source.as_bytes()).unwrap()
    }

    /// Helper: wrap a node's lowered value in its span
    fn s<T>(&self, node: Node, f: impl FnOnce(Node) -> T) -> Spanned<T> {
        Spanned(f(node), self.span(node))
    }

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

    fn translate_module(&mut self, root: Node<'src>) -> Vec<Package<UntypedCst>> {
        let mut cursor = root.walk();
        root.named_children(&mut cursor)
            .filter(|n| n.kind_id() == self.ids.k(NK::FieldAssignment))
            .map(|n| {
                let def = self.lower_expr(n);

                Package {
                    macros: std::mem::take(&mut self.defs),
                    root_node: FieldDef {
                        name: FlareSpan::default().with(String::from("$ROOT").into()),
                        params: vec![],
                        ty: None,
                        value: Some(def),
                    },
                }
            })
            .collect()
    }

    fn lower_expr(&self, node: Node<'src>) -> Spanned<Intern<CstExpr<UntypedCst>>> {
        self.si(node, |node| {
            let k = node.kind_id();
            match k {
                // ── Control flow ─────────────────────────────────────────────────
                k if k == self.ids.k(NK::LetExpression) => self.lower_let(node),
                k if k == self.ids.k(NK::IfExpression) => self.lower_if(node),
                k if k == self.ids.k(NK::MatchExpression) => self.lower_match(node),

                // ── Abstraction / application ─────────────────────────────────────
                k if k == self.ids.k(NK::Lambda) => self.lower_lambda(node),
                k if k == self.ids.k(NK::CallExpression) => self.lower_call(node),

                // ── Row / nominal dispatch ────────────────────────────────────────
                k if k == self.ids.k(NK::PropAccess) => self.lower_prop_access(node),
                k if k == self.ids.k(NK::FieldAccess) => self.lower_field_access(node),

                // ── Binary ops ───────────────────────────────────────────────────
                k if k == self.ids.k(NK::MulExpression) => self.lower_binop(node, BinOp::Mul),
                k if k == self.ids.k(NK::DivExpression) => self.lower_binop(node, BinOp::Div),
                k if k == self.ids.k(NK::AddExpression) => self.lower_binop(node, BinOp::Add),
                k if k == self.ids.k(NK::SubExpression) => self.lower_binop(node, BinOp::Sub),
                k if k == self.ids.k(NK::CmpExpression) => self.lower_cmp(node),

                // ── Constructors ─────────────────────────────────────────────────
                k if k == self.ids.k(NK::FieldedConstructor) => self.lower_record(node),
                k if k == self.ids.k(NK::SumConstructor) => self.lower_variant(node),

                // ── Transparent wrapper ───────────────────────────────────────────
                k if k == self.ids.k(NK::ParenthesizedExpression) => {
                    // Strip the parens — span of the inner expr is more useful
                    // than the span of the parens for diagnostics
                    self.lower_expr(node.named_child(0).unwrap()).node
                }

                // ── Primaries ────────────────────────────────────────────────────
                k if k == self.ids.k(NK::Identifier) => CstExpr::Ident(),
                k if k == self.ids.k(NK::Path) => self.lower_path(node),
                k if k == self.ids.k(NK::Number) => {
                    let text = node.utf8_text(self.src).unwrap();
                    match text.parse::<f64>() {
                        Ok(v) => Expr::Num(v),
                        Err(_) => {
                            self.error(node, "malformed number literal");
                            Expr::Num(0.0)
                        }
                    }
                }
                k if k == self.ids.k(NK::StringNode) => Expr::Str(self.extract_string(node)),
                k if k == self.ids.k(NK::Boolean) => {
                    Expr::Bool(node.utf8_text(self.src).unwrap() == "true")
                }
                k if k == self.ids.k(NK::UnitExpr) => Expr::Unit,

                _ => {
                    self.error(
                        node,
                        &format!("unexpected node '{}' in expression position", node.kind()),
                    );
                    CstExpr::Hole(node.utf8_text)
                }
            }
        })
    }

    fn translate_field_assignment(&self, node: Node<'src>) -> FieldDef<UntypedCst> {
        // Determine name and params from the two structural variants
        let (name, params) = if let Some(ff) = node.child_by_field_id(self.ids.f(FK::FunctionField))
        {
            // function_field: the sub-fields 'name' and 'arg' are on ff itself
            let name = ff.child_by_field_id(self.ids.f(FK::Name)).unwrap();
            let mut cur = ff.walk();
            let params = ff
                .children_by_field_id(NonZero::new(self.ids.f(FK::Arg)).unwrap(), &mut cur)
                // .map(|n| self.intern(n))
                .collect::<Vec<_>>();
            (name, params)
        } else {
            // val_field: it's just a bare identifier
            let vf = node.child_by_field_id(self.ids.f(FK::ValField)).unwrap();
            (vf, vec![])
        };

        let ty = node
            .child_by_field_id(self.ids.f(FK::FieldTy))
            .map(|n| self.lower_type(n));
        let value = node
            .child_by_field_id(self.ids.f(FK::Value))
            .map(|n| self.lower_expr(n));

        // Desugar function sugar immediately
        let value = if !params.is_empty() {
            value.map(|body| {
                params
                    .into_iter()
                    .map(|n| (n, self.name(&n)))
                    .fold(body, |expr, (node, name)| {
                        body.map(|body| {
                            CstExpr::Lambda(Untyped(self.s(node, |_| name.to_string())), expr)
                        })
                    })
            })
        } else {
            value
        };

        FieldDef {
            name,
            params: vec![],
            ty,
            value,
        }
    }

    fn lower_type(&self, node: Node<'src>) -> Spanned<Intern<CstType>> {
        self.si(node, |node| {
            let k = node.kind_id();
            match k {
                k if k == self.ids.k(NK::PrimitiveType) => self.lower_primitive_type(node),
                k if k == self.ids.k(NK::UserType) => self.lower_user_type(node),
                k if k == self.ids.k(NK::GenericType) => {
                    // ?a — the identifier is the only named child
                    CstType::Generic(self.s(node, |node| {
                        node.child_by_field_id(self.ids.f(FK::Name))
                            .unwrap()
                            .utf8_text(self.file.source.as_bytes())
                            .unwrap()
                            .to_string()
                    }))
                }
                k if k == self.ids.k(NK::ArrowType) => {
                    let param =
                        self.lower_type(node.child_by_field_id(self.ids.f(FK::Parameter)).unwrap());
                    let result =
                        self.lower_type(node.child_by_field_id(self.ids.f(FK::Return)).unwrap());
                    CstType::Func(param, result)
                }
                k if k == self.k_product_type => self.lower_product_type(node),
                k if k == self.k_sum_type => self.lower_sum_type(node),
                k if k == self.k_self_type => Type::SelfType,
                k if k == self.k_grouped_type => {
                    // transparent paren — unwrap
                    self.lower_type(node.named_child(0).unwrap()).node
                }
                _ => {
                    self.error(node, "unexpected type node");
                    Type::SelfType
                }
            }
        })
    }
}

fn translate_tree_sitter(tree: &Tree, file: &FileSource) -> PackageCollection<UntypedCst> {
    let root = tree.root_node();
    let ids = LangIds::new(&tree_sitter::Language::new(tree_sitter_flare::LANGUAGE));
    let mut lowerer = Translate::new(&ids, file);
    let packages = lowerer.translate_module(root);
    PackageCollection { packages }
}

/// Public parsing function. Produces a parse tree from a source string.
pub fn parse(file: &FileSource) -> Result<PackageCollection<UntypedCst>, anyhow::Error> {
    let mut parser = Parser::new();
    parser.set_language(&tree_sitter_flare::LANGUAGE.into())?;
    let tree = parser.parse(&file.source, None).unwrap();
    Ok(translate_tree_sitter(&tree, file))
}
