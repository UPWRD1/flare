use crate::resource::{
    errors::{CompResult, CompilerErr, DynamicErr, ErrorCollection},
    rep::frontend::{
        ast::Untyped,
        cst::{CstExpr, ProductRow, UntypedCst},
        csttypes::{CstClosedRow, CstType},
        files::{FileID, FileSource},
    },
};

use tree_sitter_flare::NODE_TYPES;

use internment::Intern;
// use lasso::{Interner, Rodeo};
use ordered_float::OrderedFloat;

use tree_sitter::{InputEdit, Language, Parser, Point, Tree};

fn translate_tree_sitter(tree: &Tree) -> ProductRow<UntypedCst> {
    let mut cursor = tree.walk();
    for expr in tree.root_node().children(&mut cursor) {
        match expr.kind() {
            "source_file" => todo!(),
            anythingelse => panic!("{anythingelse}"),
        }
    }
    todo!()
}

/// Public parsing function. Produces a parse tree from a source string.
pub fn parse(file: &FileSource) -> Result<ProductRow<UntypedCst>, anyhow::Error> {
    let mut parser = Parser::new();
    parser.set_language(&tree_sitter_flare::LANGUAGE.into())?;
    let mut tree = parser.parse(&file.source, None).unwrap();
    let root_node = tree.root_node();
    Ok(translate_tree_sitter(&tree))
}
