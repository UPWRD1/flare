use std::collections::BTreeMap;

use crate::{passes::backend::{lir::{ClosureConvertOut, closure_convert}, target::Target}, resource::{rep::{backend::lir::Item, midend::ir::{self, ItemId}}, pretty::{Render, DocExt}}};

use itertools::Itertools;
#[derive(Clone, Copy)]
pub struct LIRTarget;

impl LIRTarget {
    fn render_item(&self, item: Item) -> String {
        format!(
            "fn {}({}) {{\n\t{}\n}}\n",
            item.id.0,
            item.params
                .iter()
                .map(|x| format!(
                    "${}: {}",
                    x.id.0,
                    tiny_pretty::print(
                        &x.ty.clone().render(),
                        &tiny_pretty::PrintOptions {
                            width: 80,
                            ..Default::default()
                        }
                    )
                ))
                .join(","),
            tiny_pretty::print(
                &item.body.render(),
                &tiny_pretty::PrintOptions {
                    width: 80,
                    ..Default::default()
                }
            ),
        )
    }

    fn render_closures(&self, closures: BTreeMap<ItemId, Item>) -> String {
        closures
            .into_iter()
            .map(|(i, x)| self.render_item(x))
            .join("\n")
    }
}

impl Target for LIRTarget {
    type Partial = String;

    type Output = String;
    type Input = ClosureConvertOut;

    fn generate(&mut self, ir: Self::Input) -> Self::Partial {
        let closures = self.render_closures(ir.closure_items);
        let main_body = self.render_item(ir.item);

        format!("closures = {closures}\n{main_body}")
    }

    fn finish(&self, p: Vec<Self::Partial>) -> Self::Output {
        p.into_iter()
            // .enumerate()
            // .map(|(i, x)| format!("{x}"))
            // .collect::<Vec<String>>()
            .join("---------------------\n\n")
    }
    fn ext(&self) -> &str {
        "lir"
    }

    fn convert(&self, ir: Vec<ir::IR>) -> Vec<Self::Input> {
        closure_convert(ir)
    }
}