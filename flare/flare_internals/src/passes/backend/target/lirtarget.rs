use std::collections::BTreeMap;

use crate::{
    passes::backend::{lir::ClosureConvertOut, target::Target},
    resource::{
        pretty::Render,
        rep::{backend::lir::Item, midend::ir::ItemId},
    },
};

use itertools::Itertools;
#[derive(Clone, Copy)]
pub struct LIRTarget;

impl LIRTarget {
    fn render_item(&self, item: Item) -> String {
        format!(
            "fn {}({}) -> {} {{\n\t{}\n}}\n",
            item.id.0,
            item.params
                .iter()
                .map(|x| format!(
                    "${}: {}",
                    x.id.0,
                    tiny_pretty::print(
                        &x.ty.render(),
                        &tiny_pretty::PrintOptions {
                            width: 80,
                            ..Default::default()
                        }
                    )
                ))
                .join(","),
            tiny_pretty::print(
                &item.ret_ty.render(),
                &tiny_pretty::PrintOptions {
                    width: 80,
                    ..Default::default()
                }
            ),
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
            .into_values()
            .map(|x| self.render_item(x))
            .join("\n")
    }
}

impl Target for LIRTarget {
    type Output = String;

    fn generate(&mut self, ir: Vec<ClosureConvertOut>) -> Self::Output {
        ir.into_iter()
            .map(|ir| {
                let closures = self.render_closures(ir.closure_items);
                let main_body = self.render_item(ir.item);
                format!("closures = {closures}\n{main_body}")
            })
            .join("---------------------\n\n")
    }
    fn ext(&self) -> &str {
        "lir"
    }
}
