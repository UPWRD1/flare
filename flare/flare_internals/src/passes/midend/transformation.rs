use std::collections::HashMap;

use petgraph::graph::NodeIndex;

use crate::resource::rep::{ast::Variable, mir::VarId};
#[allow(dead_code, reason = "Indev")]
use crate::{passes::midend::environment::Environment, resource::rep::mir::ANF};

pub struct Transformer<'env> {
    env: &'env Environment,
    ir: HashMap<NodeIndex, ANF>,
    next_var: VarId,
}

impl<'env> Transformer<'env> {
    pub fn new(env: &'env Environment) -> Self {
        Self {
            env,
            ir: HashMap::new(),
            next_var: 0,
        }
    }

    fn fresh_var(&mut self) -> VarId {
        let id = self.next_var;
        self.next_var += 1;
        id
    }

    // pub fn transform(&mut self) -> ANF {
    //     let main = self
    //         .env
    //         .get_from_context(&QualifierFragment::Func("main"), &QualifierFragment::Package("Main"));

    // todo!()
    // }
}
