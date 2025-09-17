#[cfg(test)]
use std::sync::{Arc, Mutex};

#[salsa::db]
#[derive(Clone)]
#[cfg_attr(not(test), derive(Default))]
pub struct Db {
    storage: salsa::Storage<Self>,

    // The logs are only used for testing and demonstrating reuse:
    #[cfg(test)]
    logs: Arc<Mutex<Option<Vec<String>>>>,
}

impl salsa::Database for Db {}