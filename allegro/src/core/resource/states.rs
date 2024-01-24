use super::ast::{Expr, Statement, SymbolKind, ValDecl};

#[derive(Debug, Clone)]
pub enum ObjectState {
    Unused,
    Read,
    Write,
    Used,
}

#[derive(Debug, Clone)]
pub struct Appearances {
    used: i32,
    read: i32,
    write: i32,
    path: i32,
}

pub const ZERO_APPEARANCES: Appearances = Appearances {
    used: 0,
    read: 0,
    write: 0,
    path: 0,
};

pub const USED_ONCE: Appearances = Appearances {
    used: 1,
    read: 0,
    write: 0,
    path: 0,
};

pub const READ_ONCE: Appearances = Appearances {
    used: 0,
    read: 1,
    write: 0,
    path: 0,
};

pub const WRITE_ONCE: Appearances = Appearances {
    used: 0,
    read: 0,
    write: 1,
    path: 0,
};

pub const PATH_ONCE: Appearances = Appearances {
    used: 0,
    read: 0,
    write: 0,
    path: 1,
};

pub fn merge(a: Appearances, b: Appearances) -> Appearances {
    return Appearances {
        used: a.used + b.used,
        read: a.read + b.read,
        write: a.write + b.write,
        path: a.path + b.path,
    };
}

pub fn merge_list(l: Vec<Appearances>) -> Appearances {
    l.iter().fold(ZERO_APPEARANCES, |acc, x| merge(acc, x.clone()))
}

pub fn count_apps(name: Statement, expr: Expr) -> Appearances {
    let c: Appearances = match expr {
        Expr::Value(ve) => {
            if name.clone().get_token_value() == ve.name {
                USED_ONCE
            } else {
                ZERO_APPEARANCES
            }
        }
        _ => {
            panic!("Unknown expression {:?}", expr)
        }
    };
    c
}

#[derive(Debug, Clone)]
pub struct StateTableEntry {
    pub name: Statement,
    pub state: ObjectState,
}

#[derive(Debug, Clone)]
pub struct StateTable {
    pub entries: Vec<StateTableEntry>,
}

impl StateTable {
    pub fn new() -> Self {
        StateTable { entries: vec![] }
    }

    pub fn init_table(&mut self, params: Vec<Statement>) {
        for param in params {
            self.add_entry(param)
        }
    }

    pub fn get_entry(&mut self, to: Statement) -> Option<(usize, StateTableEntry)> {
        match self.entries.iter().position(|r| r.name == to) {
            Some(x) => {
                let ns = self.entries[x].clone();
                return Some((x, ns));
            }
            None => None,
        }
    }

    pub fn add_entry(&mut self, vd: Statement) {
        match self.get_entry(vd.clone()) {
            None => self.entries.push(StateTableEntry {
                name: vd.clone(),
                state: ObjectState::Unused,
            }),

            Some(_) => {
                panic!("Value binding {vd:?} is already in use!");
            }
        }
    }

    pub fn update_table(&mut self, tbl: StateTable, name: Statement, state: ObjectState) {
        match self.get_entry(name.clone()) {
            None => panic!(
                "Tried to update the state of value binding {:?}, but no such binding was found.",
                name
            ),
            Some((depth, vd)) => self.entries.push(StateTableEntry { name, state }),
        }
    }

    pub fn remove_entry(&mut self, name: Statement) {
        match self.get_entry(name.clone()) {
            None => {
                panic!("Tried to update the state of value binding {name:?}, but no such binding was found.")
            }

            Some((depth, entry)) => match entry.state {
                ObjectState::Used => {
                    self.entries.remove(depth);
                }

                _ => {
                    panic!("Forgot to consume a linear value binding!")
                }
            },
        }
    }
}
