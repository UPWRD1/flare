use serde::{Deserialize, Serialize};
use std::collections::HashMap;

use crate::root::resource::ast::{FnSignature, SymbolType};

#[derive(Debug, Clone, Serialize, Deserialize, PartialEq)]
pub struct SymbolTableGroup {
    pub name: String,
    pub children: HashMap<String, FnSignature>,
}

#[derive(Debug, Clone, Serialize, Deserialize, PartialEq)]
pub struct SymbolTable {
    pub entries: HashMap<String, SymbolType>,
    pub groups: Vec<SymbolTableGroup>,
    pub parent: Box<Option<Self>>,
}

impl Default for SymbolTable {
    fn default() -> Self {
        Self::new()
    }
}

impl SymbolTable {
    pub fn new() -> Self {
        Self {
            entries: HashMap::new(),
            groups: vec![],
            parent: Box::new(None),
        }
    }
    pub fn compare_ty(&mut self, lt: &SymbolType, rt: &SymbolType) -> bool {
        //println!("{:?} vs {:?}", lt, rt);
        if lt.is_generic() {
            if lt == rt {
                return true;
            } else if self.entries.contains_key(&lt.get_generic_name()) {
                let new = self.get(lt.get_generic_name());
                return self.compare_ty(&new, rt);
            } else {
                self.redefine(&lt.get_generic_name(), rt);
            }
            //let new = self.symbol_table.get(lt.get_generic_name());
            //return self.compare_ty(lt, rt);
        }
        if rt.is_generic() {
            if rt == lt {
                return true;
            }
            if self.has(&rt.get_generic_name()) {
                let new = self.get(rt.get_generic_name());
                return self.compare_ty(lt, &new);
            } else {
                self.redefine(&rt.get_generic_name(), lt);
            }
            //let new = self.symbol_table.get(lt.get_generic_name());
            //return self.compare_ty(lt, rt);
        }
        if lt.is_custom() {
            let nlt = self.get(lt.get_custom_name());
            //dbg!(nlt.clone());
            return self.compare_ty(&nlt, rt);
        }
        if rt.is_custom() {
            let nrt = self.get(rt.get_custom_name());
            return self.compare_ty(lt, &nrt);
        }
        if lt.is_variant() && !lt.get_variant_members().iter().all(|x| !x.is_generic()) {
            let mut newt: Vec<SymbolType> = vec![];
            for t in lt.get_variant_members() {
                if t.is_generic() {
                    newt.push(self.get(t.get_generic_name()))
                } else {
                    newt.push(t)
                }
            }
            let fin = SymbolType::Variant(lt.get_variant_name(), newt.into());
            return self.compare_ty(&fin, rt);
        }
        if rt.is_variant() && !rt.get_variant_members().iter().all(|x| !x.is_generic()) {
            let mut newt: Vec<SymbolType> = vec![];
            for t in rt.get_variant_members() {
                if t.is_generic() {
                    newt.push(self.get(t.get_generic_name()))
                } else {
                    newt.push(t)
                }
            }
            let fin = SymbolType::Variant(rt.get_variant_name(), newt.into());
            return self.compare_ty(lt, &fin);
        }
        if lt.is_enum() {
            if rt.is_enum() {
                let variants = lt.get_variants();
                let rvt = rt.get_variants();
                for variant in variants.iter().enumerate() {
                    //println!("vtc: {:?} vs {:?}", variant.1, &rvt[variant.0]);
                    if self.compare_ty(&variant.1, &rvt[variant.0]) {
                        continue;
                    } else {
                        return false;
                    }
                }
                return true;
            } else if rt.is_generic() {
                //dbg!(rt.get_generic_name());
                return true
            }
        }
        lt.compare(rt)
    }

    //#[recursive]
    pub fn get(&mut self, name: String) -> SymbolType {
        //println!("get {:?}", name);
        //dbg!(self.clone());
        
        //dbg!(self.entries.contains_key(&name.clone()));
        if !self.entries.contains_key(&name.clone()) {
            if self.parent.is_some() {
                self.parent.clone().unwrap().get(name)
            } else {
                if name.starts_with("?_") {
                    //todo!();
                    return SymbolType::Generic(name)
                }
                  else {
                    panic!("Undefined binding: {}", name)
                }
            }
        } else {
            let x = self.entries.get(&name);
            let res = x.unwrap().clone();
            res
        }
    }

    pub fn get_module(&mut self, obj: String, name: String) -> FnSignature {
        //println!("Get module {} element {}", obj, name);
        let ot = self.get(obj.clone()).extract();
        let t = self.handle_custom(ot);
        //dbg!(t.clone());
        let gidx = self.groups.iter().position(|f| f.name == t.get_obj_name());
        if gidx.is_none() {
            if self.parent.is_some() {
                self.parent.clone().unwrap().get_module(obj, name)
            } else {
                panic!("Undefined object: {}", obj)
            }
        } else {
            let g = self.groups[gidx.unwrap()].clone();
            g.children.get(&name).unwrap().clone()
        }
    }

    pub fn has(&mut self, name: &String) -> bool {
        //println!("has {:?}", name);
        let x = self.entries.get(name);
        if x.is_none() {
            if self.parent.is_some() {
                self.parent.clone().unwrap().has(name)
            } else {
                false
            }
        } else {
            let res = x.unwrap().clone();
            if res.is_generic() {
                //dbg!(res.get_generic_name().clone());
                if self.entries.contains_key(&res.get_generic_name()) {
                    true
                } else {
                    false
                }
            } else {
                true
            }
        }
    }

    //#[recursive]
    pub fn handle_custom(&mut self, t: SymbolType) -> SymbolType {
        if t.is_custom() {
            let a = self.get(t.get_custom_name());
            //println!("handling {:?}, found {:?}", t, a);

            match a {
                SymbolType::Generic(v) => self.get(v),
                SymbolType::Custom(v, _) => self.get(v),
                _ => a,
            }
        } else if t.is_generic() {
            //println!("handling generic {:?}", t);

            self.get(t.get_generic_name())
        } else if t.is_fn() {
            //println!("handling {:?}", t);
            let mut nargs: Vec<SymbolType> = vec![];
            for a in t.get_args() {
                nargs.push(self.handle_custom(a))
            }
            let nrt = self.handle_custom(t.get_rt());
            SymbolType::Fn(nargs.into(), Box::new(nrt), t.is_variant_constructor())
        } else if t.is_obj() {
            //println!("handling {:?}", t);
            let members = t.get_members();
            let mut nm: Vec<(String, SymbolType)> = vec![];
            for m in members {
                nm.push((m.0, self.handle_custom(m.1)));
            }
            SymbolType::Obj(t.get_obj_name(), nm.into())
        } else {
            t
        }
    }

    //#[inline]
    pub fn set(&mut self, name: &String, t: &SymbolType) {
        let newt = self.handle_custom(t.clone());
        //let newt = t.clone();
        if self.entries.contains_key(name) {
            if self.get(name.clone()).is_mut() {
                //println!("set {} to {:?}", name, newt);
                self.entries
                    .insert(name.to_string(), SymbolType::Mut(Box::new(newt)));
            } else {
                if self.entries.get(name).unwrap() == t {
                    return; // do nothing
                } else {
                    panic!("Cannot redefine immutable value {name} to {:?}; {name} already has value {:?}", t, self.entries.get(name))
                }
            }
        } else {
            if self.entries.get(name).is_some_and(|x| x == t) {
                return; // do nothing
            } else {
                //println!("set {} to {:?}", name, newt);
                self.entries.insert(name.to_string(), newt);
            }
        }
    }

    // #[recursive]
    pub fn redefine(&mut self, name: &String, t: &SymbolType) {
        if name.starts_with("?_") {
            //println!("redefine {} to {:?}", name, t);
            self.entries.insert(name.clone(), t.clone());
        } else {
            let x = self.entries.get(name);
            if x.is_none() {
                if self.parent.is_some() {
                    let mut m = self.parent.clone().unwrap();
                    m.redefine(name, t);
                    self.parent = Box::new(Some(m));
                } else {
                    panic!("Undefined binding: {}", name)
                }
            } else {
                //println!("redefine {} to {:?}", name, t);
                self.entries.insert(name.clone(), t.clone());
            }
        }
    }

    pub fn redefine_group_element(&mut self, group: &String, name: &String, t: &FnSignature) {
        for g in &mut self.groups {
            if g.name == *group {
                if g.children.contains_key(name) {
                    g.children.insert(name.to_string(), t.clone());
                    break;
                } else {
                    panic!(
                        "Cannot redefine undefined {} in group {} with type {:?}",
                        name, group, t
                    )
                }
            }
        }
        if self.parent.is_some() {
            let mut m = self.parent.clone().unwrap();
            m.redefine_group_element(group, name, t);
            self.parent = Box::new(Some(m));
        } else {
            panic!(
                "Cannot redefine undefined {} in group {} with type {:?}",
                name, group, t
            )
        }
    }

    pub fn new_scope(&mut self) {
        //println!("open scope");
        let temp = self.clone();
        self.parent = Box::new(Some(temp));
        self.entries = HashMap::new();
    }

    pub fn pop_scope(&mut self) {
        ////println!("close scope");
        let temp = self.parent.clone().unwrap();
        self.entries = temp.entries;
        self.parent = temp.parent;
    }
}
