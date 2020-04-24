use crate::{Generics, ParamMap, Path};
use std::default::Default;

#[derive(Debug, Clone)]
pub struct Parent {
    pub(crate) path: Path,
    pub(crate) generics: Generics,
    pub(crate) parent_kind: ParentKind,
}

pub struct ParentBuilder {
    pub(crate) path: Option<Path>,
    pub(crate) generics: Generics,
    pub(crate) parent_kind: ParentKind,
}

impl ParentBuilder {
    pub fn new(parent_kind: ParentKind) -> Self {
        ParentBuilder {
            path: None,
            generics: Default::default(),
            parent_kind,
        }
    }

    pub fn into_parent(self) -> Parent {
        Parent {
            path: self.path.unwrap(),
            generics: self.generics,
            parent_kind: self.parent_kind,
        }
    }

    pub fn set_path<'a, F>(&'a mut self, into_path: F)
    where
        F: FnOnce(&'a mut ParamMap) -> Path,
    {
        self.path = Some((into_path)(&mut self.generics.param_map))
    }

    pub fn set_generic_params(&mut self, params: &[&str]) -> &mut ParamMap {
        self.generics.set_generic_params(params)
    }

    pub fn set_generic_constraints(&mut self, constraints: &[&str]) {
        self.generics.set_generic_constraints(constraints)
    }
}

#[derive(Debug, Copy, Clone)]
pub enum ParentKind {
    Trait,
    DataStructure,
}

impl Parent {
    pub fn get_param_map(&self) -> &ParamMap {
        &self.generics.param_map
    }
}
