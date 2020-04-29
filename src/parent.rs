use crate::{Generics, Path, SynParamMap};
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

pub trait SetPath<'a, P> {
    fn set_path(&'a mut self, into_path: P);
}

impl<'a> SetPath<'a, Path> for ParentBuilder {
    fn set_path(&'a mut self, into_path: Path) {
        self.path = Some(into_path);
    }
}

impl<'a, F> SetPath<'a, F> for ParentBuilder
where
    F: FnOnce(&'a mut SynParamMap) -> Path,
{
    fn set_path(&'a mut self, into_path: F) {
        self.path = Some((into_path)(&mut self.generics.param_map));
    }
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

    /// Set the path of the Parent.
    /// P can be either a Path or a type implementing FnOnce(&'a mut ParamMap) -> Path
    pub fn set_path<'a, P>(&'a mut self, into_path: P)
    where
        Self: SetPath<'a, P>,
    {
        <Self as SetPath<'a, P>>::set_path(self, into_path);
    }

    pub fn set_generic_params(&mut self, params: &[&str]) {
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
    pub fn get_param_map(&self) -> &SynParamMap {
        &self.generics.param_map
    }
}
