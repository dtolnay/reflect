use crate::{Generics, ParamMap, Path};

#[derive(Debug, Clone)]
pub struct Parent {
    pub(crate) path: Path,
    pub(crate) generics: Option<Generics>,
    pub(crate) parent_kind: ParentKind,
}

#[derive(Debug, Copy, Clone)]
pub enum ParentKind {
    Trait,
    DataStructure,
}

impl Parent {
    pub fn new(ty: Path, parent_kind: ParentKind) -> Self {
        Self {
            path: ty,
            generics: None,
            parent_kind,
        }
    }

    pub fn set_generics(&mut self, generics: Generics) {
        self.generics = Some(generics);
    }

    pub fn get_param_map(&self) -> Option<ParamMap> {
        self.generics
            .as_ref()
            .map(|generics| generics.get_param_map())
    }
}
