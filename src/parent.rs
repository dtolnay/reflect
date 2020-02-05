use crate::{Generics, ParamMap, Type};

#[derive(Debug, Clone)]
pub struct Parent {
    pub(crate) ty: Type,
    pub(crate) generics: Option<Generics>,
}

impl Parent {
    pub fn new(ty: Type) -> Self {
        Self { ty, generics: None }
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
