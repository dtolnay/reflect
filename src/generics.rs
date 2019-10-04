use crate::GenericRef;
use crate::Ident;
use crate::Type;

#[derive(Debug, Clone)]
pub struct Generics {
    pub(crate) params: Vec<GenericParam>,
}

#[derive(Debug, Clone)]
pub(crate) enum GenericParam {
    Type(TypeParam),
    Lifetime(Lifetime),
    Const(ConstParam),
}

#[derive(Debug, Clone)]
pub(crate) struct TypeParam {
    pub(crate) ident: Ident,
    pub(crate) index: GenericRef,
    pub(crate) bounds: Vec<TypeParamBound>,
}

#[derive(Debug, Clone)]
pub(crate) enum TypeParamBound {
    Trait(TraitBound),
    Lifetime(Lifetime),
}

#[derive(Debug, Clone)]
pub(crate) struct TraitBound {
    ///A set of bound Lifetimes: `for<'a, 'b, 'c>`.
    pub(crate) lifetimes: Vec<Lifetime>,
    pub(crate) ty: Type,
}

#[derive(Debug, Clone)]
pub(crate) struct Lifetime {
    pub(crate) ident: Ident,
    pub(crate) index: GenericRef,
    pub(crate) bounds: Vec<Lifetime>,
}

#[derive(Debug, Clone)]
pub(crate) struct ConstParam {
    pub(crate) private: (),
}

#[derive(Debug, Clone)]
pub struct GenericArguments {
    pub(crate) args: Vec<GenericArgument>,
}

#[derive(Debug, Clone)]
pub(crate) enum GenericArgument {
    Lifetime(Lifetime),
    Type(Type),
    Binding(Binding),
    Constraint(Constraint),
    Const(Expr),
}

#[derive(Debug, Clone)]
pub(crate) struct Binding {
    pub(crate) ident: Ident,
    pub(crate) index: GenericRef,
    pub(crate) ty: Type,
}

#[derive(Debug, Clone)]
pub(crate) struct Constraint {
    pub(crate) ident: Ident,
    pub(crate) index: GenericRef,
    pub(crate) bounds: Vec<TypeParamBound>,
}

#[derive(Debug, Clone)]
pub(crate) struct Expr {
    pub(crate) private: (),
}
