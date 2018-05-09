use Data;
use Ident;
use InvokeRef;
use Type;
use ValueRef;

#[derive(Debug, Clone)]
pub(crate) enum ValueNode {
    Unit,
    Str(String),
    Reference(ValueRef),
    ReferenceMut(ValueRef),
    Dereference(ValueRef),
    Binding { name: Ident, ty: Type },
    DataStructure { name: String, data: Data<ValueRef> },
    Invoke(InvokeRef),
    Destructure { parent: ValueRef, field: Ident },
}
