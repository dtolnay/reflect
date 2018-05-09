use Type;

#[derive(Debug, Clone)]
pub struct Signature {
    pub(crate) receiver: Receiver,
    pub(crate) inputs: Vec<Type>,
    pub(crate) output: Type,
}

#[derive(Debug, Clone, Copy)]
pub enum Receiver {
    NoSelf,
    SelfByValue,
    SelfByReference,
    SelfByReferenceMut,
}

impl Signature {
    pub fn new() -> Self {
        Signature {
            receiver: Receiver::NoSelf,
            inputs: Vec::new(),
            output: Type::unit(),
        }
    }

    pub fn set_self_by_value(&mut self) {
        self.receiver = Receiver::SelfByValue;
    }

    pub fn set_self_by_reference(&mut self) {
        self.receiver = Receiver::SelfByReference;
    }

    pub fn set_self_by_reference_mut(&mut self) {
        self.receiver = Receiver::SelfByReferenceMut;
    }

    pub fn add_input(&mut self, input: Type) {
        self.inputs.push(input);
    }

    pub fn set_output(&mut self, output: Type) {
        self.output = output;
    }
}
