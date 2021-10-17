use super::ty::Ty;

#[derive(Debug, Clone)]
pub struct Value {
    pub ty: Ty,
    pub name: String,
}

#[derive(Debug, Clone, Eq, PartialEq, EnumAsInner)]
pub enum InstKind {
    Binary(BinaryInst),
    Memory(MemoryInstr),
    Branch(TerminateInstr),
}