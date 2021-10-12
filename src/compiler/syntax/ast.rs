#[derive(Debug, Clone)]
pub struct Program {
    pub decls: Vec<DeclStmt>,
    pub funcs: Vec<FuncStmt>,
}

#[derive(Debug, Clone)]
pub struct FuncStmt {
    pub span: Span,
    pub name: Ident,
    pub params: Vec<FuncParam>,
    pub ret_ty: TyDef,
    pub body: BlockStmt,
}