use syn::{Expr, Ident};

pub struct PropDesc {
    pub ident: Ident,
    pub name: String,
    pub name_ru: String,
    pub readable: bool,
    pub writable: bool,
    pub ty: ParamType,
}

#[derive(Clone)]
pub enum ParamType {
    Bool,
    I32,
    F64,
    String,
    Date,
    Blob,
}

pub struct FuncDesc {
    pub ident: Ident,
    pub name: String,
    pub name_ru: String,
    pub params: Vec<(ParamType, Option<Expr>)>,
    pub return_value: (Option<ParamType>, bool),
}
