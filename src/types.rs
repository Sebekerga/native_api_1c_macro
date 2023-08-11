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
    Bool(Option<Expr>),
    I32(Option<Expr>),
    F64(Option<Expr>),
    String(Option<Expr>),
}

pub struct FuncDesc {
    pub ident: Ident,
    pub name: String,
    pub name_ru: String,
    pub params: Vec<ParamType>,
    pub return_value: (Option<ParamType>, bool),
}
