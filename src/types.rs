use syn::Ident;

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
    SelfType,
    SelfTypeMut,
    Bool(bool),
    I32(bool),
    F64(bool),
    String(bool),
}

pub struct FuncDesc {
    pub ident: Ident,
    pub name: String,
    pub name_ru: String,
    pub params: Vec<ParamType>,
    pub return_value: Option<ParamType>,
}
