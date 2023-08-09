use proc_macro::{LexError, TokenStream};
use proc_macro2::Span;
use quote::{quote, spanned::Spanned, ToTokens};
use syn::{parse_macro_input, punctuated::Punctuated, DeriveInput, Ident, MetaNameValue, Token, Expr, BareFnArg};

macro_rules! tkn_err_inner {
    ($str:expr, $span:expr) => {
        syn::Error::new($span, $str).to_compile_error().into()
    };
}

macro_rules! tkn_err {
    ($str:expr, $span:expr) => {
        Err(tkn_err_inner!($str, $span))
    };
}

#[proc_macro_derive(AddIn, attributes(add_in_prop, add_in_func, add_in_con))]
pub fn derive(input: TokenStream) -> TokenStream {
    let derive_input = parse_macro_input!(input as DeriveInput);
    match derive_result(&derive_input) {
        Ok(tokens) => tokens,
        Err(tokens) => tokens,
    }
}

fn derive_result(input: &DeriveInput) -> Result<TokenStream, TokenStream> {
    let extern_functions = build_extern_functions(input)?;
    let impl_block = build_impl_block(input)?;

    Ok(quote! {
        #impl_block
        #extern_functions
    }
    .into())
}

struct PropDesc {
    ident: Ident,
    name: String,
    name_ru: String,
    readable: bool,
    writable: bool,
    ty: ParamType,
}

enum ParamType {
    SelfType,
    SelfTypeMut,
    Bool,
    I32,
    F64,
    String,
}

struct FuncDesc {
    ident: Ident,
    name: String,
    name_ru: String,
    params: Vec<ParamType>,
    return_value: Option<ParamType>,
}

fn build_impl_block(input: &DeriveInput) -> Result<proc_macro2::TokenStream, TokenStream> {
    let struct_ident = &input.ident;
    let syn::Data::Struct(struct_data) = &input.data else {
        return tkn_err!("AddIn can only be derived for structs",struct_ident.span());
    };
    let add_in_name_literal = str_literal_token(&struct_ident.to_string(), struct_ident)?;

    let mut props: Vec<PropDesc> = Vec::new();
    let mut funcs: Vec<FuncDesc> = Vec::new();

    // iterate over props
    for prop in &struct_data.fields {
        if prop.attrs.len() > 1 {
            return tkn_err!("AddIn fields can have 1 attribute at most", prop.__span());
        }
        let Some(attr) = prop.attrs.get(0) else { 
            continue; 
        };
        if !attr.path().is_ident("add_in_prop") {
            continue;
        };
        let syn::Visibility::Public(_) = prop.vis else {
            return tkn_err!("AddIn props must be public", prop.ident.__span());
        };
        let Some(prop_ident) = prop.ident.clone() else {
            return tkn_err!("AddIn props must have a name", prop.__span());
        };

        let name_values: Punctuated<Expr, Token![,]> = attr
            .parse_args_with(Punctuated::parse_terminated)
            .map_err::<TokenStream, _>(|e| tkn_err_inner!(e.to_string(), attr.__span()))?;

        let args = name_values
            .iter()
            .flat_map(|exp| {
                let Expr::Assign(assign) = exp else { 
                    return Some((exp.to_token_stream().to_string(), None, exp.__span())); 
                };
                let Expr::Lit(lit) = &*assign.right else { return None };
                let syn::Lit::Str(str_lit) = &lit.lit else { return None };
                Some((assign.left.to_token_stream().to_string(), Some(str_lit.value()), exp.__span()))
            });
        let Some(prop_name) = args
            .clone()
            .find(|(name, _, _)| name == "name") else {
                return tkn_err!("AddIn prop must have a `name` argument", attr.__span());
            };
        let Some(prop_name) = prop_name.1 else {
            return tkn_err!("AddIn prop argument `name` must be a string literal assignment: name = \"MyPropName\"", prop_name.2);
        };

        let Some(prop_name_ru) = args
            .clone()
            .find(|(name, _, _)| name == "name_ru") else {
                return tkn_err!("AddIn prop must have a `name_ru` argument", attr.__span());
            };
        let Some(prop_name_ru) = prop_name_ru.1 else {
            return tkn_err!("AddIn prop argument `name_ru` must be a string literal assignment: name_ru = \"МоеСвойство\"", prop_name_ru.2);
        };

        let readable = args
            .clone()
            .find(|(name, _, _)| name == "readable")
            .is_some();

        let writable = args
            .clone()
            .find(|(name, _, _)| name == "writable")
            .is_some();

        if !readable && !writable {
            return tkn_err!("AddIn prop must be either readable, writable or both", attr.__span());
        }

        props.push(PropDesc {
            ident: prop_ident,
            name: prop_name,
            name_ru: prop_name_ru,
            readable,
            writable,
            ty: convert_ty_to_param_type(&prop.ty, prop.__span())?,
        });
    }
    // iterate over methods
    for prop in &struct_data.fields {
        if prop.attrs.len() > 1 {
            return tkn_err!("AddIn fields can have 1 attribute at most", prop.__span());
        }
        let Some(attr) = prop.attrs.get(0) else { 
            continue; 
        };
        if !attr.path().is_ident("add_in_func") {
            continue;
        };
        let syn::Visibility::Public(_) = prop.vis else {
            return tkn_err!("AddIn functions must be public", prop.ident.__span());
        };
        let Some(prop_ident) = prop.ident.clone() else {
            return tkn_err!("AddIn props must have a name", prop.__span());
        };

        let name_values: Punctuated<Expr, Token![,]> = attr
            .parse_args_with(Punctuated::parse_terminated)
            .map_err::<TokenStream, _>(|e| tkn_err_inner!(e.to_string(), attr.__span()))?;

        let args = name_values
            .iter()
            .flat_map(|exp| {
                let Expr::Assign(assign) = exp else { 
                    return Some((exp.to_token_stream().to_string(), None, exp.__span())); 
                };
                let Expr::Lit(lit) = &*assign.right else { return None };
                let syn::Lit::Str(str_lit) = &lit.lit else { return None };
                Some((assign.left.to_token_stream().to_string(), Some(str_lit.value()), exp.__span()))
            });
        let Some(prop_name) = args
            .clone()
            .find(|(name, _, _)| name == "name") else {
                return tkn_err!("AddIn prop must have a `name` argument", attr.__span());
            };
        let Some(prop_name) = prop_name.1 else {
            return tkn_err!("AddIn prop argument `name` must be a string literal assignment: name = \"MyPropName\"", prop_name.2);
        };

        let Some(prop_name_ru) = args
            .clone()
            .find(|(name, _, _)| name == "name_ru") else {
                return tkn_err!("AddIn prop must have a `name_ru` argument", attr.__span());
            };
        let Some(prop_name_ru) = prop_name_ru.1 else {
            return tkn_err!("AddIn prop argument `name_ru` must be a string literal assignment: name_ru = \"МоеСвойство\"", prop_name_ru.2);
        };

        let mut func_desc = FuncDesc {
            ident: prop_ident,
            name: prop_name,
            name_ru: prop_name_ru,
            params: Vec::new(),
            return_value: None,
        };
        
        let syn::Type::BareFn(fn_desc) = &prop.ty else {
            return tkn_err!("AddIn functions type must be a function type", prop.ident.__span());
        }; 
        if fn_desc.lifetimes.is_some() {
            return tkn_err!("AddIn functions must not have lifetimes", prop.ident.__span());
        }
        if fn_desc.unsafety.is_some() {
            return tkn_err!("AddIn functions must be safe", prop.ident.__span());
        }
        if fn_desc.abi.is_some() {
            return tkn_err!("AddIn functions must not have abi", prop.ident.__span());
        }
        for (i, input) in fn_desc.inputs.iter().enumerate() {
            let param_type = parse_func_param(i, input)?;
            func_desc.params.push(param_type);
        }
        func_desc.return_value = parse_func_return(&fn_desc.output)?;
    }



    let number_of_props = props.len();
    let number_of_func = funcs.len();

    let mut find_prop_body = quote! {};
    let mut get_prop_name_body = quote! {};
    let mut is_prop_readable_body = quote! {};
    let mut is_prop_writable_body = quote! {};
    let mut get_prop_val_body = quote! {};
    let mut set_prop_val_body = quote! {};

    for prop in &props {
        let prop_ident = &prop.ident;
        let name_literal = str_literal_token(&prop.name, struct_ident)?;
        let name_ru_literal = str_literal_token(&prop.name_ru, struct_ident)?;
        let readable = prop.readable;
        let writable = prop.writable;
        let prop_index = props.iter().position(|p| p.name == prop.name).unwrap();

        find_prop_body = quote! {
            #find_prop_body
            if utf16_lit::utf16_null!(#name_literal) == name { return Some(#prop_index) };
            if utf16_lit::utf16_null!(#name_ru_literal) == name { return Some(#prop_index) };
        };
        get_prop_name_body = quote! {
            #get_prop_name_body
            if num == #prop_index && alias == 0 { return Some(utf16_lit::utf16_null!(#name_ru_literal).into()) };
            if num == #prop_index { return Some(utf16_lit::utf16_null!(#name_literal).into()) };
        };
        is_prop_readable_body = quote! {
            #is_prop_readable_body
            if num == #prop_index { return #readable };
        };
        is_prop_writable_body = quote! {
            #is_prop_writable_body
            if num == #prop_index { return #writable };
        };     

        if readable { 
            let ffi_set_tkn = param_ty_to_ffi_return(&prop.ty, quote! { #prop_ident})?;
            get_prop_val_body = quote! {
                #get_prop_val_body
                if num == #prop_index { 
                    #ffi_set_tkn;
                    return true; 
                };
            };  
        };

        if writable {
            let prop_set_tkn = param_ty_to_ffi_set(&prop.ty, quote! { #prop_ident })?;
            set_prop_val_body = quote! {
                #set_prop_val_body
                if num == #prop_index {
                    match val {
                        #prop_set_tkn
                        _ => return false,
                    } 
                    return true; 
                };
            };
        }
    };

    let mut find_func_body = quote! {};
    let mut get_func_name_body = quote! {};
    let mut has_ret_val_body = quote! {};
    let mut get_n_params_body = quote! {};
    let mut call_as_proc_body = quote! {};
    let mut call_as_func_body = quote! {};

    for func in &funcs {
        let name_literal = str_literal_token(&func.name, struct_ident)?;
        let name_ru_literal = str_literal_token(&func.name_ru, struct_ident)?;
        let has_ret_val = func.return_value.is_some();
        let func_index = funcs.iter().position(|p| p.name == func.name).unwrap();
        let number_of_params = func.params.iter().filter(|p| match p { ParamType::SelfType | ParamType::SelfTypeMut => false, _ => true }).count();

        find_func_body = quote! {
            #find_func_body
            if utf16_lit::utf16_null!(#name_literal) == name { return Some(#func_index) };
            if utf16_lit::utf16_null!(#name_ru_literal) == name { return Some(#func_index) };
        };
        get_func_name_body = quote! {
            #get_func_name_body
            if num == #func_index && alias == 0 { return Some(utf16_lit::utf16_null!(#name_ru_literal).into()) };
            if num == #func_index { return Some(utf16_lit::utf16_null!(#name_literal).into()) };
        };
        has_ret_val_body = quote! {
            #has_ret_val_body
            if num == #func_index { return #has_ret_val };
        };
        get_n_params_body = quote! {
            #get_n_params_body
            if num == #func_index { return #number_of_params };
        };
    }

    // panic!("set_prop_val_body: {}", set_prop_val_body.to_string());
    // panic!("find_prop_body: {}", find_prop_body.to_string());

    let result = quote! {
        impl native_api_1c::interface::AddInWrapper for #struct_ident {
            fn init(&mut self, interface: &'static native_api_1c::ffi::connection::Connection) -> bool {
                self.connection = std::sync::Arc::new(Some(interface));
                true
            }

            fn get_info(&self) -> u16 {
                2000
            }

            fn done(&mut self) {}

            fn register_extension_as(&mut self) -> &[u16] {
                &utf16_lit::utf16_null!(#add_in_name_literal)
            }

            fn get_n_props(&self) -> usize {
                #number_of_props
            }
            fn find_prop(&self, name: &[u16]) -> Option<usize> {
                #find_prop_body
                None
            }
            fn get_prop_name(&self, num: usize, alias: usize) -> Option<Vec<u16>> {
                #get_prop_name_body
                None
            }
            fn get_prop_val(&self, num: usize, val: native_api_1c::ffi::types::ReturnValue) -> bool {
                #get_prop_val_body
                false
            }
            fn set_prop_val(&mut self, num: usize, val: &native_api_1c::ffi::types::ParamValue) -> bool {
                // #set_prop_val_body
                false     
            }
            fn is_prop_readable(&self, num: usize) -> bool {
                #is_prop_readable_body
                false
            }
            fn is_prop_writable(&self, num: usize) -> bool {
                #is_prop_writable_body
                false
            }
            fn get_n_methods(&self) -> usize {
                #number_of_func
            }
            fn find_method(&self, name: &[u16]) -> Option<usize> {
                #find_func_body
                None
            }
            fn get_method_name(&self, num: usize, alias: usize) -> Option<Vec<u16>> {
                #get_func_name_body
                None
            }
            fn get_n_params(&self, num: usize) -> usize {
                #get_n_params_body
                0
            }
            fn get_param_def_value(
                &self,
                method_num: usize,
                param_num: usize,
                value: native_api_1c::ffi::types::ReturnValue,
            ) -> bool {
                false
            }
            fn has_ret_val(&self, method_num: usize) -> bool {
                #has_ret_val_body
                false
            }
            fn call_as_proc(
                &mut self,
                method_num: usize,
                params: &[native_api_1c::ffi::types::ParamValue],
            ) -> bool {
                #call_as_proc_body
                false
            }
            fn call_as_func(
                &mut self,
                method_num: usize,
                params: &[native_api_1c::ffi::types::ParamValue],
                val: native_api_1c::ffi::types::ReturnValue,
            ) -> bool {
                #call_as_func_body
                false
            }
            fn set_locale(&mut self, loc: &[u16]) {
            }
            fn set_user_interface_language_code(&mut self, lang: &[u16]) {
            }
        }
    };
    Ok(result)
}

fn build_extern_functions(input: &DeriveInput) -> Result<proc_macro2::TokenStream, TokenStream> {
    let struct_ident = &input.ident;
    let get_class_object_body = quote! { 
        match *name as u8 {
            b'1' => {
                let add_in_1 = #struct_ident::new();
                native_api_1c::ffi::create_component(component, add_in_1)
            },
            _ => 0, 
        }   
    };
    let get_class_names_body = quote! { utf16_lit::utf16_null!("1").as_ptr() };

    let result = quote! {
        pub static mut PLATFORM_CAPABILITIES: std::sync::atomic::AtomicI32 =
            std::sync::atomic::AtomicI32::new(-1);

        #[allow(non_snake_case)]
        #[no_mangle]
        pub extern "C" fn GetAttachType() -> native_api_1c::ffi::AttachType {
            native_api_1c::ffi::AttachType::Any
        }

        #[allow(non_snake_case)]
        #[no_mangle]
        pub unsafe extern "C" fn DestroyObject(component: *mut *mut std::ffi::c_void) -> std::ffi::c_long {
            native_api_1c::ffi::destroy_component(component)
        }

        #[allow(non_snake_case)]
        #[no_mangle]
        pub unsafe extern "C" fn GetClassObject(
            name: *const u16,
            component: *mut *mut std::ffi::c_void,
        ) -> std::ffi::c_long {
            #get_class_object_body
        }

        #[allow(non_snake_case)]
        #[no_mangle]
        pub extern "C" fn GetClassNames() -> *const u16 {
            #get_class_names_body
        }
    };

    Ok(result)
}

fn str_literal_token(
    str_literal: &str,
    err_ident: &Ident,
) -> Result<proc_macro2::TokenStream, TokenStream> {
    let token: Result<TokenStream, TokenStream> = format!(r#""{}""#,str_literal).parse().map_err(|e: LexError| {
        let token2: TokenStream = Err(syn::Error::new(
            err_ident.span(),
            format!("LexErr: {}", e.to_string()),
        )
        .to_compile_error())
        .unwrap();
        token2
    });
    token.map(|t| t.into())
}

fn parse_func_param(index: usize, input: &BareFnArg) -> Result<ParamType, TokenStream> {
    let syn::BareFnArg {
        attrs,
        name,
        ty,
    } = input;
    if attrs.len() != 0 {
        return tkn_err!("AddIn functions arguments must not have attributes", input.__span());
    }
    let ty = convert_ty_to_param_type(ty, input.__span())?;
    Ok(ty)
}

fn parse_func_return(input: &syn::ReturnType) -> Result<Option<ParamType>, TokenStream> {
    Ok(match input {
        syn::ReturnType::Default => None,
        syn::ReturnType::Type(_, ty) => Some(convert_ty_to_param_type(ty, input.__span())?),
    })
}

fn convert_ty_to_param_type(ty: &syn::Type, span: Span) -> Result<ParamType, TokenStream> {
    match ty {
        syn::Type::Reference(ref_type) => {
            let ty = &*ref_type.elem;
            if ty.to_token_stream().to_string() != "Self" {
                return tkn_err!("AddIn functions arguments must be Self", span);
            }
            if ref_type.mutability.is_some() {
                Ok(ParamType::SelfTypeMut)
            } else {
                Ok(ParamType::SelfType)
            }
        }
        syn::Type::Path(path_type) => {
            let syn::Path { leading_colon, segments } = &path_type.path;
            if leading_colon.is_some() {
                return tkn_err!("AddIn functions arguments must not have leading colons", span);
            }
            if segments.len() != 1 {
                return tkn_err!("AddIn functions arguments must have exactly one segment", span);
            }
            let syn::PathSegment { ident, arguments } = segments.iter().next().unwrap();
            let syn::PathArguments::None = arguments else {
                return tkn_err!("AddIn functions arguments must not have arguments", span);
            };
            match ident.to_string().as_str() {
                "bool" => Ok(ParamType::Bool),
                "i32" => Ok(ParamType::I32),
                "f64" => Ok(ParamType::F64),
                "String" => Ok(ParamType::String),
                _ => return tkn_err!("AddIn functions arguments must be bool, i32, f64 or String", span),
            }
        }
        _ => return tkn_err!("AddIn functions arguments must be bool, i32, f64, or String", span),
    }
}

fn param_ty_to_ffi_return(param_type: &ParamType, param_path: proc_macro2::TokenStream) -> Result<proc_macro2::TokenStream, TokenStream> {
    match param_type {
        ParamType::Bool => Ok(quote! { val.set_bool(self.#param_path) }),
        ParamType::I32 => Ok(quote! { val.set_i32(self.#param_path) }),
        ParamType::F64 => Ok(quote! { val.set_f64(self.#param_path) }),
        ParamType::String => Ok(quote! { val.set_str(&self.#param_path) }),
        _ => return tkn_err!("AddIn functions arguments must be bool, i32, f64, or String", param_path.__span()),
    }
}

fn param_ty_to_ffi_set(param_type: &ParamType, param_path: proc_macro2::TokenStream) -> Result<proc_macro2::TokenStream, TokenStream> {
    match param_type {
        ParamType::Bool => Ok(quote! { native_api_1c::ffi::types::ParamValue::Bool(inner_value) => self.#param_path = inner_value, }),
        ParamType::I32 => Ok(quote! { native_api_1c::ffi::types::ParamValue::I32(inner_value) => self.#param_path = inner_value, }),
        ParamType::F64 => Ok(quote! { native_api_1c::ffi::types::ParamValue::F64(inner_value) => self.#param_path = inner_value, }),
        ParamType::String => Ok(quote! { native_api_1c::ffi::types::ParamValue::String(inner_value) => self.#param_path = inner_value, }),
        _ => return tkn_err!("AddIn functions arguments must be bool, i32, f64, or String", param_path.__span()),
    }
}