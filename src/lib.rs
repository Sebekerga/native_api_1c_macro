use proc_macro::{LexError, TokenStream};
use quote::{quote, spanned::Spanned, ToTokens};
use syn::{parse_macro_input, punctuated::Punctuated, DeriveInput, Ident, MetaNameValue, Token, Expr};

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
    name: String,
    name_ru: String,
    readable: bool,
    writable: bool,
}

enum ParamType {
    Empty,
    Bool,
    I32,
    F64,
    String,
    Blob,
}

struct FuncDesc {
    name: String,
    name_ru: String,
    params: Vec<ParamType>,
    return_value: ParamType,
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
            name: prop_name,
            name_ru: prop_name_ru,
            readable,
            writable,
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
        
        // match 
    }

    let number_of_props = 0;

    let result = quote! {
        impl native_api_1c::interface::AddInWrapper for #struct_ident {
            fn init(&mut self, interface: &'static native_api_1c::ffi::connection::Connection) -> bool {
                self.connection = Some(*interface);
                true
            }

            fn get_info(&self) -> u16 {
                2000
            }

            fn done(&mut self) {}

            fn register_extension_as(&mut self) -> &[u16] {
                utf16_lit::utf16_null!(#add_in_name_literal).as_ptr()
            }

            fn get_n_props(&self) -> usize {
                #number_of_props
            }
            fn find_prop(&self, name: &[u16]) -> Option<usize> {
                match name {
                    b"MyProp" => Some(0),
                    _ => None,
                }
                todo!("implement")
            };
            // fn get_prop_name(&self, num: usize, alias: usize) -> Option<Vec<u16>> {
            //     None
            //     todo!("implement")
            // };
            // fn get_prop_val(&self, num: usize, val: ReturnValue) -> bool {
            //     false
            //     todo!("implement")
            // };
            // fn set_prop_val(&mut self, num: usize, val: &ParamValue) -> bool {
            //     false
            //     todo!("implement")
            // };
            // fn is_prop_readable(&self, num: usize) -> bool {
            //     false
            //     todo!("implement")
            // };
            // fn is_prop_writable(&self, num: usize) -> bool {
            //     false
            //     todo!("implement")
            // };
            // fn get_n_methods(&self) -> usize {
            //     false
            //     todo!("implement")
            // };
            // fn find_method(&self, name: &[u16]) -> Option<usize> {
            //     false
            //     todo!("implement")
            // };
            // fn get_method_name(&self, num: usize, alias: usize) -> Option<Vec<u16>> {
            //     false
            //     todo!("implement")
            // };
            // fn get_n_params(&self, num: usize) -> usize {
            //     false
            //     todo!("implement")
            // };
            // fn get_param_def_value(
            //     &self,
            //     method_num: usize,
            //     param_num: usize,
            //     value: ReturnValue,
            // ) -> bool {
            //     false
            //     todo!("implement")
            // };
            // fn has_ret_val(&self, method_num: usize) -> bool {
            //     false
            //     todo!("implement")
            // };
            // fn call_as_proc(
            //     &mut self,
            //     method_num: usize,
            //     params: &[ParamValue],
            // ) -> bool {
            //     false
            //     todo!("implement")
            // };
            // fn call_as_func(
            //     &mut self,
            //     method_num: usize,
            //     params: &[ParamValue],
            //     val: ReturnValue,
            // ) -> bool {
            //     false
            //     todo!("implement")
            // };
            // fn set_locale(&mut self, loc: &[u16]) {
            //     false
            //     todo!("implement")
            // };
            // fn set_user_interface_language_code(&mut self, lang: &[u16]) {
            //     todo!("implement")
            // };
        }
    };
    Ok(result)
}

fn build_extern_functions(input: &DeriveInput) -> Result<proc_macro2::TokenStream, TokenStream> {
    let get_class_object_body = quote! { 0 };
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
    let token: Result<TokenStream, TokenStream> = str_literal.parse().map_err(|e: LexError| {
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
