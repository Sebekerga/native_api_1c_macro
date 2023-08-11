use proc_macro::{LexError, TokenStream};
use proc_macro2::Span;
use quote::{quote, spanned::Spanned, ToTokens};
use syn::{
    parse_macro_input, punctuated::Punctuated, BareFnArg, DeriveInput, Expr, GenericArgument,
    Ident, Token, Type,
};

use crate::types::{FuncDesc, ParamType};

use self::macros::tkn_err;

pub mod macros {
    macro_rules! tkn_err_inner {
        ($str:expr, $span:expr) => {
            syn::Error::new($span, $str).to_compile_error().into()
        };
    }

    macro_rules! tkn_err {
        ($str:expr, $span:expr) => {
            Err(crate::utils::macros::tkn_err_inner!($str, $span))
        };
    }

    pub(crate) use tkn_err;
    pub(crate) use tkn_err_inner;
}

pub fn type_arrow_body(field_type: &Type, name: &str) -> Vec<Type> {
    let Type::Path(path) = field_type else { return vec![]; };
    let path = &path.path;

    if !(path.segments.len() == 1 && path.segments.iter().next().unwrap().ident == name) {
        return vec![];
    };

    let option_path = &path.segments.iter().next().unwrap().arguments;
    let syn::PathArguments::AngleBracketed(option_path) = option_path else { return vec![]; };

    option_path
        .args
        .iter()
        .map(|arg| match arg {
            GenericArgument::Type(ty) => ty.clone(),
            _ => panic!("Unexpected generic argument"),
        })
        .collect()
}

pub fn parse_func_param(index: usize, input: &BareFnArg) -> Result<ParamType, TokenStream> {
    let syn::BareFnArg { attrs, name, ty } = input;
    if attrs.len() != 0 {
        return tkn_err!(
            "AddIn functions arguments must not have attributes",
            input.__span()
        );
    }
    let ty = convert_ty_to_param_type(ty, input.__span())?;
    Ok(ty)
}

pub fn parse_func_return(input: &syn::ReturnType) -> Result<Option<ParamType>, TokenStream> {
    Ok(match input {
        syn::ReturnType::Default => None,
        syn::ReturnType::Type(_, ty) => Some(convert_ty_to_param_type(ty, input.__span())?),
    })
}

pub fn convert_ty_to_param_type(ty: &Type, span: Span) -> Result<ParamType, TokenStream> {
    match ty {
        Type::Reference(ref_type) => {
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
        Type::Path(path_type) => {
            let syn::Path {
                leading_colon,
                segments,
            } = &path_type.path;
            if leading_colon.is_some() {
                return tkn_err!(
                    "AddIn functions arguments must not have leading colons",
                    span
                );
            }
            if segments.len() != 1 {
                return tkn_err!(
                    "AddIn functions arguments must have exactly one segment",
                    span
                );
            }
            let syn::PathSegment { ident, arguments } = segments.iter().next().unwrap();
            let syn::PathArguments::None = arguments else {
                return tkn_err!("AddIn functions arguments must not have arguments", span);
            };

            match ident.to_string().as_str() {
                "bool" => Ok(ParamType::Bool(false)),
                "i32" => Ok(ParamType::I32(false)),
                "f64" => Ok(ParamType::F64(false)),
                "String" => Ok(ParamType::String(false)),
                _ => {
                    return tkn_err!(
                        "AddIn functions arguments must be bool, i32, f64 or String",
                        span
                    )
                }
            }
        }
        _ => {
            return tkn_err!(
                "AddIn functions arguments must be bool, i32, f64, or String",
                span
            )
        }
    }
}

pub fn param_ty_to_ffi_return(
    param_type: &ParamType,
    param_path: proc_macro2::TokenStream,
) -> Result<proc_macro2::TokenStream, TokenStream> {
    match param_type {
        ParamType::Bool(_) => Ok(quote! { val.set_bool(self.#param_path) }),
        ParamType::I32(_) => Ok(quote! { val.set_i32(self.#param_path) }),
        ParamType::F64(_) => Ok(quote! { val.set_f64(self.#param_path) }),
        ParamType::String(_) => Ok(
            quote! { val.set_str(&native_api_1c::native_api_1c_core::ffi::utils::os_string_nil(&self.#param_path)) },
        ),
        _ => {
            return tkn_err!(
                "AddIn functions arguments must be bool, i32, f64, or String",
                param_path.__span()
            )
        }
    }
}

pub fn param_ty_to_ffi_set(
    param_type: &ParamType,
    param_path: proc_macro2::TokenStream,
) -> Result<proc_macro2::TokenStream, TokenStream> {
    match param_type {
        ParamType::Bool(_) => Ok(
            quote! { native_api_1c::native_api_1c_core::ffi::types::ParamValue::Bool(inner_value) => self.#param_path = inner_value.clone(), },
        ),
        ParamType::I32(_) => Ok(
            quote! { native_api_1c::native_api_1c_core::ffi::types::ParamValue::I32(inner_value) => self.#param_path = inner_value.clone(), },
        ),
        ParamType::F64(_) => Ok(
            quote! { native_api_1c::native_api_1c_core::ffi::types::ParamValue::F64(inner_value) => self.#param_path = inner_value.clone(), },
        ),
        ParamType::String(_) => Ok(
            quote! { native_api_1c::native_api_1c_core::ffi::types::ParamValue::String(inner_value) => self.#param_path = inner_value.clone(), },
        ),
        _ => {
            return tkn_err!(
                "AddIn functions arguments must be bool, i32, f64, or String",
                param_path.__span()
            )
        }
    }
}

pub fn func_call_tkn(
    func: &FuncDesc,
    return_value: bool,
) -> Result<proc_macro2::TokenStream, TokenStream> {
    let func_ident = &func.ident;
    let mut param_checkers = quote! {};
    let mut func_call = quote! {};
    let mut counter = 0;
    for param in &func.params {
        if matches!(param, ParamType::SelfType | ParamType::SelfTypeMut) {
            continue;
        }

        let param_ident = Ident::new(&format!("param_{}", counter + 1), Span::call_site());
        param_checkers = quote! {
            #param_checkers
            let Some(param_data) = params.get(#counter as usize) else { return false; };
        };
        match param {
            ParamType::Bool(_) => {
                param_checkers = quote! {
                    #param_checkers
                    let native_api_1c::native_api_1c_core::ffi::types::ParamValue::Bool(#param_ident) = param_data
                };
            }
            ParamType::I32(_) => {
                param_checkers = quote! {
                    #param_checkers
                    let native_api_1c::native_api_1c_core::ffi::types::ParamValue::I32(#param_ident) = param_data
                };
            }
            ParamType::F64(_) => {
                param_checkers = quote! {
                    #param_checkers
                    let native_api_1c::native_api_1c_core::ffi::types::ParamValue::F64(#param_ident) = param_data
                };
            }
            ParamType::String(_) => {
                param_checkers = quote! {
                    #param_checkers
                    let native_api_1c::native_api_1c_core::ffi::types::ParamValue::String(#param_ident) = param_data
                };
            }
            _ => {
                return tkn_err!(
                    "AddIn functions arguments cannot be `Self`",
                    param_ident.__span()
                )
            }
        }
        param_checkers = quote! {
            #param_checkers
            else { return false; };
        };
        func_call = quote! {
            #func_call
            #param_ident.clone(),
        };
        counter += 1;
    }

    if return_value {
        let value_setter = match &func.return_value.clone().unwrap() {
            ParamType::Bool(_) => quote! { val.set_bool(result); },
            ParamType::I32(_) => quote! { val.set_i32(result); },
            ParamType::F64(_) => quote! { val.set_f64(result); },
            ParamType::String(_) => {
                quote! { val.set_str(&native_api_1c::native_api_1c_core::ffi::utils::os_string_nil(&result)); }
            }
            _ => {
                return tkn_err!(
                    "AddIn functions must return (), bool, i32, f64, or String",
                    func_ident.__span()
                )
            }
        };
        Ok(quote! {
            #param_checkers
            let result = self.#func_ident(#func_call);
            #value_setter
        })
    } else {
        Ok(quote! {
            #param_checkers
            self.#func_ident(#func_call);
        })
    }
}

pub fn str_literal_token(
    str_literal: &str,
    err_ident: &Ident,
) -> Result<proc_macro2::TokenStream, TokenStream> {
    let token: Result<TokenStream, TokenStream> =
        format!(r#""{}""#, str_literal)
            .parse()
            .map_err(|e: LexError| {
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
