use proc_macro::{LexError, TokenStream};
use proc_macro2::Span;
use quote::quote;
use syn::{Ident, Type};

use crate::types::ParamType;

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

pub fn convert_ty_to_param_type(ty: &Type, span: Span) -> Result<ParamType, TokenStream> {
    match ty {
        Type::Path(path_type) => {
            let syn::Path {
                leading_colon,
                segments,
            } = &path_type.path;
            if leading_colon.is_some() {
                return tkn_err!("AddIn props type must not have leading colons", span);
            }
            if segments.len() != 1 {
                return tkn_err!("AddIn props type must have exactly one segment", span);
            }
            let syn::PathSegment { ident, arguments } = segments.iter().next().unwrap();
            let syn::PathArguments::None = arguments else {
                return tkn_err!("AddIn props type must not have arguments", span);
            };

            match ident.to_string().as_str() {
                "bool" => Ok(ParamType::Bool(None)),
                "i32" => Ok(ParamType::I32(None)),
                "f64" => Ok(ParamType::F64(None)),
                "String" => Ok(ParamType::String(None)),
                _ => return tkn_err!("AddIn props type must be bool, i32, f64 or String", span),
            }
        }
        _ => return tkn_err!("AddIn props type must be bool, i32, f64, or String", span),
    }
}

pub fn param_ty_to_ffi_return(
    param_type: &ParamType,
    param_path: proc_macro2::TokenStream,
) -> Result<proc_macro2::TokenStream, TokenStream> {
    match param_type {
        ParamType::Bool(_) => Ok(quote! { val.set_bool(self.#param_path.into()) }),
        ParamType::I32(_) => Ok(quote! { val.set_i32(self.#param_path.into()) }),
        ParamType::F64(_) => Ok(quote! { val.set_f64(self.#param_path.into()) }),
        ParamType::String(_) => Ok(
            quote! { val.set_str(&native_api_1c::native_api_1c_core::ffi::utils::os_string_nil(String::from(&self.#param_path).as_str())) },
        ),
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
        ParamType::String(_) => Ok(quote! {
            native_api_1c::native_api_1c_core::ffi::types::ParamValue::Str(inner_value) => self.#param_path
                = native_api_1c::native_api_1c_core::ffi::utils::from_os_string(inner_value).into(),
        }),
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
