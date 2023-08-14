use proc_macro::TokenStream;
use proc_macro2::Span;
use quote::{spanned::Spanned, ToTokens, quote};
use syn::{
    punctuated::Punctuated, Expr, Token, DataStruct, Attribute, Type, ReturnType, Ident,
};

use crate::{types::{FuncDesc, ParamType}, utils::macros::{tkn_err, tkn_err_inner}, constants::{ALL_RETURN_TYPES, BOOL_TYPE, I32_TYPE, F64_TYPE, UNTYPED_TYPE, STRING_TYPE, ALL_ARG_TYPES, NAME_ATTR, NAME_RU_ATTR, ARG_ATTR, RETURNS_ATTR, DEFAULT_ATTR, RESULT_ATTR, BLOB_TYPE, DATE_TYPE}};

pub fn parse_functions(struct_data: &DataStruct) -> Result<Vec<FuncDesc>, TokenStream> {
    
    let mut functions_descriptions = vec![];    
    
    // iterate over methods
    for field in &struct_data.fields {
        let Some(attr) = field.attrs.get(0) else { 
            continue; 
        };
        if !attr.path().is_ident("add_in_func") {
            continue;
        };
        let Some(prop_ident) = field.ident.clone() else {
            return tkn_err!("AddIn props must have a name", field.ident.__span());
        };

        // parsing main `add_in_func` attribute 
        let func_desc = parse_add_in_func_attr(attr)?;
        let mut func_desc = FuncDesc {
            ident: prop_ident,
            name: func_desc.0,
            name_ru: func_desc.1,
            params: vec![],
            return_value: (None, false),
        };

        // parsing `arg` attribute 
        let arg_iter = field.attrs.iter().filter(|attr| attr.path().is_ident(ARG_ATTR));
        for attr in arg_iter {
            func_desc.params.push(parse_arg_attr(attr)?);
        }

        // parsing `returns` attribute     
        let returns_iter = field.attrs.iter().filter(|attr| attr.path().is_ident(RETURNS_ATTR));
        if returns_iter.clone().count() > 1 {
            return tkn_err!("AddIn functions can have only 1 `returns` attribute", field.ident.__span());
        };
        if let Some(attr) = returns_iter.clone().next() {
            let (arg_ty, result) = parse_returns_attr(attr)?;
            func_desc.return_value = (arg_ty, result);
        };

        // check function definition
        let Type::BareFn(field_ty) = &field.ty else {
            return tkn_err!("AddIn functions must have bare `fn` type", field.ident.__span());
        };
        if matches!(field_ty.output, ReturnType::Default) && (func_desc.return_value.0.is_some() || func_desc.return_value.1) {
            return tkn_err!("AddIn functions must have a return type if `returns` attribute is specified", field.ident.__span());
        };       

        functions_descriptions.push(func_desc);
    }

    Ok(functions_descriptions)
}

fn parse_add_in_func_attr(attr: &Attribute) -> Result<(String, String), TokenStream> {
    let name_values: Punctuated<Expr, Token![,]> = attr
        .parse_args_with(Punctuated::parse_terminated)
        .map_err::<TokenStream, _>(|e| tkn_err_inner!(e.to_string(), attr.bracket_token.span.__span()))?;

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
        .find(|(name, _, _)| name == NAME_ATTR) else {
            return tkn_err!("AddIn prop must have a `name` argument", attr.bracket_token.span.__span());
        };
    let Some(prop_name) = prop_name.1 else {
        return tkn_err!("AddIn prop argument `name` must be a string literal assignment: name = \"MyPropName\"", prop_name.2);
    };

    let Some(prop_name_ru) = args
        .clone()
        .find(|(name, _, _)| name == NAME_RU_ATTR) else {
            return tkn_err!("AddIn prop must have a `name_ru` argument", attr.bracket_token.span.__span());
        };
    let Some(prop_name_ru) = prop_name_ru.1 else {
        return tkn_err!("AddIn prop argument `name_ru` must be a string literal assignment: name_ru = \"МоеСвойство\"", prop_name_ru.2);
    };

    Ok((prop_name, prop_name_ru))
}

fn parse_arg_attr(attr: &Attribute) -> Result<(ParamType, Option<Expr>), TokenStream> {
    let exprs: Punctuated<Expr, Token![,]> = attr
        .parse_args_with(Punctuated::parse_terminated)
        .map_err::<TokenStream, _>(|e| tkn_err_inner!(e.to_string(), attr.bracket_token.span.__span()))?; 

    let arg_ty = exprs.iter().find(|expr| {
        if matches!(expr, Expr::Assign(_)) { 
            return false;  
        };
        let expr = expr.to_token_stream().to_string();
        ALL_ARG_TYPES.contains(&expr.as_str())
    }).map(|expr| expr.to_token_stream().to_string());
    let Some(arg_ty_str) = arg_ty else {
        return tkn_err!("AddIn function attribute `arg` must have a type specified: `#[arg(TYPE, ...)]`, where type: Bool, Int, Float or Str", attr.bracket_token.span.__span());
    };

    let default = exprs.iter().find_map(|expr| {
        let Expr::Assign(assign) = expr else { 
            return None; 
        };
        let left = assign.left.to_token_stream().to_string();
        if &left != DEFAULT_ATTR {
            return None;
        };        
        
        Some((&*assign.right).to_owned())
    });

    if arg_ty_str == BLOB_TYPE && default.is_some() {
        return tkn_err!("AddIn function attribute `arg` of type `Blob` cannot have default value", attr.bracket_token.span.__span());
    };

    if arg_ty_str == DATE_TYPE && default.is_some() {
        return tkn_err!("AddIn function attribute `arg` of type `Date` cannot have default value", attr.bracket_token.span.__span());
    };
    
    Ok((match arg_ty_str.as_str() {
        BOOL_TYPE => ParamType::Bool,
        I32_TYPE => ParamType::I32,
        F64_TYPE => ParamType::F64,
        STRING_TYPE => ParamType::String,
        DATE_TYPE => ParamType::Date,
        BLOB_TYPE => ParamType::Blob,
        _ => unreachable!(),
    }, default))
}

fn parse_returns_attr(attr: &Attribute) -> Result<(Option<ParamType>, bool), TokenStream> {
    let exprs: Punctuated<Expr, Token![,]> = attr
        .parse_args_with(Punctuated::parse_terminated)
        .map_err::<TokenStream, _>(|e| tkn_err_inner!(e.to_string(), attr.bracket_token.span.__span()))?; 

    let arg_ty = exprs.iter().find(|expr| {
        if matches!(expr, Expr::Assign(_)) { 
            return false;  
        };
        let expr = expr.to_token_stream().to_string();
        ALL_RETURN_TYPES.contains(&expr.as_str())
    });
    let Some(arg_ty_str) = arg_ty else {
        return tkn_err!("AddIn function attribute `returns` must have a type specified: use `#[returns(None, ...)]` if doesn't return anything", attr.bracket_token.span.__span());
    };
    let arg_ty = match arg_ty_str.to_token_stream().to_string().as_str() {
        BOOL_TYPE => Some(ParamType::Bool),
        I32_TYPE => Some(ParamType::I32),
        F64_TYPE => Some(ParamType::F64),
        STRING_TYPE => Some(ParamType::String),
        DATE_TYPE => Some(ParamType::Date),
        BLOB_TYPE => Some(ParamType::Blob),
        UNTYPED_TYPE => None,
        _ => unreachable!(),
    };

    let result = exprs.iter().any(|expr| {
        if let Expr::Assign(_assign) = expr { 
            return false; 
        };
        let expr = expr.to_token_stream().to_string();
        expr.as_str() == RESULT_ATTR
    });

    Ok((arg_ty, result))        
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
        let param_ident = Ident::new(&format!("param_{}", counter + 1), Span::call_site());
        param_checkers = quote! {
            #param_checkers
            let Some(param_data) = params.get(#counter as usize) else { return false; };
        };
        match param.0 {
            ParamType::Bool => {
                param_checkers = quote! {
                    #param_checkers
                    let native_api_1c::native_api_1c_core::ffi::types::ParamValue::Bool(#param_ident) 
                    = param_data else { 
                        return false; 
                    };
                };
            }
            ParamType::I32 => {
                param_checkers = quote! {
                    #param_checkers
                    let native_api_1c::native_api_1c_core::ffi::types::ParamValue::I32(#param_ident) 
                    = param_data else { 
                        return false; 
                    };
                };
            }
            ParamType::F64 => {
                param_checkers = quote! {
                    #param_checkers
                    let native_api_1c::native_api_1c_core::ffi::types::ParamValue::F64(#param_ident) 
                        = param_data else { 
                            return false; 
                        };
                };
            }
            ParamType::String => {
                param_checkers = quote! {
                    #param_checkers
                    let native_api_1c::native_api_1c_core::ffi::types::ParamValue::Str(#param_ident) 
                    = param_data else { 
                        return false; 
                    };
                    let #param_ident = native_api_1c::native_api_1c_core::ffi::utils::from_os_string(#param_ident);
                };
            }
            ParamType::Date => {
                param_checkers = quote! {
                    #param_checkers
                    let native_api_1c::native_api_1c_core::ffi::types::ParamValue::Date(#param_ident) 
                    = param_data else { 
                        return false; 
                    };
                    let #param_ident: chrono::DateTime<chrono::FixedOffset> = #param_ident.into();
                };
            }
            ParamType::Blob => {
                param_checkers = quote! {
                    #param_checkers
                    let native_api_1c::native_api_1c_core::ffi::types::ParamValue::Blob(#param_ident) 
                    = param_data else { 
                        return false; 
                    };
                    let #param_ident = #param_ident.to_vec();
                };
            }
        }
        func_call = quote! {
            #func_call
            #param_ident.clone().into(),
        };
        counter += 1;
    }

    let mut func_call = quote! {
        #param_checkers
        let call_result = self.#func_ident(#func_call);
    };

    if func.return_value.1 {
        func_call = quote! {
            #func_call
            let Ok(call_result) = call_result else { return false; };
        };
    }

    if return_value {
        let value_setter = match &func.return_value.clone().0.unwrap() {
            ParamType::Bool => quote! { val.set_bool(call_result.into()); },
            ParamType::I32 => quote! { val.set_i32(call_result.into()); },
            ParamType::F64 => quote! { val.set_f64(call_result.into()); },
            ParamType::String => {
                quote! { val.set_str(&native_api_1c::native_api_1c_core::ffi::utils::os_string_nil(String::from(&call_result).as_str())); }
            }
            ParamType::Date => {
                quote! { val.set_date(call_result.into()); }
            }
            ParamType::Blob => {
                quote! { val.set_blob(&call_result); }
            }
        };
        func_call = quote! {
            #func_call
            #value_setter
        };
    }

    Ok(func_call)
}
