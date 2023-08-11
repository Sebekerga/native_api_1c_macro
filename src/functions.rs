use proc_macro::{LexError, TokenStream};
use proc_macro2::Span;
use quote::{quote, spanned::Spanned, ToTokens};
use syn::{
    parse_macro_input, punctuated::Punctuated, BareFnArg, DeriveInput, Expr, GenericArgument,
    Ident, Token, Type, DataStruct,
};

use crate::{types::FuncDesc, utils::{macros::{tkn_err, tkn_err_inner}, parse_func_param, parse_func_return}};

pub fn parse_funcs(struct_data: &DataStruct) -> Result<Vec<FuncDesc>, TokenStream> {
    
    let mut funcs = vec![];    
    
    // iterate over methods
    for prop in &struct_data.fields {
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
        
        let Type::BareFn(fn_desc) = &prop.ty else {
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
        funcs.push(func_desc);
    }

    Ok(funcs)
}
