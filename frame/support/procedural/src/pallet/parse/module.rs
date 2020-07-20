use super::{CheckStructDefGenerics};
use syn::spanned::Spanned;
use quote::ToTokens;

/// List of additional token to be used for parsing.
mod keyword {
	syn::custom_keyword!(Module);
}

pub struct ModuleDef {
	pub item: syn::ItemStruct,
	pub has_instance: bool,
}

impl ModuleDef {
	// Check has one or two generics named T or T, I and default instance is set
	pub fn try_from(item: syn::Item) -> syn::Result<Self> {
		if let syn::Item::Struct(item) = item {
			syn::parse2::<keyword::Module>(item.ident.to_token_stream())?;

			if !matches!(item.vis, syn::Visibility::Public(_)) {
				let msg = "Invalid pallet::module, Module must be public";
				return Err(syn::Error::new(item.span(), msg));
			}

			// TODO TODO: assert no where clause ? or use it for constant_metadata.

			syn::parse2::<CheckStructDefGenerics>(item.generics.params.to_token_stream())?;
			let has_instance = item.generics.params.len() == 2;

			// TODO TODO : also check fields.

			Ok(Self { item, has_instance })
		} else {
			let msg = "Invalid pallet::module, expect struct definition";
			Err(syn::Error::new(item.span(), msg))
		}
	}
}
