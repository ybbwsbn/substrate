use syn::spanned::Spanned;
use quote::ToTokens;

/// List of additional token to be used for parsing.
mod keyword {
	syn::custom_keyword!(Module);
}

pub struct ModuleDef {
	pub item: syn::ItemStruct,
	/// Use of instance, must be check for consistency with trait declaration.
	pub instances: Vec<Option<super::keyword::I>>,
}

impl ModuleDef {
	// Check has one or two generics named T or T, I and default instance is set
	pub fn try_from(item: syn::Item) -> syn::Result<Self> {
		let item = if let syn::Item::Struct(item) = item {
			item
		} else {
			let msg = "Invalid pallet::module, expect struct definition";
			return Err(syn::Error::new(item.span(), msg));
		};

		syn::parse2::<keyword::Module>(item.ident.to_token_stream())?;

		if !matches!(item.vis, syn::Visibility::Public(_)) {
			let msg = "Invalid pallet::module, Module must be public";
			return Err(syn::Error::new(item.span(), msg));
		}

		// TODO TODO: assert no where clause ?

		let mut instances = vec![];
		instances.push(super::check_type_def_generics(&item.generics, item.span())?);

		// TODO TODO : also check fields.

		Ok(Self { item, instances })
	}
}
