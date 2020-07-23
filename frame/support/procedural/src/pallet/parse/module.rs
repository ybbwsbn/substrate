use super::helper;
use syn::spanned::Spanned;
use quote::ToTokens;

/// List of additional token to be used for parsing.
mod keyword {
	syn::custom_keyword!(Module);
}

pub struct ModuleDef {
	/// The index of error item in pallet module.
	pub index: usize,
	/// A set of usage of instance, must be check for consistency with trait.
	pub instances: Vec<helper::InstanceUsage>,
	/// The keyword module used (contains span).
	pub module: keyword::Module,
}

impl ModuleDef {
	pub fn try_from(index: usize, item: &mut syn::Item) -> syn::Result<Self> {
		let item = if let syn::Item::Struct(item) = item {
			item
		} else {
			let msg = "Invalid pallet::module, expect struct definition";
			return Err(syn::Error::new(item.span(), msg));
		};

		let module = syn::parse2::<keyword::Module>(item.ident.to_token_stream())?;

		if !matches!(item.vis, syn::Visibility::Public(_)) {
			let msg = "Invalid pallet::module, Module must be public";
			return Err(syn::Error::new(item.span(), msg));
		}
		if item.generics.where_clause.is_some() {
			let msg = "Invalid pallet::module, where clause not supported on Module declaration";
			return Err(syn::Error::new(item.generics.where_clause.span(), msg));
		}

		let mut instances = vec![];
		instances.push(helper::check_type_def_generics(&item.generics, item.span())?);

		// TODO TODO : also check fields.

		Ok(Self { index, instances, module })
	}
}
