use syn::spanned::Spanned;

pub struct ModuleInterfaceDef {
	pub item: syn::ItemImpl,
}

impl ModuleInterfaceDef {
	// Check has one or two generics named T or T, I and default instance is set
	pub fn try_from(item: syn::Item) -> syn::Result<Self> {
		if let syn::Item::Impl(item) = item {
			let item_trait = &item.trait_.as_ref()
				.ok_or_else(|| {
					let msg = "Invalid pallet::module_interface, expect impl... ModuleInterface \
						for ...";
					syn::Error::new(item.span(), msg)
				})?.1;

			if item_trait.segments.len() != 1
				|| item_trait.segments[0].ident != "ModuleInterface"
			{
				let msg = "Invalid pallet::module_interface, expect trait to be `ModuleInterface`";
				return Err(syn::Error::new(item_trait.span(), msg));
			}

			// TODO TODO: we can check: it is for Module with correct generics, and impl_bounds has correct generics
			Ok(Self { item })
		} else {
			let msg = "Invalid pallet::module_interface, expect item impl";
			Err(syn::Error::new(item.span(), msg))
		}
	}
}

