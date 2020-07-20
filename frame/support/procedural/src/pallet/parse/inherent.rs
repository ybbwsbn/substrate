use syn::spanned::Spanned;

pub struct InherentDef {
	pub item: syn::ItemImpl,
	// TODO TODO: has_instance
}

impl InherentDef {
	pub fn try_from(item: syn::Item) -> syn::Result<Self> {
		if let syn::Item::Impl(mut item) = item {
			if item.trait_.is_none() {
				let msg = "Invalid pallet::inherent, expect impl.. ProvideInherent for Module..";
				return Err(syn::Error::new(item.span(), msg));
			}

			if let Some(last) = item.trait_.as_ref().unwrap().1.segments.last() {
				if last.ident != "ProvideInherent" {
					let msg = "Invalid pallet::inherent, expect trait ProvideInherent";
					return Err(syn::Error::new(last.span(), msg));
				}
			} else {
				let msg = "Invalid pallet::inherent, expect impl.. ProvideInherent for Module..";
				return Err(syn::Error::new(item.span(), msg));
			}

			// TODO TODO: maybe check module generics

			Ok(InherentDef { item })
		} else {
			Err(syn::Error::new(item.span(), "Invalid pallet::inherent, expect item impl"))
		}
	}
}
