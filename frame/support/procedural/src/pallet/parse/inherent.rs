use syn::spanned::Spanned;

pub struct InherentDef<'a> {
	/// The impl block that implement ProvideInherent.
	pub item: &'a mut syn::ItemImpl,
	/// A set of usage of instance, must be check for consistency with trait.
	pub instances: Vec<super::InstanceUsage>,
}

impl InherentDef {
	pub fn try_from(item: &'a mut syn::Item) -> syn::Result<Self<'a>> {
		if let syn::Item::Impl(item) = item {
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

			let mut instances = vec![];
			instances.push(super::check_module_usage(&item.self_ty)?);

			Ok(InherentDef { item, instances })
		} else {
			Err(syn::Error::new(item.span(), "Invalid pallet::inherent, expect item impl"))
		}
	}
}
