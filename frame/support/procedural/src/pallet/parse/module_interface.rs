use syn::spanned::Spanned;
use super::helper;

pub struct ModuleInterfaceDef {
	/// The index of error item in pallet module.
	pub index: usize,
	/// A set of usage of instance, must be check for consistency with trait.
	pub instances: Vec<helper::InstanceUsage>,
}

impl ModuleInterfaceDef {
	pub fn try_from(index: usize, item: &mut syn::Item) -> syn::Result<Self> {
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

			let mut instances = vec![];
			instances.push(helper::check_module_usage(&item.self_ty)?);

			Ok(Self { index, instances })
		} else {
			let msg = "Invalid pallet::module_interface, expect item impl";
			Err(syn::Error::new(item.span(), msg))
		}
	}
}

