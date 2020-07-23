use syn::spanned::Spanned;
use super::helper;

/// Either:
/// * `type Origin`
/// * `struct Origin`
/// * `enum Origin`
///
/// type generics can only be nothing or `T` or `T, I`
pub struct OriginDef {
	/// The index of error item in pallet module.
	pub index: usize,
	pub has_instance: bool,
	pub is_generic: bool,
	/// A set of usage of instance, must be check for consistency with trait.
	pub instances: Vec<helper::InstanceUsage>,
}

impl OriginDef {
	pub fn try_from(index: usize, item: &mut syn::Item) -> syn::Result<Self> {
		let item_span = item.span();
		let (vis, ident, generics) = match &item {
			syn::Item::Enum(item) => (&item.vis, &item.ident, &item.generics),
			syn::Item::Struct(item) => (&item.vis, &item.ident, &item.generics),
			syn::Item::Type(item) => (&item.vis, &item.ident, &item.generics),
			_ => {
				let msg = "Invalid pallet::origin, expect enum or struct or type";
				return Err(syn::Error::new(item.span(), msg));
			},
		};

		let has_instance = generics.params.len() == 2;
		let is_generic = generics.params.len() > 0;

		let mut instances = vec![];
		if let Some(u) = helper::check_type_def_optional_generics(&generics, item.span())? {
			instances.push(u);
		}

		if !matches!(vis, syn::Visibility::Public(_)) {
			let msg = "Invalid pallet::origin, Origin must be public";
			return Err(syn::Error::new(item_span, msg));
		}

		if ident != "Origin" {
			let msg = "Invalid pallet::origin, ident must `Origin`";
			return Err(syn::Error::new(ident.span(), msg));
		}

		Ok(OriginDef {
			index,
			has_instance,
			is_generic,
			instances,
		})
	}
}
