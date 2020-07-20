use syn::spanned::Spanned;

/// Either:
/// * `type Origin`
/// * `struct Origin`
/// * `enum Origin`
pub struct OriginDef {
	pub item: syn::Item,
	pub generics: syn::Generics, // TODO TODO: can be replaced to is_generic
	// TODO TODO: has_instance
}

impl OriginDef {
	pub fn try_from(item: syn::Item) -> syn::Result<Self> {
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

		if !matches!(vis, syn::Visibility::Public(_)) {
			let msg = "Invalid pallet::origin, Origin must be public";
			return Err(syn::Error::new(item_span, msg));
		}

		if ident != "Origin" {
			let msg = "Invalid pallet::origin, ident must `Origin`";
			return Err(syn::Error::new(ident.span(), msg));
		}

		// TODO TODO: check instance has default and consistent

		Ok(OriginDef { generics: generics.clone(), item })
	}
}
