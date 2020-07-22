use super::get_doc_literals;
use syn::spanned::Spanned;

/// This checks error declaration as a enum declaration with only variants without fields nor
/// discriminant.
pub struct ErrorDef {
	/// Full enum_ declaration.
	pub item: syn::ItemEnum,
	/// Variants ident and doc literals (ordered as declaration order)
	pub variants: Vec<(syn::Ident, Vec<syn::Lit>)>,
	/// A set of usage of instance, must be check for consistency with trait.
	pub instances: Vec<super::InstanceUsage>,
}

impl ErrorDef {
	pub fn try_from(item: syn::Item) -> syn::Result<Self> {
		let item = if let syn::Item::Enum(item) = item {
			item
		} else {
			return Err(syn::Error::new(item.span(), "Invalid pallet::error, expect item enum"));
		};
		if !matches!(item.vis, syn::Visibility::Public(_)) {
			let msg = "Invalid pallet::error, `Error` must be public";
			return Err(syn::Error::new(item.span(), msg));
		}

		let mut instances = vec![];
		instances.push(super::check_type_def_generics(&item.generics, item.span())?);

		if item.generics.where_clause.is_some() {
			let msg = "Invalid pallet::error, unexpected where clause";
			return Err(syn::Error::new(item.generics.where_clause.unwrap().span(), msg));
		}

		let variants = item.variants.iter()
			.map(|variant| {
				if !matches!(variant.fields, syn::Fields::Unit) {
					let msg = "Invalid pallet::error, unexpected fields, must be `Unit`";
					return Err(syn::Error::new(variant.fields.span(), msg));
				}
				if variant.discriminant.is_some() {
					let msg = "Invalid pallet::error, unexpected discriminant, discriminant \
						are not supported";
					let span = variant.discriminant.as_ref().unwrap().0.span();
					return Err(syn::Error::new(span, msg));
				}

				Ok((variant.ident.clone(), get_doc_literals(&variant.attrs)))
			})
			.collect::<Result<_, _>>()?;

		Ok(ErrorDef {
			item,
			variants,
			instances,
		})
	}
}
