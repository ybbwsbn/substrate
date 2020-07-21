use super::{get_doc_literals, CheckStructDefGenerics};
use quote::ToTokens;
use syn::spanned::Spanned;

/// This checks error declaration as a enum declaration with only variants without fields nor
/// discriminant.
pub struct ErrorDef {
	/// Full enum_ declaration.
	pub item: syn::ItemEnum,
	/// Variants ident and doc literals (ordered as declaration order)
	pub variants: Vec<(syn::Ident, Vec<syn::Lit>)>,
	/// Whether the error has been declared with instance or not, must be consistent with trait
	/// declaration.
	pub has_instance: bool,
}

impl ErrorDef {
	pub fn try_from(item: syn::Item) -> syn::Result<Self> {
		if let syn::Item::Enum(item) = item {
			if !matches!(item.vis, syn::Visibility::Public(_)) {
				let msg = "Invalid pallet::error, `Error` must be public";
				return Err(syn::Error::new(item.span(), msg));
			}

			syn::parse2::<CheckStructDefGenerics>(item.generics.params.to_token_stream())?;
			let has_instance = item.generics.params.len() == 2;

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
				has_instance,
			})
		} else {
			Err(syn::Error::new(item.span(), "Invalid pallet::error, expect item enum"))
		}
	}
}
