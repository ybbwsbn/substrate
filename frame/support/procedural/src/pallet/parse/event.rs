use super::take_item_attrs;
use syn::spanned::Spanned;

/// List of additional token to be used for parsing.
mod keyword {
	syn::custom_keyword!(metadata);
}

pub struct EventDef {
	pub enum_: syn::ItemEnum,
	pub metadata: Vec<(syn::Type, syn::Ident)>,
	// TODO TODO: has_instance, is_generic
}

impl EventDef {
	pub fn try_from(item: syn::Item) -> syn::Result<Self> {
		if let syn::Item::Enum(mut item) = item {
			let mut event_attrs: Vec<PalletEventAttr> = take_item_attrs(&mut item.attrs)?;
			if event_attrs.len() > 1 {
				let msg = "Invalid pallet::metadata, expected only one attribute \
					`pallet::metadata`";
				return Err(syn::Error::new(event_attrs[1].span, msg));
			}
			let metadata = event_attrs.pop().map_or(vec![], |attr| attr.metadata);

			if !matches!(item.vis, syn::Visibility::Public(_)) {
				let msg = "Invalid pallet::event, `Error` must be public";
				return Err(syn::Error::new(item.span(), msg));
			}
			if item.generics.params.len() != 0 {
				let msg = "Invalid pallet::event, unexpected generic";
				return Err(syn::Error::new(item.generics.params.first().unwrap().span(), msg));
			}
			if item.generics.where_clause.is_some() {
				let msg = "Invalid pallet::event, unexpected where clause";
				return Err(syn::Error::new(item.generics.where_clause.unwrap().span(), msg));
			}

			// TODO TODO: check instance is consistent and has default

			Ok(EventDef {
				enum_: item,
				metadata,
			})
		} else {
			Err(syn::Error::new(item.span(), "Invalid pallet::event, expect item enum"))
		}
	}
}

pub struct PalletEventAttr {
	metadata: Vec<(syn::Type, syn::Ident)>,
	span: proc_macro2::Span,
}

fn parse_event_metadata_element(input: syn::parse::ParseStream) -> syn::Result<(syn::Type, syn::Ident)> {
	let typ = input.parse::<syn::Type>()?;
	input.parse::<syn::Token![=]>()?;
	let ident = input.parse::<syn::Ident>()?;
	Ok((typ, ident))
}

impl syn::parse::Parse for PalletEventAttr {
	fn parse(input: syn::parse::ParseStream) -> syn::Result<Self> {
		input.parse::<syn::Token![#]>()?;
		let content;
		syn::bracketed!(content in input);
		content.parse::<syn::Ident>()?;
		content.parse::<syn::Token![::]>()?;

		let lookahead = content.lookahead1();
		if lookahead.peek(keyword::metadata) {
			let span = content.parse::<keyword::metadata>()?.span();
			let metadata_content;
			syn::parenthesized!(metadata_content in content);

			let metadata = metadata_content
				.parse_terminated::<_, syn::Token![,]>(parse_event_metadata_element)?
				.into_pairs()
				.map(syn::punctuated::Pair::into_value)
				.collect();

			Ok(PalletEventAttr { metadata, span })
		} else {
			Err(lookahead.error())
		}
	}
}

