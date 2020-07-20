use super::{CheckTraitDefGenerics, take_item_attrs, get_doc_literals};
use syn::spanned::Spanned;
use quote::ToTokens;

/// List of additional token to be used for parsing.
mod keyword {
	syn::custom_keyword!(Trait);
	syn::custom_keyword!(Get);
	syn::custom_keyword!(trait_);
	syn::custom_keyword!(const_);
}

// TODO TODO: maybe allow additional spefici constant metadata

pub struct TraitDef {
	/// The trait definition, with all pallet attribute processed. (i.e. pallet::const_ attributes
	/// has been removed.
	pub item: syn::ItemTrait,

	// struct_def_generics: `<T: Trait<I>, T: Instance = DefaultInstance> where ..`
	// impl_generics: `<T: Trait<I>, T: Instance> where ..`
	// struct_use_generics: `<T, I>`

	pub has_instance: bool,

	// `type $ident: Get<$type>`
	pub consts_metadata: Vec<ConstMetadataDef>, // Maybe add `Get`
	// instance: Span,

	// REQUIRES:
	// - `T`, Trait, I, Instance, ¿where_clause?, then define a function constant metadata used by module,
	// - consts: name type, value=getter, docs
}

impl TraitDef {
	pub fn try_from(item: syn::Item) -> syn::Result<Self> {
		if let syn::Item::Trait(mut item) = item {
			syn::parse2::<keyword::Trait>(item.ident.to_token_stream())?;
			if item.generics.where_clause.is_some() {
				let msg = "Invalid pallet::trait, expect no where clause";
				return Err(syn::Error::new(item.generics.where_clause.span(), msg));
			}
			if item.generics.params.len() > 1 {
				let msg = "Invalid pallet::trait, expect no more than one generics";
				return Err(syn::Error::new(item.generics.params[2].span(), msg));
			}

			let has_instance;
			if let Some(instance) = item.generics.params.first() {
				syn::parse2::<CheckTraitDefGenerics>(instance.to_token_stream())?;
				has_instance = true;
			} else {
				has_instance = false;
			}

			let mut consts_metadata = vec![];
			for trait_item in &mut item.items {
				let trait_item_attrs: Vec<PalletTraitAttr> = take_item_attrs(trait_item)?;

				if trait_item_attrs.len() > 1 {
					let msg = "Invalid attribute in pallet::trait, only one attribute is expected";
					return Err(syn::Error::new(trait_item_attrs[2].span(), msg));
				}

				match trait_item_attrs.first() {
					Some(PalletTraitAttr::Const(_span)) => match trait_item {
						syn::TraitItem::Type(type_) => {
							let const_ = syn::parse2::<ConstMetadataDef>(type_.to_token_stream())?;
							consts_metadata.push(const_);
						},
						_ => {
							let msg = "Invalid pallet::const in pallet::trait, expect type trait \
								item";
							return Err(syn::Error::new(trait_item.span(), msg));
						},
					},
					None => (),
				}
			}

			Ok(Self { item, has_instance, consts_metadata })
		} else {
			let msg = "Invalid pallet::trait, expect Trait definition";
			Err(syn::Error::new(item.span(), msg))
		}
	}
}

pub struct ConstMetadataDef {
	pub ident: syn::Ident,
	pub type_: syn::Type,
	pub doc: Vec<syn::Lit>,
}

impl syn::parse::Parse for ConstMetadataDef  {
	fn parse(input: syn::parse::ParseStream) -> syn::Result<Self> {
		let doc = get_doc_literals(&syn::Attribute::parse_outer(input)?);
		input.parse::<syn::Token![type]>()?;
		let ident = input.parse::<syn::Ident>()?;
		input.parse::<syn::Token![:]>()?;
		input.parse::<keyword::Get>()?;
		input.parse::<syn::Token![<]>()?;
		let type_ = input.parse::<syn::Type>()?;
		input.parse::<syn::Token![>]>()?;
		input.parse::<syn::Token![;]>()?;

		Ok(Self { ident, type_, doc })
	}
}

// TODO TODO: change to struct
pub enum PalletTraitAttr {
	Const(proc_macro2::Span),
}

impl Spanned for PalletTraitAttr {
	fn span(&self) -> proc_macro2::Span {
		match self {
			Self::Const(span) => span.clone(),
		}
	}
}

impl syn::parse::Parse for PalletTraitAttr {
	fn parse(input: syn::parse::ParseStream) -> syn::Result<Self> {
		input.parse::<syn::Token![#]>()?;
		let content;
		syn::bracketed!(content in input);
		content.parse::<syn::Ident>()?;
		content.parse::<syn::Token![::]>()?;

		let lookahead = content.lookahead1();
		if lookahead.peek(keyword::const_) { // TODO TODO: maybe `const` is doable
			Ok(PalletTraitAttr::Const(content.parse::<keyword::const_>()?.span()))
		} else {
			Err(lookahead.error())
		}
	}
}
