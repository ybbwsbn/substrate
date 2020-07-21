mod trait_;
mod module;
mod module_interface;
mod call;
mod error;
mod origin;
mod inherent;
mod event;

use syn::spanned::Spanned;

fn expand_trait_(def: &Def) -> proc_macro2::TokenStream {
	let item = &def.trait_.item;
	let scrate = &def.scrate();
	let type_impl_gen = &def.impl_block_generics();
	let type_decl_gen = &def.type_decl_generics();
	let type_use_gen = &def.type_use_generics();

	let consts = def.trait_.consts_metadata.iter()
		.map(|const_| {
			let type_ = &const_.type_;
			let type_str = format!("{:?}", type_);
			let ident = &const_.ident;
			let ident_str = format!("{}", ident);
			let doc = const_.doc.clone().into_iter();
			let default_byte_getter = syn::Ident::new(
				&format!("{}DefaultByteGetter", ident),
				ident.span()
			);

			quote::quote!({
				#[allow(non_upper_case_types)]
				#[allow(non_camel_case_types)]
				struct #default_byte_getter<#type_decl_gen>(
					#scrate::sp_std::marker::PhantomData<(#type_use_gen)>
				);

				impl<#type_impl_gen> #scrate::dispatch::DefaultByte for
					#default_byte_getter<#type_use_gen>
				{
					fn default_byte(&self) -> #scrate::sp_std::vec::Vec<u8> {
						let value = <T::#ident as #scrate::traits::Get<#type_>>::get();
						#scrate::codec::Encode::encode(&value)
					}
				}

				unsafe impl<#type_impl_gen> Send for $default_byte_getter<#type_use_gen> {}
				unsafe impl<#type_impl_gen> Sync for $default_byte_getter<#type_use_gen> {}

				$crate::dispatch::ModuleConstantMetadata {
					name: $crate::dispatch::DecodeDifferent::Encode(#type_str),
					ty: $crate::dispatch::DecodeDifferent::Encode(#ident_str),
					value: $crate::dispatch::DecodeDifferent::Encode(
						$crate::dispatch::DefaultByteGetter(
							&$default_byte_getter::<#type_use_gen>(
								$crate::sp_std::marker::PhantomData
							)
						)
					),
					documentation: $crate::dispatch::DecodeDifferent::Encode(
						&[ #( #doc )* ]
					),
				}
			})
		});

	quote::quote!(
		impl<#type_impl_gen> Module<#type_use_gen> {

			#[doc(hidden)]
			pub fn module_constants_metadata()
				-> &'static [#scrate::dispatch::ModuleConstantMetadata]
			{
				&[ #( #consts )* ]
			}
		}

		#item
	)
}

// TODO TODO: maybe add derive for Clone Copy and else (as done by decl_module)
fn expand_module(def: &Def) -> proc_macro2::TokenStream {
	let item = &def.module.item;
	quote::quote!(
		#item
	)
}

fn expand_module_interface(def: &Def) -> proc_macro2::TokenStream {
	let item = &def.module_interface.item;
	quote::quote!(
		#item
	)
}

fn expand_inherent(def: &Def) -> proc_macro2::TokenStream {
	if let Some(inherent) = &def.inherent {
		let item = &inherent.item;
		quote::quote!(
			#item
		)
	} else {
		Default::default()
	}
}

// TODO TODO: for each `format!(` check that space are correctly handled!!

fn expand_event(def: &Def) -> proc_macro2::TokenStream {
	let event = if let Some(event) = &def.event {
		event
	} else {
		return Default::default()
	};

	let mut item = event.item.clone();
	let item_ident = &event.item.ident;
	let scrate = &def.scrate();
	let event_use_gen = &event.event_use_gen();
	let event_impl_gen= &event.event_impl_gen();
	let metadata = event.metadata.iter()
		.map(|(ident, args, docs)| {
			let name = format!("{}", ident);
			quote::quote!(
				#scrate::event::EventMetadata {
					name: #scrate::event::DecodeDifferent::Encode(#name),
					arguments: #scrate::event::DecodeDifferent::Encode(&[ #( #args )* ]),
					documentation: $crate::event::DecodeDifferent::Encode(&[ #( #docs )* ]),
				},
			)
		});

	// Phantom data is added for generic event.
	if event.is_generic {
		let variant = syn::parse2(quote::quote!(
			#[doc(hidden)]
			#[codec(skip)]
			__Ignore(
				#scrate::sp_std::marker::PhantomData<(#event_use_gen)>,
				#scrate::Never,
			),
		)).expect("Internal error: internally constructed variant should be valid");

		// Push ignore variant at the end.
		item.variants.push(variant);
	}

	// Codec is derived TODO TODO
	// For Clone, PartialEq and Eq this must be that all type implement Parameter.
	// Enforcing them is fine because anyway this should be doable with Codec
	// Or just create one similar to Codec

	quote::quote!(
			// TODO TODO: derive Clone, PartialEq, Eq
			// TODO TODO: Debug manually
			Clone, PartialEq, Eq,
			$crate::codec::Encode,
			$crate::codec::Decode,
			$crate::RuntimeDebug,

		#item_with_phantom_data

		impl<#event_impl_gen> From<#item_ident<#event_use_gen>> for () {
			fn from(_: #item_ident<#event_use_gen>) -> () { () }
		}

		impl<#event_impl_gen> #item_ident<#event_use_gen> {
			#[allow(dead_code)]
			#[doc(hidden)]
			pub fn metadata() -> &'static [#scrate::event::EventMetadata] {
				&[ #( #metadata )* ]
			}
		}
	)
}

fn expand_error(def: &Def) -> proc_macro2::TokenStream {
	let error = if let Some(error) = &def.error {
		error
	} else {
		return Default::default()
	};

	let item = &error.item;
	let item_ident = &error.item.ident;
	let scrate = &def.scrate();
	let type_impl_gen = &def.impl_block_generics();
	let type_decl_gen = &def.type_decl_generics();
	let type_use_gen = &def.type_use_generics();

	let item_with_phantom_data = {
		let mut i = item.clone();
		let variant = syn::parse2(quote::quote!(
			#[doc(hidden)]
			__Ignore(
				#scrate::sp_std::marker::PhantomData<(#type_use_gen)>,
				#scrate::Never,
			),
		)).expect("Internal error: internally constructed variant should be valid");

		i.variants.insert(0, variant);
		i
	};

	let as_u8_matches = error.variants.iter()
		.enumerate()
		.map(|(i, (variant, _))| quote::quote!(Self::#variant => #i,));

	let as_str_matches = error.variants.iter()
		.map(|(variant, _)| {
			let variant_str = format!("{}", variant);
			quote::quote!(Self::#variant => #variant_str,)
		});

	let metadata = error.variants.iter()
		.map(|(variant, doc)| {
			let variant_str = format!("{}", variant);
			quote::quote!(
				#scrate::error::ErrorMetadata {
					name: #scrate::error::DecodeDifferent::Encode(#variant_str),
					documentation: #scrate::error::DecodeDifferent::Encode(&[ #( #doc )* ]),
				}
			)
		});

	quote::quote!(
		#item

		impl<#type_impl_gen> #scrate::sp_std::fmt::Debug for #item_ident<#type_use_gen> {
			fn fmt(&self, f: &mut $crate::sp_std::fmt::Formatter<'_>)
				-> $crate::sp_std::fmt::Result
			{
				f.write_str(self.as_str())
			}
		}

		impl<#type_impl_gen> #item_ident<#type_use_gen> {
			fn as_u8(&self) -> u8 {
				match &self {
					Self::__Ignore(_, _) => unreachable!("`__Ignore` can never be constructed"),
					#( #as_u8_matches )*
				}
			}

			fn as_str(&self) -> &'static str {
				match &self {
					Self::__Ignore(_, _) => unreachable!("`__Ignore` can never be constructed"),
					#( #as_str_matches )*
				}
			}
		}

		impl<#type_impl_gen> From<#item_ident<#type_use_gen>> for &'static str {
			fn from(err: #item_ident<#type_use_gen>) -> &'static str {
				err.as_str()
			}
		}

		impl<#type_impl_gen> From<#item_ident<#type_use_gen>>
			for #scrate::sp_runtime::DispatchError
		{
			fn from(err: #item_ident<#type_use_gen>) -> Self {
				let index = <
					<T as #scrate::frame_system::Trait>::ModuleToIndex
					as #scrate::traits::ModuleToIndex
				>::module_to_index::<Module<#type_use_gen>>()
					.expect("Every active module has an index in the runtime; qed") as u8;

				#scrate::sp_runtime::DispatchError::Module {
					index,
					error: err.as_u8(),
					message: Some(err.as_str()),
				}
			}
		}

		impl<#type_impl_gen> #scrate::error::ModuleErrorMetadata for #item_ident<#type_use_gen> {
			fn metadata() -> &'static [#scrate::error::ErrorMetadata] {
				&[ #( #metadata )* ]
			}
		}
	)
}

fn expand_call(def: &Def) -> proc_macro2::TokenStream {
	todo!();
}

fn expand_origin(def: &Def) -> proc_macro2::TokenStream {
	if let Some(origin) = &def.origin {
		let item = &origin.item;
		quote::quote!(
			#item
		)
	} else {
		Default::default()
	}
}

/// Parsed definition of a pallet.
pub struct Def {
	trait_: trait_::TraitDef,
	module: module::ModuleDef,
	module_interface: module_interface::ModuleInterfaceDef,
	call: call::CallDef,
	// storage: StorageDef,
	error: Option<error::ErrorDef>,
	event: Option<event::EventDef>,
	origin: Option<origin::OriginDef>,
	inherent: Option<inherent::InherentDef>,
}

impl Def {
	pub fn try_from(item: syn::ItemMod) -> syn::Result<Self> {
		let item_span = item.span().clone();
		let items = &mut item.content.expect("TODO TODO: mandatory inline").1;

		// First passe get information for generations
		// Second passe generate stuff with information and modify items with information
		//
		// Maybe the first passe should return something constructed with def and second passe will
		// only modify it.

		let mut trait_def = None;
		let mut module_def = None;
		let mut module_interface_def = None;
		let mut call_def = None;
		let mut error_def = None;
		let mut event_def = None;
		let mut origin_def = None;
		let mut inherent_def = None;
		let mut other_items = vec![];

		for mut item in items.drain(..) {
			let pallet_attr: Option<PalletAttr> = take_first_item_attr(&mut item)?;

			match pallet_attr {
				Some(PalletAttr::Trait) => trait_def = Some(trait_::TraitDef::try_from(item)?),
				Some(PalletAttr::Module) => module_def = Some(module::ModuleDef::try_from(item)?),
				Some(PalletAttr::ModuleInterface) => {
					let module_interface = module_interface::ModuleInterfaceDef::try_from(item)?;
					module_interface_def = Some(module_interface);
				},
				Some(PalletAttr::Call) => call_def = Some(call::CallDef::try_from(item)?),
				Some(PalletAttr::Error) => error_def = Some(error::ErrorDef::try_from(item)?),
				Some(PalletAttr::Event) => event_def = Some(event::EventDef::try_from(item)?),
				Some(PalletAttr::Origin) => origin_def = Some(origin::OriginDef::try_from(item)?),
				Some(PalletAttr::Inherent) =>
					inherent_def = Some(inherent::InherentDef::try_from(item)?),
				None => other_items.push(item),
			}
		}

		Ok(Def {
			trait_: trait_def.ok_or_else(|| syn::Error::new(item_span, "Missing pallet::trait_"))?,
			module: module_def
				.ok_or_else(|| syn::Error::new(item_span, "Missing pallet::module"))?,
			module_interface: module_interface_def
				.ok_or_else(|| syn::Error::new(item_span, "Missing pallet::module_interface"))?,
			call: call_def.ok_or_else(|| syn::Error::new(item_span, "Missing pallet::call"))?,
			error: error_def,
			event: event_def,
			origin: origin_def,
			inherent: inherent_def,
		})
	}

	/// * either `T: Trait`
	/// * or `T: Trait<I>, I: Instance`
	fn impl_block_generics(&self) -> proc_macro2::TokenStream {
		if self.trait_.has_instance {
			quote::quote!(T: Trait<I>, I: Instance)
		} else {
			quote::quote!(T: Trait)
		}
	}

	/// * either `T: Trait`
	/// * or `T: Trait<I>, I: Instance = DefaultInstance`
	fn type_decl_generics(&self) -> proc_macro2::TokenStream {
		if self.trait_.has_instance {
			quote::quote!(T: Trait<I>, I: Instance = DefaultInstance)
		} else {
			quote::quote!(T: Trait)
		}
	}

	/// * either `T`
	/// * or `T, I`
	fn type_use_generics(&self) -> proc_macro2::TokenStream {
		if self.trait_.has_instance {
			quote::quote!(T, I)
		} else {
			quote::quote!(T)
		}
	}

	/// Return path to frame-support crate.
	fn scrate(&self) -> proc_macro2::TokenStream {
		// TODO TODO
		quote::quote!(frame_support)
	}
}

// TODO TODO: for all def, also check there is no longer pallet attributes, or just make all inner
// attribute being something like event instead of pallet::event_metadata

/// List of additional token to be used for parsing.
mod keyword {
	syn::custom_keyword!(I);
	syn::custom_keyword!(inherent);
	syn::custom_keyword!(weight);
	syn::custom_keyword!(event);
	syn::custom_keyword!(compact);
	syn::custom_keyword!(OriginFor);
	syn::custom_keyword!(origin);
	syn::custom_keyword!(call);
	syn::custom_keyword!(Trait);
	syn::custom_keyword!(T);
	syn::custom_keyword!(Instance);
	syn::custom_keyword!(DefaultInstance);
	syn::custom_keyword!(module);
	syn::custom_keyword!(Module);
	syn::custom_keyword!(trait_);
	syn::custom_keyword!(module_interface);
	syn::custom_keyword!(error);
}

// TODO TODO: all those check must give hint about the wanted structure.
/// Check the syntax: `I: Instance = DefaultInstance`
pub struct CheckTraitDefGenerics;
impl syn::parse::Parse for CheckTraitDefGenerics {
	fn parse(input: syn::parse::ParseStream) -> syn::Result<Self> {
		input.parse::<keyword::I>()?;
		input.parse::<syn::Token![:]>()?;
		input.parse::<keyword::Instance>()?;
		input.parse::<syn::Token![=]>()?;
		input.parse::<keyword::DefaultInstance>()?;
		// TODO TODO: parse terminated.

		Ok(Self)
	}
}

/// Check the syntax: `origin: OriginFor<T>`
pub struct CheckDispatchableFirstArg;
impl syn::parse::Parse for CheckDispatchableFirstArg {
	fn parse(input: syn::parse::ParseStream) -> syn::Result<Self> {
		input.parse::<keyword::origin>()?;
		input.parse::<syn::Token![:]>()?;
		input.parse::<keyword::OriginFor>()?;
		input.parse::<syn::Token![<]>()?;
		input.parse::<keyword::T>()?;
		input.parse::<syn::Token![>]>()?;

		// TODO TODO: parse terminated.

		Ok(Self)
	}
}

/// Check the syntax:
/// * either `T`
/// * or `T, I = DefaultInstance`
pub struct CheckStructDefGenerics;
impl syn::parse::Parse for CheckStructDefGenerics {
	fn parse(input: syn::parse::ParseStream) -> syn::Result<Self> {
		input.parse::<keyword::T>()?;
		if input.peek(syn::Token![,]) {
			input.parse::<syn::Token![,]>()?;
			input.parse::<keyword::I>()?;
			input.parse::<syn::Token![=]>()?;
			input.parse::<keyword::DefaultInstance>()?;
		}
		// TODO TODO: parse terminated.

		Ok(Self)
	}
}

/// Check the syntax:
/// * either `T`
/// * or `T, I = DefaultInstance`
/// * or nothing
pub struct CheckTypeDefOptionalGenerics;
impl syn::parse::Parse for CheckTypeDefOptionalGenerics {
	fn parse(input: syn::parse::ParseStream) -> syn::Result<Self> {
		if input.is_empty() {
			return Ok(Self)
		}

		input.parse::<keyword::T>()?;
		if input.peek(syn::Token![,]) {
			input.parse::<syn::Token![,]>()?;
			input.parse::<keyword::I>()?;
			input.parse::<syn::Token![=]>()?;
			input.parse::<keyword::DefaultInstance>()?;
		}
		// TODO TODO: parse terminated.

		Ok(Self)
	}
}

/// Check the syntax:
/// * either `Module<T>`
/// * or `Module<T, I>`
pub struct CheckModuleUseType;
impl syn::parse::Parse for CheckModuleUseType {
	fn parse(input: syn::parse::ParseStream) -> syn::Result<Self> {
		input.parse::<keyword::Module>()?;
		input.parse::<syn::Token![<]>()?;
		input.parse::<keyword::T>()?;
		if input.peek(syn::Token![,]) {
			input.parse::<syn::Token![,]>()?;
			input.parse::<keyword::I>()?;
		}
		input.parse::<syn::Token![>]>()?;
		// TODO TODO: parse terminated.

		Ok(Self)
	}
}

/// Check the syntax:
/// * either `T`
/// * or `T, I`
pub struct CheckStructUseGenerics;
impl syn::parse::Parse for CheckStructUseGenerics {
	fn parse(input: syn::parse::ParseStream) -> syn::Result<Self> {
		input.parse::<keyword::T>()?;
		if input.peek(syn::Token![,]) {
			input.parse::<syn::Token![,]>()?;
			input.parse::<keyword::I>()?;
		}
		// TODO TODO: parse terminated.

		Ok(Self)
	}
}

/// Check the syntax:
/// * either `T: Trait`
/// * or `T: Trait<I>, I: Instance`
pub struct CheckImplGenerics;
impl syn::parse::Parse for CheckImplGenerics {
	fn parse(input: syn::parse::ParseStream) -> syn::Result<Self> {
		input.parse::<keyword::T>()?;
		input.parse::<syn::Token![:]>()?;
		input.parse::<keyword::Trait>()?;
		if input.peek(syn::Token![<]) {
			input.parse::<syn::Token![<]>()?;
			input.parse::<keyword::I>()?;
			input.parse::<syn::Token![>]>()?;
			input.parse::<syn::Token![,]>()?;
			input.parse::<keyword::I>()?;
			input.parse::<syn::Token![:]>()?;
			input.parse::<keyword::Instance>()?;
		}
		// TODO TODO: parse terminated.

		Ok(Self)
	}
}

/// Parse attribute which starts with `pallet::` (e.g. `#[pallet::trait_]`)
pub enum PalletAttr {
	Trait,
	Module,
	ModuleInterface,
	Call,
	Error,
	Event,
	Origin,
	Inherent,
}

impl syn::parse::Parse for PalletAttr {
	fn parse(input: syn::parse::ParseStream) -> syn::Result<Self> {
		input.parse::<syn::Token![#]>()?;
		let content;
		syn::bracketed!(content in input);
		content.parse::<syn::Ident>()?;
		content.parse::<syn::Token![::]>()?;

		let lookahead = content.lookahead1();
		if lookahead.peek(keyword::trait_) {
			content.parse::<keyword::trait_>()?;
			Ok(PalletAttr::Trait)
		} else if lookahead.peek(keyword::module) {
			content.parse::<keyword::module>()?;
			Ok(PalletAttr::Module)
		} else if lookahead.peek(keyword::module_interface) {
			content.parse::<keyword::module_interface>()?;
			Ok(PalletAttr::ModuleInterface)
		} else if lookahead.peek(keyword::call) {
			content.parse::<keyword::call>()?;
			Ok(PalletAttr::Call)
		} else if lookahead.peek(keyword::error) {
			content.parse::<keyword::error>()?;
			Ok(PalletAttr::Error)
		} else if lookahead.peek(keyword::event) {
			content.parse::<keyword::event>()?;
			Ok(PalletAttr::Event)
		} else if lookahead.peek(keyword::origin) {
			content.parse::<keyword::origin>()?;
			Ok(PalletAttr::Origin)
		} else if lookahead.peek(keyword::inherent) {
			content.parse::<keyword::inherent>()?;
			Ok(PalletAttr::Inherent)
		} else {
			Err(lookahead.error())
		}
	}
}

/// Trait implemented for syn items to get mutable references on their attributes.
///
/// NOTE: verbatim variants are not supported.
trait MutItemAttrs {
	fn mut_item_attrs(&mut self) -> &mut Vec<syn::Attribute>;
}

/// Take the first pallet attribute (e.g. attribute like `#[pallet..]`) and decode it to `Attr`
fn take_first_item_attr<Attr>(item: &mut impl MutItemAttrs) -> syn::Result<Option<Attr>> where
	Attr: syn::parse::Parse,
{
	let mut pallet_attr = None;

	// TODO TODO: factorize
	let attrs = item.mut_item_attrs();
	*attrs = attrs.drain(..)
		.filter_map(|attr| {
			if pallet_attr.is_none()
				&& attr.path.segments.first().map_or(false, |segment| segment.ident == "pallet")
			{
				pallet_attr = Some(attr);
				None
			} else {
				Some(attr)
			}
		})
		.collect();

	pallet_attr
		.map(quote::ToTokens::into_token_stream)
		.map(syn::parse2::<Attr>)
		.transpose()
}

// TODO TODO: factorize
/// Take all the pallet attributes (e.g. attribute like `#[pallet..]`) and decode them to `Attr`
fn take_item_attrs<Attr>(item: &mut impl MutItemAttrs) -> syn::Result<Vec<Attr>> where
	Attr: syn::parse::Parse,
{
	let mut pallet_attrs = Vec::new();

	let attrs = item.mut_item_attrs();
	*attrs = attrs.drain(..)
		.filter_map(|attr| {
			if attr.path.segments.first().map_or(false, |segment| segment.ident == "pallet") {
				pallet_attrs.push(attr);
				None
			} else {
				Some(attr)
			}
		})
		.collect();

	pallet_attrs.drain(..)
		.map(quote::ToTokens::into_token_stream)
		.map(syn::parse2::<Attr>)
		.collect()
}

impl MutItemAttrs for syn::Item {
	fn mut_item_attrs(&mut self) -> &mut Vec<syn::Attribute> {
		match self {
			Self::Const(item) => item.attrs.as_mut(),
			Self::Enum(item) => item.attrs.as_mut(),
			Self::ExternCrate(item) => item.attrs.as_mut(),
			Self::Fn(item) => item.attrs.as_mut(),
			Self::ForeignMod(item) => item.attrs.as_mut(),
			Self::Impl(item) => item.attrs.as_mut(),
			Self::Macro(item) => item.attrs.as_mut(),
			Self::Macro2(item) => item.attrs.as_mut(),
			Self::Mod(item) => item.attrs.as_mut(),
			Self::Static(item) => item.attrs.as_mut(),
			Self::Struct(item) => item.attrs.as_mut(),
			Self::Trait(item) => item.attrs.as_mut(),
			Self::TraitAlias(item) => item.attrs.as_mut(),
			Self::Type(item) => item.attrs.as_mut(),
			Self::Union(item) => item.attrs.as_mut(),
			Self::Use(item) => item.attrs.as_mut(),
			Self::Verbatim(_) => panic!("Internal Error: Verbatim variants are not supported"),
			Self::__Nonexhaustive => panic!("Internal Error: uncovered variant"),
		}
	}
}


impl MutItemAttrs for syn::TraitItem {
	fn mut_item_attrs(&mut self) -> &mut Vec<syn::Attribute> {
		match self {
			Self::Const(item) => item.attrs.as_mut(),
			Self::Method(item) => item.attrs.as_mut(),
			Self::Type(item) => item.attrs.as_mut(),
			Self::Macro(item) => item.attrs.as_mut(),
			Self::Verbatim(_) => panic!("Internal Error: Verbatim variants are not supported"),
			Self::__Nonexhaustive => panic!("Internal Error: uncovered variant"),
		}
	}
}

impl MutItemAttrs for Vec<syn::Attribute> {
	fn mut_item_attrs(&mut self) -> &mut Vec<syn::Attribute> {
		self
	}
}

/// Return all doc attributes literals found.
fn get_doc_literals(attrs: &Vec<syn::Attribute>) -> Vec<syn::Lit> {
	attrs.iter()
		.filter_map(|attr| {
			if let Ok(syn::Meta::NameValue(meta)) = attr.parse_meta() {
				if meta.path.get_ident().map_or(false, |ident| ident == "doc") {
					Some(meta.lit.clone())
				} else {
					None
				}
			} else {
				None
			}
		})
		.collect()
}
