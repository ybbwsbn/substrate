// This file is part of Substrate.

// Copyright (C) 2019-2020 Parity Technologies (UK) Ltd.
// SPDX-License-Identifier: Apache-2.0

// Licensed under the Apache License, Version 2.0 (the "License");
// you may not use this file except in compliance with the License.
// You may obtain a copy of the License at
//
// 	http://www.apache.org/licenses/LICENSE-2.0
//
// Unless required by applicable law or agreed to in writing, software
// distributed under the License is distributed on an "AS IS" BASIS,
// WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
// See the License for the specific language governing permissions and
// limitations under the License.

use quote::ToTokens;
use syn::spanned::Spanned;

///#[support2::pallet]
/// ```nocompile
/// pub mod pallet {
/// 	use support2::prelude::*; // Import support2::Map and DoubleMap and Value
///
/// 	#[pallet::trait] // define what is the trait for the pallet
/// 	pub trait Trait<I: Instance = DefaultInstance>: super::system::Trait {
/// 		#[pallet::const] // put the constant in metadata
/// 		type MyGetParam: Get<u32>;
/// 	}
///
/// 	#[pallet::module] // define the module and expand the struct with PhantomData
/// 	pub struct Module<T, I>(core::marker::PhantomData<T, I>);
///
/// 	#[pallet::module_interface]
/// 	impl<T: Trait<I>, I: Instance> ModuleInterface for Module<T, I> {
/// 	// OnFInalize and else
/// 	}
///
/// 	// expand Call variants with the following impl item
/// 	#[pallet::call]
/// 	impl<T: Trait<I>, I: Instance> Call for Module<T, I> {
/// 		fn toto(#origin, #[compact] a: ()) {}
/// 	}
///
/// 	#[pallet::error] // expand metadata,
/// 	pub enum Error {
/// 		/// doc
/// 		InsufficientProposersBalance,
/// 	}
///
/// 	#[event] // expand metadata, derive Clone and stuff (without `T: Clone`).
/// 	#[derive_with_correct_bound(Clone, ...)]
/// 	#[event(metadata(BalanceOf<T> == Balance))] // Set the metadata ident for the type
/// 	pub enum Event<T: Trait> {
/// 		/// doc
/// 		Proposed(<T as frame_system::Trait>::AccountId), // if not define then last path element is metadata (==AccountId)
/// 		/// doc
/// 		Spending(BalanceOf<T>), // if not define then it is BalanceOf
/// 	}
///
/// 	// expand the struct to PhantomData and metadata for its trait implementation
/// 	// (trait implementation must be the following item)
/// 	#[storage]
/// 	pub struct TotalBalance<T>; impl<T: Trait> Map for TotalBalance<T> {}
///
/// 	#[storage] // idem above
/// 	pub struct TotalBalance2<T>; impl<T: Trait> Map for TotalBalance2<T> {}
///
///		pub struct TotalBalance<T> {
///			key: Type
///			query: Type
///		}
/// 	pallet_storage!(TotalBalance config(): map hasher(MyHasher) u32 => u32;);
///
/// 	#[genesis_config]
/// 	struct GenesisConfig {
///			#[default(expr)]
/// 		myfield: u32,
///			#[genesis_config(additional_fields)]
/// 	}
/// 	impl<T: Trait> Build for GenesisConfig<T> {
/// 		fn build(&self) {
/// 			// build stuff your own stuff
///				#[genesis_config(additional_builds)]
/// 		}
/// 	}
///
///		// TODO TODO
/// 	pub enum Origin<T: Trait> {
/// 	}
/// 	// TODO TODO:
/// 	Inherent
/// }
/// ```
pub struct Def {
	trait_: TraitDef,
	module: ModuleDef,
	module_interface: ModuleInterfaceDef,
	call: CallDef,
	// storage: StorageDef,
	// error: Option<ErrorDef>,
	// event: Option<EventDef>,
	// origin: Option<OriginDef>,
	// inherent: Option<InherentDef>,
}

pub struct CallDef {
	has_instance: bool,
	impl_: syn::ItemImpl,
	methods: Vec<CallVariantDef>,
	call: keyword::Call,
}

pub struct CallVariantDef {
	// has_instance: bool, ??
	fn_: syn::Ident,
	args: Vec<(bool, Box<syn::Type>)>,
	weight: syn::Expr,
}

impl CallDef {
	// TODO TODO: maybe give attributes
	fn try_from(item: syn::Item) -> syn::Result<Self> {
		if let syn::Item::Impl(mut item) = item {
			syn::parse2::<CheckImplGenerics>(item.generics.params.to_token_stream())?;
			syn::parse2::<CheckModuleUseType>(item.self_ty.to_token_stream())?;

			let call = item.trait_.take()
				.ok_or_else(|| {
					let msg = "Invalid pallet::call, expect impl.. Call for Module..";
					syn::Error::new(item.span(), msg)
				})?.1;
			let call = syn::parse2::<keyword::Call>(call.to_token_stream())?;

			let mut methods = vec![];
			for impl_item in &mut item.items {
				if let syn::ImplItem::Method(method) = impl_item {
					if method.sig.inputs.len() == 0 {
						let msg = "Invalid pallet::call, must have at least origin arg";
						return Err(syn::Error::new(method.sig.inputs.span(), msg));
					}
					syn::parse2::<CheckDispatchableFirstArg>(method.sig.inputs[0].to_token_stream())?;

					if let syn::ReturnType::Type(_, type_) = &method.sig.output {
						syn::parse2::<keyword::DispatchResultWithPostInfo>(type_.to_token_stream())?;
					} else {
						let msg = "Invalid pallet::call, require return type \
							DispatchResultWithPostInfo";
						return Err(syn::Error::new(method.sig.span(), msg));
					}

					let mut call_var_attrs: Vec<PalletCallVariantAttr> = take_item_attrs(&mut method.attrs)?;

					if call_var_attrs.len() != 1 {
						let msg = if call_var_attrs.len() == 0 {
							"Invalid pallet::call, require weight attribute"
						} else {
							"Invalid pallet::call, to many weight attribute given"
						};
						return Err(syn::Error::new(method.sig.span(), msg));
					}
					let weight = call_var_attrs.pop().unwrap().weight;

					let mut args = vec![];
					for arg in method.sig.inputs.iter_mut().skip(1) {
						if let syn::FnArg::Typed(arg) = arg {
							let arg_attrs: Vec<PalletCallVariantArgAttr> = take_item_attrs(&mut arg.attrs)?;
							if arg_attrs.len() > 1 {
								let msg = "Invalid pallet::call, invalid multiple args";
								return Err(syn::Error::new(arg.span(), msg));
							}
							args.push((!arg_attrs.is_empty(), arg.ty.clone()));
						} else {
							unreachable!("Only first argument can be receiver");
						}
					}

					methods.push(CallVariantDef {
						fn_: method.sig.ident.clone(),
						weight,
						args,
					});
				} else {
					let msg = "Invalid pallet::call, only method accepted";
					return Err(syn::Error::new(impl_item.span(), msg));
				}
			}

			Ok(Self {
				call,
				has_instance: item.generics.params.len() == 2,
				impl_: item,
				methods
			})
		} else {
			Err(syn::Error::new(item.span(), "Invalid pallet::call, expect item impl"))
		}
	}
}

pub struct ConstMetadataDef {
	ident: syn::Ident,
	type_: syn::Type,
}

impl syn::parse::Parse for ConstMetadataDef  {
	fn parse(input: syn::parse::ParseStream) -> syn::Result<Self> {
		syn::Attribute::parse_inner(input)?;
		input.parse::<syn::Token![type]>()?;
		let ident = input.parse::<syn::Ident>()?;
		input.parse::<syn::Token![:]>()?;
		input.parse::<keyword::Get>()?;
		input.parse::<syn::Token![<]>()?;
		let type_ = input.parse::<syn::Type>()?;
		input.parse::<syn::Token![>]>()?;
		input.parse::<syn::Token![;]>()?;

		Ok(Self { ident, type_ })
	}
}

pub struct TraitDef {
	item: syn::ItemTrait,
	// struct_def_generics: `<T: Trait<I>, T: Instance = DefaultInstance> where ..`
	// impl_generics: `<T: Trait<I>, T: Instance> where ..`
	// struct_use_generics: `<T, I>`

	has_instance: bool,

	// `type $ident: Get<$type>`
	consts_metadata: Vec<ConstMetadataDef>, // Maybe add `Get`
	// instance: Span,

	// REQUIRES:
	// - `T`, Trait, I, Instance, ¿where_clause?, then define a function constant metadata used by module,
	// - consts: name type, value=getter, docs
}

mod keyword {
	syn::custom_keyword!(I);
	syn::custom_keyword!(weight);
	syn::custom_keyword!(compact);
	syn::custom_keyword!(OriginFor);
	syn::custom_keyword!(origin);
	syn::custom_keyword!(DispatchResultWithPostInfo);
	syn::custom_keyword!(Module);
	syn::custom_keyword!(Call);
	syn::custom_keyword!(call);
	syn::custom_keyword!(Trait);
	syn::custom_keyword!(T);
	syn::custom_keyword!(Instance);
	syn::custom_keyword!(DefaultInstance);
	syn::custom_keyword!(Get);
	syn::custom_keyword!(trait_);
	syn::custom_keyword!(const_);
	syn::custom_keyword!(module);
	syn::custom_keyword!(module_interface);
}

// TODO TODO: all those check must give hint about the wanted structure.
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

impl TraitDef {
	// TODO TODO: maybe give attributes
	fn try_from(item: syn::Item) -> syn::Result<Self> {
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

pub struct ModuleDef {
	item: syn::ItemStruct,
	has_instance: bool,
}

impl ModuleDef {
	// TODO TODO: maybe give attributes
	// Check has one or two generics named T or T, I and default instance is set
	fn try_from(item: syn::Item) -> syn::Result<Self> {
		if let syn::Item::Struct(item) = item {
			syn::parse2::<keyword::Module>(item.ident.to_token_stream())?;

			if !matches!(item.vis, syn::Visibility::Public(_)) {
				let msg = "Invalid pallet::module, Module must be public";
				return Err(syn::Error::new(item.span(), msg));
			}

			syn::parse2::<CheckStructDefGenerics>(item.generics.params.to_token_stream())?;
			let has_instance = item.generics.params.len() == 2;

			// TODO TODO : also check fields.

			Ok(Self { item, has_instance })
		} else {
			let msg = "Invalid pallet::module, expect struct definition";
			Err(syn::Error::new(item.span(), msg))
		}
	}
}

pub struct ModuleInterfaceDef {
	item: syn::ItemImpl,
}

impl ModuleInterfaceDef {
	// TODO TODO: maybe give attributes
	// Check has one or two generics named T or T, I and default instance is set
	fn try_from(item: syn::Item) -> syn::Result<Self> {
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

			// TODO TODO: we can check: it is for Module with correct generics, and impl_bounds has correct generics
			Ok(Self { item })
		} else {
			let msg = "Invalid pallet::module_interface, expect item impl";
			Err(syn::Error::new(item.span(), msg))
		}
	}
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
			Self::Verbatim(_) => todo!("unreachable because we don't expanded yet"),
			Self::__Nonexhaustive => todo!(),
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
			Self::Verbatim(_) => todo!("unreachable because we don't expanded yet"),
			Self::__Nonexhaustive => todo!(),
		}
	}
}

impl MutItemAttrs for Vec<syn::Attribute> {
	fn mut_item_attrs(&mut self) -> &mut Vec<syn::Attribute> {
		self
	}
}


// start with pallet::
pub enum PalletAttr {
	Trait,
	Module,
	ModuleInterface,
	Call,
}

impl syn::parse::Parse for PalletAttr {
	fn parse(input: syn::parse::ParseStream) -> syn::Result<Self> {
		input.parse::<syn::Token![#]>()?;
		let content;
		syn::bracketed!(content in input);
		content.parse::<syn::Ident>()?;
		content.parse::<syn::Token![::]>()?;

		let lookahead = content.lookahead1();
		if lookahead.peek(keyword::trait_) { // TODO TODO: maybe `trait` is doable
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
		} else {
			Err(lookahead.error())
		}
	}
}

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

pub struct PalletCallVariantAttr {
	weight: syn::Expr,
}

impl syn::parse::Parse for PalletCallVariantAttr {
	fn parse(input: syn::parse::ParseStream) -> syn::Result<Self> {
		input.parse::<syn::Token![#]>()?;
		let content;
		syn::bracketed!(content in input);
		content.parse::<syn::Ident>()?;
		content.parse::<syn::Token![::]>()?;

		let lookahead = content.lookahead1();
		if lookahead.peek(keyword::weight) {
			content.parse::<keyword::weight>()?;
			content.parse::<syn::Token![=]>()?;

			Ok(PalletCallVariantAttr {
				weight: content.parse::<syn::Expr>()?,
			})
		} else {
			Err(lookahead.error())
		}
	}
}

/// I.e. is_compact
pub struct PalletCallVariantArgAttr;

impl syn::parse::Parse for PalletCallVariantArgAttr {
	fn parse(input: syn::parse::ParseStream) -> syn::Result<Self> {
		input.parse::<syn::Token![#]>()?;
		let content;
		syn::bracketed!(content in input);
		content.parse::<syn::Ident>()?;
		content.parse::<syn::Token![::]>()?;

		let lookahead = content.lookahead1();
		if lookahead.peek(keyword::compact) {
			content.parse::<keyword::compact>()?;
			Ok(PalletCallVariantArgAttr)
		} else {
			Err(lookahead.error())
		}
	}
}

/// Implemented on syn item to have mutable references on its attributes.
trait MutItemAttrs {
	fn mut_item_attrs(&mut self) -> &mut Vec<syn::Attribute>;
}

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

pub fn pallet(_attr: proc_macro::TokenStream, item: proc_macro::TokenStream) -> proc_macro::TokenStream {
	// TODO TODO: assert attr is empty
	let item = syn::parse_macro_input!(item as syn::ItemMod);
	match pallet_from_item_mod(item) {
		Ok(s) => s.into(),
		Err(e) => e.to_compile_error().into(),
	}
}

pub fn pallet_from_item_mod(item: syn::ItemMod) -> syn::Result<proc_macro2::TokenStream> {
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
	let mut other_items = vec![];

	for mut item in items.drain(..) {
		let pallet_attrs: Vec<PalletAttr> = take_item_attrs(&mut item)?;

		assert!(pallet_attrs.len() <= 1, "TODO TODO");

		match pallet_attrs.first() {
			Some(PalletAttr::Trait) => trait_def = Some(TraitDef::try_from(item)?),
			Some(PalletAttr::Module) => module_def = Some(ModuleDef::try_from(item)?),
			Some(PalletAttr::ModuleInterface) =>
				module_interface_def = Some(ModuleInterfaceDef::try_from(item)?),
			Some(PalletAttr::Call) => call_def = Some(CallDef::try_from(item)?),
			None => other_items.push(item),
		}
	}

	let _def = Def {
		trait_: trait_def.ok_or_else(|| syn::Error::new(item_span, "Missing pallet::trait_"))?,
		module: module_def.ok_or_else(|| syn::Error::new(item_span, "Missing pallet::module"))?,
		module_interface: module_interface_def
			.ok_or_else(|| syn::Error::new(item_span, "Missing pallet::module_interface"))?,
		call: call_def.ok_or_else(|| syn::Error::new(item_span, "Missing pallet::call"))?,
	};

	// // TODO TODO: search for:
	// // * trait -> const metadata
	// // * module -> implement stuff on it
	// // * call -> call enum
	// // * error -> error metadata
	// // * event -> event metadata

	// // TODO TODO: for decl_event we can easily implement a good derive just by assigning where
	// // clause to each field
	// // Trait is just read and generate const_metadata
	// let trait_item = ();
	// // Trait is just read or modified to add phantomdata, see with IDE
	// let module_item = ();
	// // genesis is just read and generate maybe something for AutoConstructRuntime
	// let genesis_config_item = ();
	// // Derive manually Clone, Eq and PartialEq same as call or same as Encode/Decode
	// // generate metadata and associate to module.
	// // Also define deposit_event
	// // Check that overarching associated type Event in trait exists
	// let event_item = ();
	// // Add some variant and generate metadata and associate to Module
	// let error_item = ();
	// // Call is expanded!
	// let call_item = ();
	// // Item is just read or modified to add phantomdata, see with IDE
	// // And generate StoragePrefix
	// let store_decl_item = ();
	// // Trait is just read and Generate metadata
	// let store_impl_item = ();

	// for item in item.content.unwrap().1.iter() {
	// }
	todo!("expand");
}
