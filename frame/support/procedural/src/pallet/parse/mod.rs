mod trait_;
mod module;
mod module_interface;
mod call;
mod error;
mod origin;
mod inherent;
mod event;
pub mod helper;

use syn::spanned::Spanned;

// TODO TODO: maybe check reserved function or just warn ?
// TODO TODO: flag to rename frame_system

/// Parsed definition of a pallet.
pub struct Def {
	pub item: syn::ItemMod,
	pub trait_: trait_::TraitDef,
	pub module: module::ModuleDef,
	pub module_interface: module_interface::ModuleInterfaceDef,
	pub call: call::CallDef,
	// storage: StorageDef,
	pub error: Option<error::ErrorDef>,
	pub event: Option<event::EventDef>,
	pub origin: Option<origin::OriginDef>,
	pub inherent: Option<inherent::InherentDef>,
}

impl Def {
	pub fn try_from(mut item: syn::ItemMod) -> syn::Result<Self> {
		let item_span = item.span().clone();
		let items = &mut item.content.as_mut()
			.ok_or_else(|| {
				let msg = "Invalid pallet definition, expect mod to be inlined.";
				syn::Error::new(item_span, msg)
			})?.1;

		let mut trait_ = None;
		let mut module = None;
		let mut module_interface = None;
		let mut call = None;
		let mut error = None;
		let mut event = None;
		let mut origin = None;
		let mut inherent = None;

		for (index, item) in items.iter_mut().enumerate() {
			let pallet_attr: Option<PalletAttr> = helper::take_first_item_attr(item)?;

			match pallet_attr {
				Some(PalletAttr::Trait) =>
					trait_ = Some(trait_::TraitDef::try_from(index, item)?),
				Some(PalletAttr::Module) =>
					module = Some(module::ModuleDef::try_from(index, item)?),
				Some(PalletAttr::ModuleInterface) => {
					let m = module_interface::ModuleInterfaceDef::try_from(index, item)?;
					module_interface = Some(m);
				},
				Some(PalletAttr::Call) => call = Some(call::CallDef::try_from(index, item)?),
				Some(PalletAttr::Error) => error = Some(error::ErrorDef::try_from(index, item)?),
				Some(PalletAttr::Event) => event = Some(event::EventDef::try_from(index, item)?),
				Some(PalletAttr::Origin) =>
					origin = Some(origin::OriginDef::try_from(index, item)?),
				Some(PalletAttr::Inherent) =>
					inherent = Some(inherent::InherentDef::try_from(index, item)?),
				None => (),
			}
		}

		let def = Def {
			item: item,
			trait_: trait_.ok_or_else(|| syn::Error::new(item_span, "Missing pallet::trait_"))?,
			module: module
				.ok_or_else(|| syn::Error::new(item_span, "Missing pallet::module"))?,
			module_interface: module_interface
				.ok_or_else(|| syn::Error::new(item_span, "Missing pallet::module_interface"))?,
			call: call.ok_or_else(|| syn::Error::new(item_span, "Missing pallet::call"))?,
			error,
			event,
			origin,
			inherent,
		};

		def.check_instance_usage()?;

		Ok(def)
	}

	fn check_instance_usage(&self) -> syn::Result<()> {
		let mut instances = vec![];
		instances.extend_from_slice(&self.call.instances[..]);
		instances.extend_from_slice(&self.module.instances[..]);
		instances.extend_from_slice(&self.module_interface.instances[..]);
		if let Some(event) = &self.event {
			instances.extend_from_slice(&event.instances[..]);
		}
		if let Some(error) = &self.error {
			instances.extend_from_slice(&error.instances[..]);
		}
		if let Some(inherent) = &self.inherent {
			instances.extend_from_slice(&inherent.instances[..]);
		}
		if let Some(origin) = &self.origin {
			instances.extend_from_slice(&origin.instances[..]);
		}

		let mut errors = instances.into_iter()
			.filter_map(|instances| {
				if instances.has_instance == self.trait_.has_instance {
					return None
				}
				let msg = if self.trait_.has_instance {
					"Invalid generic declaration, trait is defined with instance but generic use none"
				} else {
					"Invalid generic declaration, trait is defined without instance but generic use \
						some"
				};
				Some(syn::Error::new(instances.span, msg))
			});

		if let Some(mut first_error) = errors.next() {
			for error in errors {
				first_error.combine(error)
			}
			Err(first_error)
		} else {
			Ok(())
		}
	}

	/// * either `T: Trait`
	/// * or `T: Trait<I>, I: Instance`
	pub fn type_impl_generics(&self) -> proc_macro2::TokenStream {
		if self.trait_.has_instance {
			quote::quote!(T: Trait<I>, I: Instance)
		} else {
			quote::quote!(T: Trait)
		}
	}

	/// * either `T: Trait`
	/// * or `T: Trait<I>, I: 'static + Instance`
	pub fn type_impl_static_generics(&self) -> proc_macro2::TokenStream {
		if self.trait_.has_instance {
			quote::quote!(T: Trait<I>, I: 'static + Instance)
		} else {
			quote::quote!(T: Trait)
		}
	}

	/// * either `T: Trait`
	/// * or `T: Trait<I>, I: Instance = DefaultInstance`
	pub fn type_decl_generics(&self) -> proc_macro2::TokenStream {
		if self.trait_.has_instance {
			quote::quote!(T: Trait<I>, I: Instance = DefaultInstance)
		} else {
			quote::quote!(T: Trait)
		}
	}

	/// * either `T`
	/// * or `T, I`
	pub fn type_use_generics(&self) -> proc_macro2::TokenStream {
		if self.trait_.has_instance {
			quote::quote!(T, I)
		} else {
			quote::quote!(T)
		}
	}

	/// Return path to frame-support crate.
	pub fn scrate(&self) -> proc_macro2::TokenStream {
		// TODO TODO
		quote::quote!(frame_support)
	}
}

/// List of additional token to be used for parsing.
mod keyword {
	syn::custom_keyword!(origin);
	syn::custom_keyword!(call);
	syn::custom_keyword!(event);
	syn::custom_keyword!(module);
	syn::custom_keyword!(trait_);
	syn::custom_keyword!(module_interface);
	syn::custom_keyword!(inherent);
	syn::custom_keyword!(error);
}

/// Parse attributes for item in pallet module
/// syntax must be `pallet::` (e.g. `#[pallet::trait_]`)
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
