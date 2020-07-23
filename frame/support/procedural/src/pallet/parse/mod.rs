mod trait_;
mod module;
mod module_interface;
mod call;
mod error;
mod origin;
mod inherent;
mod event;

use syn::spanned::Spanned;
use quote::ToTokens;

// TODO TODO: maybe check reserved function or just warn ?
// TODO TODO: flag to rename frame_system

pub fn expand(mut def: Def) -> proc_macro2::TokenStream {
	let trait_ = expand_trait_(&def);
	let module = expand_module(&def);
	let module_interface = expand_module_interface(&def);
	let call = expand_call(&def);
	let error = expand_error(&def);
	let origin = expand_origin(&def);
	let inherent = expand_inherent(&def);
	let event = expand_event(&def);

	let new_items = quote::quote!(
		#trait_
		#module
		#module_interface
		#call
		#error
		#origin
		#inherent
		#event
	);

	def.item.content.as_mut().expect("This is checked by parsing").1
		.push(syn::Item::Verbatim(new_items));

	def.item.into_token_stream()
}

fn expand_trait_(def: &Def) -> proc_macro2::TokenStream {
	let item = &def.trait_.item;
	let scrate = &def.scrate();
	let type_impl_gen = &def.type_impl_generics();
	let type_impl_static_gen = &def.type_impl_static_generics();
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

				unsafe impl<#type_impl_gen> Send for #default_byte_getter<#type_use_gen> {}
				unsafe impl<#type_impl_gen> Sync for #default_byte_getter<#type_use_gen> {}

				#scrate::dispatch::ModuleConstantMetadata {
					name: #scrate::dispatch::DecodeDifferent::Encode(#type_str),
					ty: #scrate::dispatch::DecodeDifferent::Encode(#ident_str),
					value: #scrate::dispatch::DecodeDifferent::Encode(
						#scrate::dispatch::DefaultByteGetter(
							&#default_byte_getter::<#type_use_gen>(
								#scrate::sp_std::marker::PhantomData
							)
						)
					),
					documentation: #scrate::dispatch::DecodeDifferent::Encode(
						&[ #( #doc )* ]
					),
				}
			})
		});

	quote::quote!(
		impl<#type_impl_static_gen> Module<#type_use_gen> {

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

fn expand_module(def: &Def) -> proc_macro2::TokenStream {
	let scrate = &def.scrate();
	let mut item = def.module.item.clone();
	item.attrs.push(syn::parse_quote!(
		#[derive(
			#scrate::CloneBoundTypes,
			#scrate::EqBoundTypes,
			#scrate::PartialEqBoundTypes,
			#scrate::DebugStripped,
		)]
	));

	quote::quote!(
		#item

		// TODO TODO:
		// impl<$trait_instance: $trait_name $(<I>, $instance: $instantiable)?> $crate::dispatch::ModuleErrorMetadata
		// 	for $mod_type<$trait_instance $(, $instance)?> where $( $other_where_bounds )*
		// {
		// 	fn metadata() -> &'static [$crate::dispatch::ErrorMetadata] {
		// 		<$error_type as $crate::dispatch::ModuleErrorMetadata>::metadata()
		// 	}
		// }
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
					arguments: #scrate::event::DecodeDifferent::Encode(&[ #( stringify!(#args), )* ]),
					documentation: #scrate::event::DecodeDifferent::Encode(&[ #( #docs, )* ]),
				},
			)
		});

	// Phantom data is added for generic event.
	if event.is_generic {
		let variant = syn::parse_quote!(
			#[doc(hidden)]
			#[codec(skip)]
			__Ignore(
				#scrate::sp_std::marker::PhantomData<(#event_use_gen)>,
				#scrate::Never,
			)
		);

		// Push ignore variant at the end.
		item.variants.push(variant);
	}

	item.attrs.push(syn::parse_quote!(
		#[derive(
			#scrate::codec::Encode,
			#scrate::codec::Decode,
			#scrate::CloneBoundTypes,
			#scrate::EqBoundTypes,
			#scrate::PartialEqBoundTypes,
		)]
	));

	item.attrs.push(syn::parse_quote!(
		#[cfg_attr(feature = "std", derive(#scrate::DebugBoundTypes))]
	));

	item.attrs.push(syn::parse_quote!(
		#[cfg_attr(not(feature = "std"), derive(#scrate::DebugStripped))]
	));

	quote::quote!(
		#item

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

	let mut item = error.item.clone();
	let item_ident = &error.item.ident;
	let scrate = &def.scrate();
	let type_impl_gen = &def.type_impl_generics();
	let type_impl_static_gen = &def.type_impl_static_generics();
	let type_use_gen = &def.type_use_generics();

	let phantom_variant: syn::Variant = syn::parse_quote!(
		#[doc(hidden)]
		__Ignore(
			#scrate::sp_std::marker::PhantomData<(#type_use_gen)>,
			#scrate::Never,
		)
	);
	item.variants.insert(0, phantom_variant);

	let as_u8_matches = error.variants.iter().enumerate()
		.map(|(i, (variant, _))| quote::quote!(Self::#variant => #i as u8,));

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
					documentation: #scrate::error::DecodeDifferent::Encode(&[ #( #doc, )* ]),
				},
			)
		});

	quote::quote!(
		#item

		impl<#type_impl_gen> #scrate::sp_std::fmt::Debug for #item_ident<#type_use_gen> {
			fn fmt(&self, f: &mut #scrate::sp_std::fmt::Formatter<'_>)
				-> #scrate::sp_std::fmt::Result
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

		impl<#type_impl_static_gen> From<#item_ident<#type_use_gen>>
			for #scrate::sp_runtime::DispatchError
		{
			fn from(err: #item_ident<#type_use_gen>) -> Self {
				let index = <
					<T as frame_system::Trait>::ModuleToIndex
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
	let item = &def.call.item;

	let scrate = &def.scrate();
	let type_impl_gen = &def.type_impl_generics();
	let type_decl_gen = &def.type_decl_generics();
	let type_use_gen = &def.type_use_generics();
	let call_ident = &def.call.call;
	let module_ident = &def.module.item.ident;
	let where_clause = &item.generics.where_clause;

	let fn_ = def.call.methods.iter().map(|method| &method.fn_).collect::<Vec<_>>();

	let fn_weight = def.call.methods.iter().map(|method| &method.weight);

	let fn_doc = def.call.methods.iter().map(|method| &method.docs).collect::<Vec<_>>();

	let args_name = def.call.methods.iter()
		.map(|method| method.args.iter().map(|(_, name, _)| name.clone()).collect::<Vec<_>>())
		.collect::<Vec<_>>();

	let args_type = def.call.methods.iter()
		.map(|method| method.args.iter().map(|(_, _, type_)| type_.clone()).collect::<Vec<_>>())
		.collect::<Vec<_>>();

	let args_compact_attr = def.call.methods.iter().map(|method| {
		method.args.iter()
			.map(|(is_compact, _, _)| {
				if *is_compact {
					quote::quote!( #[codec(compact)] )
				} else {
					quote::quote!()
				}
			})
			.collect::<Vec<_>>()
	});

	let args_metadata_type = def.call.methods.iter().map(|method| {
		method.args.iter()
			.map(|(is_compact, _, type_)| {
				if *is_compact {
					format!("Compact<{:?}>", type_)
				} else {
					format!("{:?}", type_)
				}
			})
			.collect::<Vec<_>>()
	});

	quote::quote!(
		#item // impl dispatchables on `Module`

		#[derive(
			#scrate::codec::Encode,
			#scrate::codec::Decode,
			#scrate::CloneBoundTypes,
			#scrate::EqBoundTypes,
			#scrate::PartialEqBoundTypes,
		)]
		#[cfg_attr(feature = "std", derive(#scrate::DebugBoundTypes))]
		#[cfg_attr(not(feature = "std"), derive(#scrate::DebugStripped))]
		#[allow(non_camel_case_types)]
		pub enum #call_ident<#type_decl_gen> {
			#[doc(hidden)]
			#[codec(skip)]
			__Ignore(
				#scrate::sp_std::marker::PhantomData<(#type_use_gen)>,
				#scrate::Never,
			),
			#( #fn_( #( #args_compact_attr #args_type )* ), )*
		}

		impl<#type_impl_gen> #scrate::dispatch::GetDispatchInfo for #call_ident<#type_use_gen>
			#where_clause
		{
			fn get_dispatch_info(&self) -> #scrate::dispatch::DispatchInfo {
				match *self {
					#(
						Self::#fn_ ( #( ref #args_name, )* ) => {
							let base_weight = #fn_weight;

							let weight = <
								dyn #scrate::dispatch::WeighData<( #( & #args_type, )* )>
							>::weigh_data(&base_weight, ( #( #args_name, )* ));

							let class = <
								dyn #scrate::dispatch::ClassifyDispatch<( #( & #args_type, )* )>
							>::classify_dispatch(&base_weight, ( #( #args_name, )* ));

							let pays_fee = <
								dyn #scrate::dispatch::PaysFee<( #( & #args_type, )* )>
							>::pays_fee(&base_weight, ( #( #args_name, )* ));

							#scrate::dispatch::DispatchInfo {
								weight,
								class,
								pays_fee,
							}
						},
					)*
					Self::__Ignore(_, _) => unreachable!("__Ignore cannot be used"),
				}
			}
		}

		impl<#type_impl_gen> #scrate::dispatch::GetCallName for #call_ident<#type_use_gen>
			#where_clause
		{
			fn get_call_name(&self) -> &'static str {
				match *self {
					#( Self::#fn_(..) => stringify!(#fn_), )*
					Self::__Ignore(_, _) => unreachable!("__PhantomItem cannot be used."),
				}
			}

			fn get_call_names() -> &'static [&'static str] {
				&[ #( stringify!(#fn_), )* ]
			}
		}

		impl<#type_impl_gen> #scrate::traits::UnfilteredDispatchable for #call_ident<#type_use_gen>
			#where_clause
		{
			type Origin = OriginFor<T>;
			fn dispatch_bypass_filter(
				self,
				origin: Self::Origin
			) -> #scrate::dispatch::DispatchResultWithPostInfo {
				match self {
					#(
						Self::#fn_( #( #args_name, )* ) =>
							<#module_ident<#type_use_gen>>::#fn_(origin, #( #args_name, )* )
								.map(Into::into).map_err(Into::into),
					)*
					Self::__Ignore(_, _) => unreachable!("__PhantomItem cannot be used."),
				}
			}
		}

		impl<#type_impl_gen> #scrate::dispatch::Callable<T> for #module_ident<#type_use_gen>
			#where_clause
		{
			type Call = #call_ident<#type_use_gen>;
		}

		impl<#type_impl_gen> #module_ident<#type_use_gen> #where_clause {
			#[doc(hidden)]
			pub fn call_functions() -> &'static [#scrate::dispatch::FunctionMetadata] {
				&[ #(
					#scrate::dispatch::FunctionMetadata {
						name: #scrate::dispatch::DecodeDifferent::Encode(stringify!(#fn_)),
						arguments: #scrate::dispatch::DecodeDifferent::Encode(
							&[ #(
								#scrate::dispatch::FunctionArgumentMetadata {
									name: #scrate::dispatch::DecodeDifferent::Encode(
										stringify!(#args_name)
									),
									ty: #scrate::dispatch::DecodeDifferent::Encode(
										#args_metadata_type
									),
								},
							)* ]
						),
						documentation: #scrate::dispatch::DecodeDifferent::Encode(
							&[ #( #fn_doc ),* ]
						),
					},
				)* ]
			}
		}
	)
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
	item: syn::ItemMod,
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
	pub fn try_from(mut item: syn::ItemMod) -> syn::Result<Self> {
		let item_span = item.span().clone();
		let items = &mut item.content.as_mut()
			.ok_or_else(|| {
				let msg = "Invalid pallet definition, expect mod to be inlined.";
				syn::Error::new(item_span, msg)
			})?.1;

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

		*items = other_items;

		let def = Def {
			item: item,
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
	fn type_impl_generics(&self) -> proc_macro2::TokenStream {
		if self.trait_.has_instance {
			quote::quote!(T: Trait<I>, I: Instance)
		} else {
			quote::quote!(T: Trait)
		}
	}

	/// * either `T: Trait`
	/// * or `T: Trait<I>, I: 'static + Instance`
	fn type_impl_static_generics(&self) -> proc_macro2::TokenStream {
		if self.trait_.has_instance {
			quote::quote!(T: Trait<I>, I: 'static + Instance)
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

#[derive(Clone)]
pub struct InstanceUsage {
	has_instance: bool,
	span: proc_macro2::Span,
}

/// Check the syntax: `I: Instance = DefaultInstance`
///
/// `span` is used in case generics is empty (empty generics has span == call_site).
///
/// return the instance if found.
fn check_trait_def_generics(
	gen: &syn::Generics,
	span: proc_macro2::Span,
) -> syn::Result<()> {
	let expected = "expect `I: Instance = DefaultInstance`";
	pub struct CheckTraitDefGenerics;
	impl syn::parse::Parse for CheckTraitDefGenerics {
		fn parse(input: syn::parse::ParseStream) -> syn::Result<Self> {
			input.parse::<keyword::I>()?;
			input.parse::<syn::Token![:]>()?;
			input.parse::<keyword::Instance>()?;
			input.parse::<syn::Token![=]>()?;
			input.parse::<keyword::DefaultInstance>()?;

			Ok(Self)
		}
	}

	syn::parse2::<CheckTraitDefGenerics>(gen.params.to_token_stream())
		.map_err(|e| {
			let msg = format!("Invalid generics: {}", expected);
			let mut err = syn::Error::new(span, msg);
			err.combine(e);
			err
		})?;

	Ok(())
}

/// Check the syntax:
/// * either `T`
/// * or `T, I = DefaultInstance`
///
/// `span` is used in case generics is empty (empty generics has span == call_site).
///
/// return the instance if found.
fn check_type_def_generics(
	gen: &syn::Generics,
	span: proc_macro2::Span,
) -> syn::Result<InstanceUsage> {
	let expected = "expect `T` or `T, I = DefaultInstance`";
	pub struct CheckTypeDefGenerics(InstanceUsage);
	impl syn::parse::Parse for CheckTypeDefGenerics {
		fn parse(input: syn::parse::ParseStream) -> syn::Result<Self> {
			let mut instance_usage = InstanceUsage {
				has_instance: false,
				span: input.span(),
			};

			input.parse::<keyword::T>()?;
			if input.peek(syn::Token![,]) {
				instance_usage.has_instance = true;
				input.parse::<syn::Token![,]>()?;
				input.parse::<keyword::I>()?;
				input.parse::<syn::Token![=]>()?;
				input.parse::<keyword::DefaultInstance>()?;
			}

			Ok(Self(instance_usage))
		}
	}

	let i = syn::parse2::<CheckTypeDefGenerics>(gen.params.to_token_stream())
		.map_err(|e| {
			let msg = format!("Invalid type def generics: {}", expected);
			let mut err = syn::Error::new(span, msg);
			err.combine(e);
			err
		})?.0;

	Ok(i)
}

/// Check the syntax:
/// * either `` (no generics
/// * or `T`
/// * or `T: Trait`
/// * or `T, I = DefaultInstance`
/// * or `T: Trait<I>, I: Instance = DefaultInstance`
///
/// `span` is used in case generics is empty (empty generics has span == call_site).
///
/// return the instance if found.
fn check_type_def_optional_generics(
	gen: &syn::Generics,
	span: proc_macro2::Span,
) -> syn::Result<Option<InstanceUsage>> {
	let expected = "expect `` or `T` or `T: Trait` or `T, I = DefaultInstance` or \
		`T: Trait<I>, I: Instance = DefaultInstance`";
	pub struct CheckTypeDefOptionalGenerics(Option<InstanceUsage>);
	impl syn::parse::Parse for CheckTypeDefOptionalGenerics {
		fn parse(input: syn::parse::ParseStream) -> syn::Result<Self> {
			if input.is_empty() {
				return Ok(Self(None))
			}

			let mut instance_usage = InstanceUsage {
				span: input.span(),
				has_instance: false,
			};

			input.parse::<keyword::T>()?;

			if input.is_empty() {
				return Ok(Self(Some(instance_usage)))
			}

			let lookahead = input.lookahead1();
			if lookahead.peek(syn::Token![,]) {
				instance_usage.has_instance = true;
				input.parse::<syn::Token![,]>()?;
				input.parse::<keyword::I>()?;
				input.parse::<syn::Token![=]>()?;
				input.parse::<keyword::DefaultInstance>()?;

				Ok(Self(Some(instance_usage)))
			} else if lookahead.peek(syn::Token![:]) {
				input.parse::<syn::Token![:]>()?;
				input.parse::<keyword::Trait>()?;

				if input.is_empty() {
					return Ok(Self(Some(instance_usage)))
				}

				instance_usage.has_instance = true;
				input.parse::<syn::Token![<]>()?;
				input.parse::<keyword::I>()?;
				input.parse::<syn::Token![>]>()?;
				input.parse::<syn::Token![,]>()?;
				input.parse::<keyword::I>()?;
				input.parse::<syn::Token![:]>()?;
				input.parse::<keyword::Instance>()?;
				input.parse::<syn::Token![=]>()?;
				input.parse::<keyword::DefaultInstance>()?;

				Ok(Self(Some(instance_usage)))
			} else {
				Err(lookahead.error())
			}
		}
	}

	let i = syn::parse2::<CheckTypeDefOptionalGenerics>(gen.params.to_token_stream())
		.map_err(|e| {
			let msg = format!("Invalid type def generics: {}", expected);
			let mut err = syn::Error::new(span, msg);
			err.combine(e);
			err
		})?.0;

	Ok(i)
}

/// Check the syntax: `origin: OriginFor<T>`
fn check_dispatchable_first_arg(arg: &syn::FnArg) -> syn::Result<()> {
	let expected = "expect `origin: OriginFor<T>`";

	pub struct CheckDispatchableFirstArg;
	impl syn::parse::Parse for CheckDispatchableFirstArg {
		fn parse(input: syn::parse::ParseStream) -> syn::Result<Self> {
			input.parse::<keyword::origin>()?;
			input.parse::<syn::Token![:]>()?;
			input.parse::<keyword::OriginFor>()?;
			input.parse::<syn::Token![<]>()?;
			input.parse::<keyword::T>()?;
			input.parse::<syn::Token![>]>()?;

			Ok(Self)
		}
	}

	syn::parse2::<CheckDispatchableFirstArg>(arg.to_token_stream())
		.map_err(|e| {
			let msg = format!("Invalid arg: {}", expected);
			let mut err = syn::Error::new(arg.span(), msg);
			err.combine(e);
			err
		})?;

	Ok(())
}

/// Check the syntax:
/// * either `Module<T>`
/// * or `Module<T, I>`
///
/// return the instance if found.
fn check_module_usage(type_: &Box<syn::Type>) -> syn::Result<InstanceUsage> {
	let expected = "expect `Module<T>` or `Module<T, I>`";
	pub struct CheckModuleUseType(InstanceUsage);
	impl syn::parse::Parse for CheckModuleUseType {
		fn parse(input: syn::parse::ParseStream) -> syn::Result<Self> {
			let mut instance_usage = InstanceUsage {
				span: input.span(),
				has_instance: false,
			};

			input.parse::<keyword::Module>()?;
			input.parse::<syn::Token![<]>()?;
			input.parse::<keyword::T>()?;
			if input.peek(syn::Token![,]) {
				instance_usage.has_instance = true;
				input.parse::<syn::Token![,]>()?;
				input.parse::<keyword::I>()?;
			}
			input.parse::<syn::Token![>]>()?;

			Ok(Self(instance_usage))
		}
	}

	let i = syn::parse2::<CheckModuleUseType>(type_.to_token_stream())
		.map_err(|e| {
			let msg = format!("Invalid module type: {}", expected);
			let mut err = syn::Error::new(type_.span(), msg);
			err.combine(e);
			err
		})?.0;

	Ok(i)
}

/// Check the generic is:
/// * either `T: Trait`
/// * or `T: Trait<I>, I: Instance`
///
/// `span` is used in case generics is empty (empty generics has span == call_site).
///
/// return weither it contains instance.
fn check_impl_generics(
	gen: &syn::Generics,
	span: proc_macro2::Span
) -> syn::Result<InstanceUsage> {
	let expected = "expect `T: Trait` or `T: Trait<I>, I: Instance`";
	pub struct CheckImplGenerics(InstanceUsage);
	impl syn::parse::Parse for CheckImplGenerics {
		fn parse(input: syn::parse::ParseStream) -> syn::Result<Self> {
			let mut instance_usage = InstanceUsage {
				span: input.span(),
				has_instance: false,
			};

			input.parse::<keyword::T>()?;
			input.parse::<syn::Token![:]>()?;
			input.parse::<keyword::Trait>()?;
			if input.peek(syn::Token![<]) {
				instance_usage.has_instance = true;
				input.parse::<syn::Token![<]>()?;
				input.parse::<keyword::I>()?;
				input.parse::<syn::Token![>]>()?;
				input.parse::<syn::Token![,]>()?;
				input.parse::<keyword::I>()?;
				input.parse::<syn::Token![:]>()?;
				input.parse::<keyword::Instance>()?;
			}

			Ok(Self(instance_usage))
		}
	}

	let i = syn::parse2::<CheckImplGenerics>(gen.params.to_token_stream())
		.map_err(|e| {
			let mut err = syn::Error::new(span, format!("Invalid generics: {}", expected));
			err.combine(e);
			err
		})?.0;

	Ok(i)
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
