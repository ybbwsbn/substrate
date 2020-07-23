use crate::pallet::Def;

/// * Add __Ignore variant on Event
/// * Add derive for codec, eq, partialeq, clone, debug on Event
/// * Impl `From<Event>` for ()
/// * Impl metadata function on Event
pub fn expand_event(def: &mut Def) -> proc_macro2::TokenStream {
	let event = if let Some(event) = &def.event {
		event
	} else {
		return Default::default()
	};

	let event_ident = &event.event;
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

	let event_item = {
		let item = &mut def.item.content.as_mut().expect("Checked by def parser").1[event.index];
		if let syn::Item::Enum(item) = item {
			item
		} else {
			unreachable!("Checked by event parser")
		}
	};

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
		event_item.variants.push(variant);
	}

	event_item.attrs.push(syn::parse_quote!(
		#[derive(
			#scrate::codec::Encode,
			#scrate::codec::Decode,
			#scrate::CloneBoundTypes,
			#scrate::EqBoundTypes,
			#scrate::PartialEqBoundTypes,
		)]
	));

	event_item.attrs.push(syn::parse_quote!(
		#[cfg_attr(feature = "std", derive(#scrate::DebugBoundTypes))]
	));

	event_item.attrs.push(syn::parse_quote!(
		#[cfg_attr(not(feature = "std"), derive(#scrate::DebugStripped))]
	));

	quote::quote!(
		impl<#event_impl_gen> From<#event_ident<#event_use_gen>> for () {
			fn from(_: #event_ident<#event_use_gen>) -> () { () }
		}

		impl<#event_impl_gen> #event_ident<#event_use_gen> {
			#[allow(dead_code)]
			#[doc(hidden)]
			pub fn metadata() -> &'static [#scrate::event::EventMetadata] {
				&[ #( #metadata )* ]
			}
		}
	)
}

