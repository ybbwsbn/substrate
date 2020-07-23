mod trait_;
mod module;
mod call;
mod error;
mod event;

use crate::pallet::Def;
use quote::ToTokens;
use frame_support_procedural_tools::{generate_hidden_includes};

/// Expand definition, in particular:
/// * add some bounds and variants to type defined,
/// * create some new types,
/// * impl stuff on them.
pub fn expand(mut def: Def) -> proc_macro2::TokenStream {
	let trait_ = trait_::expand_trait_(&mut def);
	let module = module::expand_module(&mut def);
	let call = call::expand_call(&mut def);
	let error = error::expand_error(&mut def);
	let event = event::expand_event(&mut def);

	let scrate_decl = generate_hidden_includes(&def.hidden_crate_name(), "frame-support");

	let new_items = quote::quote!(
		#scrate_decl

		#trait_
		#module
		#call
		#error
		#event
	);

	def.item.content.as_mut().expect("This is checked by parsing").1
		.push(syn::Item::Verbatim(new_items));

	def.item.into_token_stream()
}
