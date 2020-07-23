use crate::pallet::Def;

/// * Add derive Eq, PartialEq, Debug and Clone on Module
pub fn expand_module(def: &mut Def) -> proc_macro2::TokenStream {
	let scrate = &def.scrate();

	let module_item = {
		let item = &mut def.item.content.as_mut().expect("Checked by def").1[def.module.index];
		if let syn::Item::Struct(item) = item {
			item
		} else {
			unreachable!("Checked by module parser")
		}
	};

	module_item.attrs.push(syn::parse_quote!(
		#[derive(
			#scrate::CloneBoundTypes,
			#scrate::EqBoundTypes,
			#scrate::PartialEqBoundTypes,
			#scrate::DebugStripped,
		)]
	));

	Default::default()
}
