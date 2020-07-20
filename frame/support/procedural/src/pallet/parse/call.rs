use super::{CheckImplGenerics, CheckModuleUseType, CheckDispatchableFirstArg, take_item_attrs};
use quote::ToTokens;
use syn::spanned::Spanned;

/// List of additional token to be used for parsing.
mod keyword {
	syn::custom_keyword!(DispatchResultWithPostInfo);
	syn::custom_keyword!(Call);
	syn::custom_keyword!(weight);
	syn::custom_keyword!(compact);
}

pub struct CallDef {
	pub has_instance: bool,
	pub impl_: syn::ItemImpl,
	pub methods: Vec<CallVariantDef>,
	pub call: keyword::Call,
}

pub struct CallVariantDef {
	// has_instance: bool, ??
	fn_: syn::Ident,
	args: Vec<(bool, Box<syn::Type>)>,
	weight: syn::Expr,
}

impl CallDef {
	pub fn try_from(item: syn::Item) -> syn::Result<Self> {
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

