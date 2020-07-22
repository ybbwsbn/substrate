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

mod parse;

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
/// 	pub enum Error<T, I = DefaultInstance> {
/// 		/// doc
/// 		InsufficientProposersBalance,
/// 	}
///
/// 	#[pallet::event] // expand metadata, derive Clone and stuff (without `T: Clone`).
/// 	#[derive_with_correct_bound(Clone, ...)]
/// 	#[pallet::metadata(BalanceOf<T> == Balance)))] // Set the metadata ident for the type
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
/// 	pub enum Origin<T: Trait> {
/// 	}
/// 	Inherent
/// }
/// ```
pub fn pallet(_attr: proc_macro::TokenStream, item: proc_macro::TokenStream) -> proc_macro::TokenStream {
	// TODO TODO: assert attr is empty
	let item = syn::parse_macro_input!(item as syn::ItemMod);
	match pallet_from_item_mod(item) {
		Ok(s) => s.into(),
		Err(e) => e.to_compile_error().into(),
	}
}

pub fn pallet_from_item_mod(item: syn::ItemMod) -> syn::Result<proc_macro2::TokenStream> {
	let def = parse::Def::try_from(item)?;
	parse::def_check(&def)?;
	Ok(parse::expand(&def))
}
