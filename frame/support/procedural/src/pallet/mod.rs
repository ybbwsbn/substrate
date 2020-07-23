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
	Ok(parse::expand(def))
}
