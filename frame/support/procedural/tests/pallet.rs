// #[frame_support_procedural::pallet]
// mod Pallet {
// 	use frame_support::pallet_prelude::*;

// 	#[pallet::trait_]
// 	trait Trait<I: Instance = DefaultInstance> {
// 		#[pallet::const_]
// 		type Too: Get<u32>;
// 	}

// 	#[pallet::module]
// 	pub struct Module<T, I = DefaultInstance>(core::marker::PhantomData::<(T, I)>);

// 	#[pallet::module_interface]
// 	impl<T: Trait<I>, I: Instance> ModuleInterface for Module<T, I> {
// 	}

// 	#[pallet::call]
// 	impl<T: Trait<I>, I: Instance> Call for Module<T, I> {
// 		#[pallet::weight = 0]
// 		fn toto(origin: OriginFor<T>, #[pallet::compact] toto: u32) -> DispatchResultWithPostInfo {
// 		}
// 	}

// 	#[pallet::error]
// 	pub enum Error {
// 		/// TOTO
// 		toto,
// 		/// TATA
// 		tata
// 	}
// }


use frame_support_procedural::*;

#[derive(CloneBoundTypes, EqBoundTypes, PartialEqBoundTypes)]
struct T {
	a: u32,
	b: u64,
}

#[derive(CloneBoundTypes, EqBoundTypes, PartialEqBoundTypes)]
struct TA(u32, u64);


#[derive(CloneBoundTypes, EqBoundTypes, PartialEqBoundTypes)]
enum Ta {
	a(u32, u64),
	b { a: u32, b: u64},
	c,
	d(u128),
}

