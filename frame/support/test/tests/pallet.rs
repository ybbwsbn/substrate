#[frame_support::pallet]
mod Pallet {
	type OriginFor<T> = <T as frame_system::Trait>::Origin;

	use frame_support::traits::Get;
	use frame_support::dispatch::DispatchResultWithPostInfo;
	pub trait ModuleInterface {}
	pub trait Instance {}
	pub struct DefaultInstance;
	impl Instance for DefaultInstance {}

	#[pallet::trait_]
	pub trait Trait<I: Instance = DefaultInstance>: frame_system::Trait {
		#[pallet::const_]
		type Too: Get<u32>;
	}

	#[pallet::module]
	pub struct Module<T, I = DefaultInstance>(core::marker::PhantomData::<(T, I)>);

	#[pallet::module_interface]
	impl<T: Trait<I>, I: Instance> ModuleInterface for Module<T, I> {
	}

	#[pallet::call]
	impl<T: Trait<I>, I: Instance> Call for Module<T, I> {
		#[pallet::weight = 0]
		fn toto(origin: OriginFor<T>, #[pallet::compact] toto: u32) -> DispatchResultWithPostInfo {
			let _ = origin;
			let _ = toto;
			Ok(().into())
		}
	}
}
