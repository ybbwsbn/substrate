#[frame_support::pallet]
mod pallet {
	type OriginFor<T> = <T as frame_system::Trait>::Origin;

	use frame_support::traits::Get;
	use frame_support::dispatch::DispatchResultWithPostInfo;
	use sp_inherents::ProvideInherent;
	use sp_inherents::InherentData;
	use sp_inherents::InherentIdentifier;
	pub trait ModuleInterface {}
	pub trait Instance {}
	pub struct DefaultInstance;
	impl Instance for DefaultInstance {}

	#[pallet::trait_]
	pub trait Trait<I: Instance = DefaultInstance>: frame_system::Trait {
		#[pallet::const_]
		type Too: Get<u32>;
		type Balance;
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

	#[pallet::error]
	pub enum Error<T, I = DefaultInstance> {
		/// E
		/// E
		E,
		///
		B,
	}

	#[pallet::event]
	pub enum Event<T: Trait<I>, I: Instance = DefaultInstance> {
		/// A
		A(T::Balance, T::Balance, u32),
		/// B
		/// B2
		B { aa: u32, bb: T::Balance },
	}

	#[pallet::origin]
	pub struct Origin<T, I = DefaultInstance>(core::marker::PhantomData<(T, I)>);

	#[pallet::inherent]
	impl<T: Trait<I>, I: Instance> ProvideInherent for Module<T, I> {
		type Call = Call<T, I>;
		type Error = super::InherentError;

		const INHERENT_IDENTIFIER: InherentIdentifier = super::INHERENT_IDENTIFIER;

		fn create_inherent(_data: &InherentData) -> Option<Self::Call> {
			unimplemented!();
		}
	}
}

#[derive(codec::Encode, sp_runtime::RuntimeDebug)]
#[cfg_attr(feature = "std", derive(codec::Decode))]
pub enum InherentError {
}

impl sp_inherents::IsFatalError for InherentError {
	fn is_fatal_error(&self) -> bool {
		unimplemented!();
	}
}

pub const INHERENT_IDENTIFIER: sp_inherents::InherentIdentifier = *b"testpall";
