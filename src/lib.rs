#![allow(unused)]

#[doc(hidden)]
#[macro_export]
macro_rules! _internal_1 {
	// `Enum::Variant`: Empty variant
	($self_:ident, $fmt:ident, $desc:literal, $variant_name:ident) => {
		if let Self::$variant_name = $self_ {
			return write!($fmt, $desc);
		}
	};

	// `Enum::Variant()`: Empty tuple variant
	($self_:ident, $fmt:ident, $desc:literal, $variant_name:ident ()) => {
		if let Self::$variant_name() = $self_ {
			return write!($fmt, concat!($desc, ""));
		}
	};

	// `Enum::Variant(i32)`: Tuple variant with one field
	($self_:ident, $fmt:ident, $desc:literal, $variant_name:ident ( $t1:ty )) => {
		if let Self::$variant_name(a) = $self_ {
			return write!($fmt, concat!($desc, "{0:.0}"), a);
		}
	};

	// `Enum::Variant(i32, i32)`: Tuple variant with two fields
	($self_:ident, $fmt:ident, $desc:literal, $variant_name:ident ( $t1:ty, $t2:ty )) => {
		if let Self::$variant_name(a, b) = $self_ {
			return write!($fmt, concat!($desc, "{0:.0}{1:.0}"), a, b);
		}
	};

	// `Enum::Variant { f1: i32, f2: i32, ... }`: Struct variant with zero or more fields
	($self_:ident, $fmt:ident, $desc:literal, $variant_name:ident { $(
		$i:ident: $t:ty
	),* }) => {
		if let Self::$variant_name { $($i),* } = $self_ {
			return write!($fmt, concat!($desc, $("{", stringify!($i), ":.0}"),*), $($i = $i),*);
		}
	};

	($self_:ident, $fmt:ident, transparent, $variant_name:ident ( $t:ty )) => {
		if let Self::$variant_name(a) = $self_ {
			return write!($fmt, "{}", a); // is this correct?
		}
	};

	($self_:ident, $fmt:ident, transparent, $variant_name:ident { $f:ident: $t:ty }) => {
		if let Self::$variant_name { $f } = $self_ {
			return write!($fmt, "{}", $f); // is this correct?
		}
	};

	($self_:ident, $fmt:ident, transparent, $variant_name:ident
		$(())?
		$({})?
		$(($tt1:ty, $(tt2:ty),+))?
		$(($sf1:ident: $st1:ty, $($sf2:ident: $st2:ty),+))?
	) => {
		compile_error!("Exactly one field must be present in order to use #[error(transparent)]");
	}
}

#[doc(hidden)]
#[macro_export]
macro_rules! _internal_2 {
	($e:ident, $variant_name:ident (#[from] $t:ty)) => {
		impl From<$t> for $e {
			fn from(x: $t) -> $e {
				$e::$variant_name(x)
			}
		}
	};

	($e:ident, $variant_name:ident { #[from] $field:ident: $type_:ty}) => {
		impl From<$type_> for $e {
			fn from(x: $type_) -> $e {
				$e::$variant_name { $field: x }
			}
		}
	};

	($e:ident, $variant_name:ident $(($($tt:ty),*))? $({$($sf:ident: $st:ty),*})?) => {
		// This is just a regular unit, tuple, or struct variant without any #[from] fields
	};

	// This matches any unit, tuple, or struct variant with or without an arbitrary amount of
	// #[from] fields. However, any valid usages of #[from] will have been cought by the above macro
	// rules, so this rule only kicks in if usage was invalid
	($e:ident, $variant_name:ident $(($($(#[from])? $tt:ty),*))? $({$($(#[from])? $sf:ident: $st:ty),*})?) => {
		compile_error!("Can't use #[from] in variants with multiple fields");
	};
}

#[macro_export]
macro_rules! err_enum {
    // remember that $vis matches even nothing. No need to enclose in $()? or anything like that
	($(#[$error_attribute:meta])* $vis:vis enum $error_type_name:ident { $(
		#[error($desc:tt)]
		$variant_name:ident
		$({$(
			$(#[$sfrom:ident])? $sf:ident: $st:ty
		),* $(,)?})?
		$(($(
			$(#[$tfrom:ident])? $tt:ty
		),* $(,)?))?
	),* $(,)? }) => {
		$(#[$error_attribute])*
        $vis enum $error_type_name {
            $(
                $variant_name $({$($sf: $st),*})? $(($($tt),*))?
            ),*
        }

		impl std::fmt::Display for $error_type_name {
			fn fmt(&self, formatter: &mut std::fmt::Formatter) -> std::fmt::Result {
				$(
					$crate::_internal_1!(self, formatter, $desc, $variant_name $({$($sf: $st),*})? $(($($tt),*))?);
				)*

				// Each enum variant is covered by an if-let-return
				unreachable!()
			}
		}

		$(
			$crate::_internal_2!(
				$error_type_name,
				$variant_name
				$({$(
					$(#[$sfrom])? $sf: $st
				),*})?
				$(($(
					$(#[$tfrom])? $tt
				),*))?
			);
		)*

		impl std::error::Error for $error_type_name {}
	};
}

// err_enum! {
// 	#[derive(Debug, Clone)]
// 	pub enum Error {
// 		#[error("This is a simple error")]
// 		SimpleErr,
// 		#[error("This is a simple tuple error")]
// 		SimpleErr2(),
// 		#[error("This is a simple struct error")]
// 		SimpleErr3 {},
// 		#[error("Some other error {} {}")]
// 		Tuple1ErrWhat(i32, u32),
// 		#[error("Some other error {hello}. We're not printing world here >:)")]
// 		BraceThingy {
// 			hello: String,
// 			world: String,
// 		},
// 		#[error("See: \"{inner}\"")]
// 		StringError {
//             #[from] inner: String,
// 		},
// 		#[error("yeah. gotta use ehhhh, u32 cuz String is already taken")]
// 		StringErrorTuple(#[from] u32),
// 		#[error(transparent)]
// 		ForwardError(#[from] std::num::ParseIntError),
// 		#[error(transparent)]
// 		ForwardError2 { inner: std::num::ParseIntError },
// 	}
// }