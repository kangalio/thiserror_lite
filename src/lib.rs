// #![no_std]
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

	// `Enum::Variant(i32, i32, i32)`: Tuple variant with three fields
	($self_:ident, $fmt:ident, $desc:literal, $variant_name:ident ( $t1:ty, $t2:ty, $t3:ty )) => {
		if let Self::$variant_name(a, b, c) = $self_ {
			return write!($fmt, concat!($desc, "{0:.0}{1:.0}{2:.0}"), a, b, c);
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

	($e:ident, $variant_name:ident $(($(
		// Note that this matches source, because even though these fields may not have #[from],
		// they still may have #[source]
		$(#[source])? $tt:ty
	),*))? $({$(
		$(#[source])? $sf:ident: $st:ty
	),*})?) => {
		// This is just a regular unit, tuple, or struct variant without any #[from] fields
		// So we're not generating anything
	};

	// This matches any tuple variant with or without an arbitrary amount of
	// #[from] fields. However, any valid usages of #[from] will have been cought by the above macro
	// rules, so this rule only kicks in if usage was invalid
	($e:ident, $variant_name:ident ( $( $(#[from])? $tt:ty ),* ) ) => {
		compile_error!("Can't use #[from] in variants with multiple fields");
	};

	// Like above, but for struct variants instead of tuple variants
	($e:ident, $variant_name:ident { $( $(#[from])? $sf:ident: $st:ty ),* } ) => {
		compile_error!("Can't use #[from] in variants with multiple fields");
	};
}

#[doc(hidden)]
#[macro_export]
macro_rules! _internal_3 {
	($self_:ident, $variant_name:ident (
		#[$source_or_from:meta] $t1:ty
		$( , $t_rest:ty )*
	) ) => {
		if let Self::$variant_name(x, ..) = $self_ {
			return Some(x as _);
		}
	};

	($self_:ident, $variant_name:ident (
		$t1:ty,
		#[$source_or_from:meta] $t2:ty
		$( , $t_rest:ty )*
	) ) => {
		if let Self::$variant_name(_, x, ..) = $self_ {
			return Some(x as _);
		}
	};

	($self_:ident, $variant_name:ident (
		$t1:ty,
		$t2:ty,
		#[$source_or_from:meta] $t3:ty
		$( , $t_rest:ty )*
	) ) => {
		if let Self::$variant_name(_, _, x, ..) = $self_ {
			return Some(x as _);
		}
	};

	($self_:ident, $variant_name:ident {
		$( $f_pre:ident: $t_pre:ty, )*
		#[$source_or_from:meta] $f:ident: $t:ty
		$( , $f_post:ident: $t_post:ty )*
	} ) => {
		if let Self::$variant_name { $f, .. } = $self_ {
			return Some($f as _);
		}
	};

	// Matches tuple variant with two or more #[source]/#[from]
	($self_:ident, $variant_name:ident (
		$( $t1:ty, )*
		#[$source_or_from_1:meta] $t2:ty,
		$( $t3:ty, )*
		#[$source_or_from_2:meta] $t4:ty
		$( , $t5:ty )*
	) ) => {
		compile_error!("Can't have multiple #[source] or #[from] in a single variant");
	};

	// Matches struct variants with two or more #[source]/#[from]
	($self_:ident, $variant_name:ident {
		$( $f1:ident: $t1:ty, )*
		#[$source_or_from_1:meta] $f2:ident: $t2:ty,
		$( $f3:ident: $t3:ty, )*
		#[$source_or_from_2:meta] $f4:ident: $t4:ty
		$( , $f5:ident: $t5:ty )*
	} ) => {
		compile_error!("Can't have multiple #[source] or #[from] in a single variant");
	};

	($self_:ident, $variant_name:ident $(($(
		$tt:ty
	),*))? $({$(
		$sf:ident: $st:ty
	),*})?) => {
		// This is just a regular unit, tuple, or struct variant without any #[source] fields
		// So we're not generating anything
	};
}

macro_rules! check_that_its_from_or_source {
	(from) => {};
	(source) => {};
	($smth_else:meta) => {
		compile_error!(concat!(
			"Unknown attribute \"",
			stringify!($smth_else),
			"\""
		));
	};
}

#[macro_export]
macro_rules! err_enum {
    // remember that $vis matches even nothing. No need to enclose in $()? or anything like that
	($(#[$error_attribute:meta])* $vis:vis enum $error_type_name:ident { $(
		#[error($desc:tt)]
		$variant_name:ident
		$({$(
			$(#[$sattr:ident])? $sf:ident: $st:ty
		),* $(,)?})?
		$(($(
			$(#[$tattr:ident])? $tt:ty
		),* $(,)?))?
	),* $(,)? }) => {
		$(#[$error_attribute])*
        $vis enum $error_type_name {
            $(
                $variant_name $({$($sf: $st),*})? $(($($tt),*))?
            ),*
		}

		// check that all the attributes are either #[from] or #[source]. This assumption later
		// allows the internal macros to skip handling the unknown attribute error case
		$( // for each variant
			$( // if it's a struct variant
				$( // for each variant field
					$( // if field has attribute
						check_that_its_from_or_source!($sattr);
					)?
				)*
			)?
		)*

		impl core::fmt::Display for $error_type_name {
			fn fmt(&self, formatter: &mut core::fmt::Formatter) -> core::fmt::Result {
				#![allow(irrefutable_let_patterns)] // happens when there's just one variant

				$( // for each variant
					$crate::_internal_1!(self, formatter, $desc, $variant_name $({$($sf: $st),*})? $(($($tt),*))?);
				)*

				// Each enum variant is covered by an if-let-return
				unreachable!()
			}
		}

		$( // for each variant
			$crate::_internal_2!(
				$error_type_name,
				$variant_name
				$({$(
					$(#[$sattr])? $sf: $st
				),*})?
				$(($(
					$(#[$tattr])? $tt
				),*))?
			);
		)*

		impl std::error::Error for $error_type_name {
			fn source(&self) -> Option<&(dyn std::error::Error + 'static)> {
				#![allow(irrefutable_let_patterns)] // happens when there's just one variant

				$( // for each variant
					$crate::_internal_3!(
						self,
						$variant_name
						$( // if it's a struct variant
							{
							$( // for each variant field
								$( #[$sattr] )?
								$sf: $st
							),*
							}
						)?
						$( // if it's a tuple variant
							(
							$( // for each variant field
								$( #[$tattr] )?
								$tt
							),*
							)
						)?
					);
				)*

				// If this variant doesn't have a #[source] or #[from], we fall through to here
				None
			}
		}
	};
}

#[doc(hidden)]
#[macro_export]
macro_rules! dbg {
	($($t:tt)*) => {
		compile_error!(stringify!($($t)*));
	}
}

#[cfg(test)]
#[test]
fn test_main() {
	err_enum! {
		#[derive(Debug, Clone)]
		pub enum Error {
			#[error("This is a simple error")]
			SimpleErr,
			#[error("This is a simple tuple error")]
			SimpleErr2(),
			#[error("This is a simple struct error")]
			SimpleErr3 {},
			#[error("Some other error {} {}")]
			Tuple1ErrWhat(i32, u32),
			#[error("Some other error {hello}. We're not printing world here >:)")]
			BraceThingy {
				hello: String,
				world: std::num::ParseIntError,
			},
			#[error("Some other error {0}. We're not printing world here >:)")]
			BraceThingyTuple(String, #[source] std::num::ParseIntError),
			#[error("See: \"{inner}\"")]
			StringError {
				#[from] inner: std::num::ParseIntError,
			},
			#[error("yeah. gotta use ehhhh, u32 cuz String is already taken")]
			StringErrorTuple(#[from] std::num::ParseFloatError ),
			#[error(transparent)]
			ForwardError(#[from] std::num::TryFromIntError ),
			#[error(transparent)]
			ForwardError2 { inner: std::num::ParseIntError },
		}
	}
}

// TODO: rename "sf" -> "struct field"; "tt" -> "tuple type"; etc.
// TODO: #[error(transparent)] for #[source]
// TODO: support error structs
// TODO: add custom expressions behind error format string
