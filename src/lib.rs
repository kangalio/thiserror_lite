#![allow(unused)]

macro_rules! internal_2 {
	($i:ident, $t:ty, $e:ident) => {};
	(@ $from:tt $i:ident, $t:ty, $e:ident) => {
		impl From<$t> for $e {
			fn from(x: $t) -> $e {
				todo!()
			}
		}
	};
}

macro_rules! internal_1 {
	// `Enum::Variant`: Empty variant
	($self_:ident, $fmt:ident, $desc:literal, $variant_name:ident) => {
		if let Self::$variant_name = $self_ {
			return write!($fmt, $desc);
		}
	};

	// `Enum::Variant()`: Empty tuple variant
	($self_:ident, $fmt:ident, $desc:literal, $variant_name:ident, ()) => {
		if let Self::$variant_name() = $self_ {
			return write!($fmt, concat!($desc, ""));
		}
	};

	// `Enum::Variant(i32)`: Tuple variant with one field
	($self_:ident, $fmt:ident, $desc:literal, $variant_name:ident, ( $t1:ty $(,)? )) => {
		if let Self::$variant_name(a) = $self_ {
			return write!($fmt, concat!($desc, "{0:.0}"), a);
		}
	};

	// `Enum::Variant(i32, i32)`: Tuple variant with two fields
	($self_:ident, $fmt:ident, $desc:literal, $variant_name:ident, ( $t1:ty, $t2:ty $(,)? )) => {
		if let Self::$variant_name(a, b) = $self_ {
			return write!($fmt, concat!($desc, "{0:.0}{1:.0}"), a, b);
		}
	};

	// `Enum::Variant { f1: i32, f2: i32, ... }`: Struct variant with zero or more fields
	($self_:ident, $fmt:ident, $desc:literal, $variant_name:ident, { $(
		$i:ident: $t:ty
	),* $(,)? }) => {
		if let Self::$variant_name { $($i),* } = $self_ {
			return write!($fmt, concat!($desc, $("{", stringify!($i), ":.0}"),*), $($i = $i),*);
		}
	};
}

macro_rules! internal_2 {
	// `Enum::Variant`, `Enum::Variant()`, or `Enum::Variant {}`
	($e:ident, from, $variant_name:ident, $({})? $(())?) => {
		compile_error!("Can't generate a From impl for variant with no field");
	};

	// `Enum::Variant(i32)`: Tuple variant with one field
	($e:ident, from, $variant_name:ident, ( $t:ty $(,)? )) => {
		impl From<$t> for $e {
			fn from(x: $t) -> $e {
				$e::$variant_name(x)
			}
		}
	};

	// `Enum::Variant(i32, i32, ...)`: Tuple variant with two or more fields
	($e:ident, from, $variant_name:ident, ( $t1:ty, $($t2:ty)+ $(,)? )) => {
		compile_error!("Can't generate a From impl for variant with multiple fields")
	};

	// `Enum::Variant { f1: i32 }`: Struct variant with one field
	($e:ident, from, $variant_name:ident, { $i:ident: $t:ty $(,)? }) => {
		impl From<$t> for $e {
			fn from(x: $t) -> $e {
				$e::$variant_name { $i: x }
			}
		}
	};

	// `Enum::Variant { f1: i32, f2: i32, ... }`: Struct variant with two or more fields
	($e:ident, from, $variant_name:ident, { $i1:ident: $t1:ty, $($i2:ident: $t2:ty),+ $(,)? }) => {
		compile_error!("Can't generate a From impl for variant with multiple fields")
	};

	($e:ident, /* gap */ $variant_name:ident, $($rest:tt)?) => {
		// There is no from attribute in here, so we don't generate any From impl
	};

	($e:ident, $smth_else:ident, $variant_name:ident, $($rest:tt)?) => {
		compile_error!(concat!("Unknown attribute #[", stringify!($smth_else), "]"));
	};
}

macro_rules! err_enum {
	($(#[$error_attribute:meta])? $vis:vis enum $error_type_name:ident { $(
		#[error($desc:literal)] $(#[$from:ident])? $variant_name:ident $(: $variant_spec:tt)?
	),* $(,)? }) => {
		$(#[$error_attribute])?
		pub enum $error_type_name {
			$(
				$variant_name $($variant_spec)?
			),*
		}

		impl std::fmt::Display for $error_type_name {
			fn fmt(&self, formatter: &mut std::fmt::Formatter) -> std::fmt::Result {
				use std::io::Write as _;

				$(
					internal_1!(self, formatter, $desc, $variant_name $(, $variant_spec)?);
				)*

				// Each enum variants should have been covered by an if-let-return
				unreachable!()
			}
		}

		$(
			internal_2!($error_type_name, $($from,)? $variant_name, $($variant_spec)?);
		)*

		impl std::error::Error for $error_type_name {}
	};
}

err_enum! {
	#[derive(Debug, Clone)]
	pub enum Error {
		#[error("This is a simple error")]
		SimpleErr,
		#[error("Some other error {} {}")]
		Tuple1ErrWhat: (i32, u32),
		#[error("Some other error {hello} {world}")]
		BraceThingy: {
			hello: String,
			world: String,
		},
		#[error("See: \"{inner}\"")]
		#[from]
		StringError: {
			inner: String,
		},
	}
}