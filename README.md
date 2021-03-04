Are you a dependency-conscious library author who is torn between manually implementing all the
error handling boilerplate, and sacrificing compile times (syn, proc-macro2) by using
[thiserror](https://github.com/dtolnay/thiserror)?

Fear no more, because thiserror_lite is an almost drop-in replacement for thiserror with none of
the compilation speed implications and zero dependencies. thiserror_lite builds upon dtolnays's
amazing thiserror crate by reimplementing most functionality with 100% declarative macros.

# Advantages
- much faster compilation time
- identical syntax \[check this later\]
- passes thiserror's full test suite, when [accounting](INSERT LINK) for slight
  [usage differences](INSERT LINK) and [error message differences](INSERT LINK) \[check this later\]

INSERT VIDEO HERE DEMONSTRATING MIGRATION FROM thiserror TO thiserror_lite

ALSO, SHOULD THE NAME BE CHANGED TO thaterror OR SMTH ELSE?

# Caveats
Because of limitations in Rust's declarative macro system (`macro_rules!`), some trade-offs were made

## Different way of accessing variant fields in format string
thiserror requires you to prefix names of fields with a dot:
```rust
#[derive(thiserror::Error, Debug)]
pub enum Error {
	#[error("x^2 = {}", .x * .x)]
	SomeNumber { x: i32 },
}
```
thiserror_lite removes the dot:
```rust
#[derive(thiserror::Error, Debug)]
pub enum Error {
	#[error("x^2 = {}", x * x)]
	SomeNumber { x: i32 },
}
```
If you were using this feature, you will need to adjust your code accordingly if you want to switch
from thiserror to thiserror_lite

## No derive macro
Rust macros can't provide `#\[derive\]` functionality. Hence thiserror_lite replaces the
`#[derive(thiserror::Error)]` concept from thiserror with a wrapper macro:
```rust
thiserror_lite::err_enum! {
	pub enum Error {
		// ...
	}
}
  ```

## No generics or lifetimes in error enums
Due to an implementation detail that I got stuck on, thiserror_lite does not support generics or
lifetimes on the produced enum \[check this later\]

## Max two fields in enum tuple variants
Enum tuple variants with more than two \[check this later\] fields are not supported