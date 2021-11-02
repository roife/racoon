#![forbid(unsafe_code)]
#![warn(
clippy::pedantic,
missing_copy_implementations,
missing_debug_implementations,
rustdoc::broken_intra_doc_links,
trivial_numeric_casts,
unused_allocation
)]

mod span;
mod intrusive_linkedlist;
pub mod syntax;
pub mod ir;
pub mod irbuilder;
