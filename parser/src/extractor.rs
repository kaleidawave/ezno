use crate::functions::bases::*;

use crate::{FunctionBase, FunctionBased, FunctionId, Visitable};
use derive_enum_from_into::EnumFrom;
use std::collections::HashMap;

pub trait GetFunction<T: FunctionBased + 'static> {
	fn get_function_ref(&self, id: FunctionId<T>) -> Option<&FunctionBase<T>>;

	fn get_function(&mut self, id: FunctionId<T>) -> FunctionBase<T>;

	fn insert_function(&mut self, func: FunctionBase<T>);

	fn new_extracted_function(&mut self, func: FunctionBase<T>) -> ExtractedFunction<T> {
		let id = func.get_function_id();
		let func_pos = crate::ASTNode::get_position(&func).into_owned();
		self.insert_function(func);
		ExtractedFunction(id, func_pos)
	}
}

/// - Creates a `UniversalFunctionId`, a sum type over various [FunctionId]s
/// - Creates a `ExtractedFunctions` struct with hashmaps between different function types
/// - Implements `GetFunction` for different function bases over `ExtractedFunctions`
macro_rules! make_extracted_functions {
	{$($ty:ident),*} => {
		#[derive(Debug, Clone, Copy, Hash, PartialEq, Eq, EnumFrom)]
		pub enum UniversalFunctionId {
			$( $ty(FunctionId<$ty>), )*
		}

		#[derive(Default, Debug)]
		#[allow(non_snake_case)]
		pub struct ExtractedFunctions {
			$(
				$ty : HashMap<FunctionId<$ty>, FunctionBase<$ty>>,
			)*
		}

		$(
			impl GetFunction<$ty> for ExtractedFunctions {
				fn get_function_ref(&self, id: FunctionId<$ty>) -> Option<&FunctionBase<$ty>> {
					self.$ty.get(&id)
				}

				fn get_function(&mut self, id: FunctionId<$ty>) -> FunctionBase<$ty> {
					self.$ty.remove(&id).unwrap()
				}

				fn insert_function(&mut self, func: FunctionBase<$ty>) {
					let id = func.get_function_id();
					self.$ty.insert(id, func);
				}
			}
		)*
	}
}

make_extracted_functions! {
	ArrowFunctionBase,
	ExpressionFunctionBase,
	StatementFunctionBase,
	ObjectLiteralMethodBase,
	ClassConstructorBase,
	ClassFunctionBase
}

/// New type purely for [Visitable] implementation
#[derive(Debug, PartialEq, Eq, Clone)]
pub struct ExtractedFunction<T: FunctionBased>(pub FunctionId<T>, pub source_map::Span);

#[cfg(feature = "self-rust-tokenize")]
impl<T: FunctionBased> self_rust_tokenize::SelfRustTokenize for ExtractedFunction<T> {
	fn append_to_token_stream(
		&self,
		_token_stream: &mut self_rust_tokenize::proc_macro2::TokenStream,
	) {
		todo!("extracted function to tokens")
	}
}

impl ExtractedFunctions {
	pub fn merge(&mut self, other: Self) {
		self.ArrowFunctionBase.extend(other.ArrowFunctionBase.into_iter());
		self.ClassConstructorBase.extend(other.ClassConstructorBase.into_iter());
		self.ClassFunctionBase.extend(other.ClassFunctionBase.into_iter());
		self.ExpressionFunctionBase.extend(other.ExpressionFunctionBase.into_iter());
		self.ObjectLiteralMethodBase.extend(other.ObjectLiteralMethodBase.into_iter());
		self.StatementFunctionBase.extend(other.StatementFunctionBase.into_iter());
	}

	pub fn is_empty(&self) -> bool {
		self.ArrowFunctionBase.is_empty()
			&& self.ClassConstructorBase.is_empty()
			&& self.ClassFunctionBase.is_empty()
			&& self.ExpressionFunctionBase.is_empty()
			&& self.ObjectLiteralMethodBase.is_empty()
			&& self.StatementFunctionBase.is_empty()
	}
}

impl<T: FunctionBased + 'static> Visitable for ExtractedFunction<T>
where
	ExtractedFunctions: GetFunction<T>,
	FunctionBase<T>: Visitable,
{
	fn visit<TData>(
		&self,
		visitors: &mut (impl crate::VisitorReceiver<TData> + ?Sized),
		data: &mut TData,
		settings: &crate::VisitSettings,
		functions: &mut ExtractedFunctions,
		chain: &mut temporary_annex::Annex<crate::Chain>,
	) {
		let function: FunctionBase<T> = functions.get_function(self.0);
		function.visit(visitors, data, settings, functions, chain);
		functions.insert_function(function)
	}

	fn visit_mut<TData>(
		&mut self,
		visitors: &mut (impl crate::VisitorMutReceiver<TData> + ?Sized),
		data: &mut TData,
		settings: &crate::VisitSettings,
		functions: &mut ExtractedFunctions,
		chain: &mut temporary_annex::Annex<crate::Chain>,
	) {
		let mut function: FunctionBase<T> = functions.get_function(self.0);
		function.visit_mut(visitors, data, settings, functions, chain);
		functions.insert_function(function)
	}
}
