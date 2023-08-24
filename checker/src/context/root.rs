use super::{Context, ContextId, ContextType};
use crate::{
	types::{FunctionType, TypeId},
	GeneralContext,
};
use source_map::SourceId;
use std::{collections::HashMap, iter::FromIterator};

pub type Root = Context<RootContext>;

#[derive(Debug)]
pub struct RootContext {
	pub operators: Operators,
}

impl ContextType for RootContext {
	fn into_parent_or_root<'a>(et: &'a Context<Self>) -> GeneralContext<'a> {
		GeneralContext::Root(et)
	}

	fn get_parent<'a>(&'a self) -> Option<&'a GeneralContext<'a>> {
		None
	}

	fn is_dynamic_boundary(&self) -> bool {
		false
	}

	fn get_events(&mut self) -> Option<&mut Vec<crate::events::Event>> {
		None
	}
}

#[derive(Default, Debug)]
pub struct Operators {
	pub add: Option<FunctionType>,
	pub sub: Option<FunctionType>,
	pub mul: Option<FunctionType>,
	pub equal: Option<FunctionType>,
}

const HEADER: &[u8] = b"EZNO\0CONTEXT\0FILE";

impl Root {
	/// Merges two [RootEnvironments]. May be used for multiple `.d.ts` files
	pub(crate) fn union(&mut self, other: Self) {
		// TODO this is bad, some things need to merge, inserting over existing will be bad
		self.variables.extend(other.variables.into_iter());
		todo!()
		// self.tys.extend(other.tys.into_iter());
	}

	pub fn new_with_primitive_references() -> Self {
		// TODO number might not be a reference at some point
		let named_types = [
			("number".to_owned(), TypeId::NUMBER_TYPE),
			("string".to_owned(), TypeId::STRING_TYPE),
			("boolean".to_owned(), TypeId::BOOLEAN_TYPE),
			("null".to_owned(), TypeId::NULL_TYPE),
			("undefined".to_owned(), TypeId::UNDEFINED_TYPE),
			// void = undefined everywhere in tsc, not great but preserving compat here
			("void".to_owned(), TypeId::UNDEFINED_TYPE),
			("Array".to_owned(), TypeId::ARRAY_TYPE),
			("Function".to_owned(), TypeId::FUNCTION_TYPE),
			("object".to_owned(), TypeId::OBJECT_TYPE),
		];

		let named_types = HashMap::from_iter(named_types);

		Self {
			context_type: RootContext {
				// Controversial
				operators: Default::default(),
			},
			context_id: ContextId::ROOT,
			named_types,
			variables: Default::default(),
			variable_names: Default::default(),
			variable_current_value: Default::default(),
			deferred_function_constraints: Default::default(),
			bases: Default::default(),
			tasks_to_run: Default::default(),
			properties: Default::default(),
			object_constraints: Default::default(),
			reverse_properties: Default::default(),
			configurable: Default::default(),
			enumerable: Default::default(),
			writable: Default::default(),
			frozen: Default::default(),
			// TODO
			can_use_this: crate::context::CanUseThis::Yeah { this_ty: TypeId::ERROR_TYPE },
			prototypes: Default::default(),
		}
	}

	/// TODO working things out:
	/// - strings could reference a big string
	pub(crate) fn serialize(self) -> Vec<u8> {
		// types
		// subtyping
		// getters & setters
		// variables
		// properties
		// functions
		// jsx tag names
		todo!()

		// let mut buf = HEADER.to_owned();

		// let Self {
		// 	variables,
		// 	named_types,
		// 	types,
		// 	functions_on_type,
		// 	context_type,
		// 	object_count,
		// 	proofs,
		// 	this_bindings,
		// 	modified_constraints: _,
		// 	dependent_dependencies,
		// 	context_id: _,
		// 	deferred_function_constraints: _,
		// 	tasks_to_run: _,
		// 	open_poly_types,
		// 	getters,
		// 	setters,
		// 	variable_names,
		// 	proxies,
		// 	specializations,
		// 	can_use_this,
		// 	subtyping_constant_proofs,
		// 	terms_reverse,
		// 	class_constructors,
		// } = self;

		// buf.extend_from_slice(
		// 	&TryInto::<u16>::try_into(types.len() - TypeId::INTERNAL_TYPE_COUNT)
		// 		.unwrap()
		// 		.to_le_bytes(),
		// );

		// for ty in types.into_iter().skip(TypeId::INTERNAL_TYPE_COUNT) {
		// 	ty.serialize(&mut buf);
		// }

		// variables.serialize(&mut buf);
		// variable_names.serialize(&mut buf);

		// proofs.serialize(&mut buf);
		// functions_on_type.serialize(&mut buf);
		// subtyping_constant_proofs.serialize(&mut buf);
		// terms_reverse.serialize(&mut buf);
		// proxies.serialize(&mut buf);
		// can_use_this.serialize(&mut buf);

		// buf
	}

	pub(crate) fn deserialize(source: Vec<u8>, backing_source: SourceId) -> Result<Self, String> {
		todo!()
		// let mut ctx = Root::new_with_primitive_references();

		// if !source.starts_with(HEADER) {
		// 	return Err("Missing header".to_owned());
		// }

		// let mut bytes = source.into_iter();

		// {
		// 	assert_eq!(bytes.by_ref().take(HEADER.len()).collect::<Vec<_>>(), HEADER);
		// }

		// // Types
		// let count = u16::from_le_bytes([bytes.next().unwrap(), bytes.next().unwrap()]);

		// for _ in 0..count {
		// 	let ty = Type::deserialize(&mut bytes, backing_source);
		// 	ctx.new_type(ty);
		// }
		// crate::utils::notify!("Registered {:?} types", count);

		// ctx.variables = BinarySerializable::deserialize(&mut bytes, backing_source);
		// // TODO terrible
		// VariableId::set_counter_bad((ctx.variables.len() + 1) as u16);
		// ctx.variable_names = BinarySerializable::deserialize(&mut bytes, backing_source);

		// ctx.proofs = BinarySerializable::deserialize(&mut bytes, backing_source);
		// ctx.functions_on_type = BinarySerializable::deserialize(&mut bytes, backing_source);
		// ctx.subtyping_constant_proofs = BinarySerializable::deserialize(&mut bytes, backing_source);
		// ctx.terms_reverse = BinarySerializable::deserialize(&mut bytes, backing_source);
		// ctx.proxies = BinarySerializable::deserialize(&mut bytes, backing_source);
		// ctx.can_use_this = BinarySerializable::deserialize(&mut bytes, backing_source);

		// Ok(ctx)
	}
}
