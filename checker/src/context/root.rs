use super::{facts::Facts, ClosedOverReferencesInScope, Context, ContextId, ContextType};
use crate::{
	structures::modules::Exported, types::TypeId, CheckingData, Environment, GeneralContext,
	SynthesisedModule,
};
use source_map::SourceId;
use std::{collections::HashMap, iter::FromIterator};

pub type RootContext = Context<Root>;

#[derive(Debug)]
pub struct Root;

impl ContextType for Root {
	fn as_general_context(et: &Context<Self>) -> GeneralContext<'_> {
		GeneralContext::Root(et)
	}

	fn get_parent(&self) -> Option<&GeneralContext<'_>> {
		None
	}

	fn is_dynamic_boundary(&self) -> bool {
		false
	}

	fn get_closed_over_references(&mut self) -> Option<&mut ClosedOverReferencesInScope> {
		None
	}

	fn get_exports(&mut self) -> Option<&mut Exported> {
		None
	}
}

const HEADER: &[u8] = b"EZNO\0CONTEXT\0FILE";

impl RootContext {
	/// Merges two [RootEnvironments]. May be used for multiple `.d.ts` files
	pub(crate) fn union(&mut self, other: Self) {
		// TODO this is bad, some things need to merge, inserting over existing will be bad
		self.variables.extend(other.variables);
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
			context_type: Root,
			context_id: ContextId::ROOT,
			named_types,
			variables: Default::default(),
			variable_names: Default::default(),
			deferred_function_constraints: Default::default(),
			bases: Default::default(),
			object_constraints: Default::default(),
			// TODO
			can_use_this: crate::context::CanUseThis::Yeah { this_ty: TypeId::ERROR_TYPE },
			facts: Default::default(),
		}
	}

	pub fn new_module_context<'a, T: crate::ReadFromFS, M: crate::ASTImplementation>(
		&self,
		source: SourceId,
		module: M::Module,
		checking_data: &'a mut CheckingData<T, M>,
	) -> &'a SynthesisedModule<M::Module> {
		let mut environment = self.new_lexical_environment(crate::Scope::Module {
			source,
			exported: Exported::default(),
		});
		M::synthesize_module(&module, source, &mut environment, checking_data);

		let crate::Scope::Module { exported, .. } = environment.context_type.kind else {
			unreachable!()
		};
		let module = SynthesisedModule { content: module, exported, facts: environment.facts };

		// TODO better way to do this?
		checking_data.modules.synthesised_modules.insert(source, module);
		checking_data.modules.synthesised_modules.get(&source).unwrap()
	}

	/// TODO working things out:
	/// - strings could reference a big string
	pub(crate) fn serialize(self) -> Vec<u8> {
		todo!()
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
