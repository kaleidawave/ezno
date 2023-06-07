/// TSC's implementation is to return any for unknown type
/// This method imitates that and adds a error to the handler
pub fn get_type_handle_errors<U: crate::FSResolver>(
	&mut self,
	reference: &TypeReference,
	checking_data: &mut CheckingData<U>,
) -> TypeId {
	let get_type_result = self.get_type(reference, checking_data);
	if let Ok(ty) = get_type_result {
		ty
	} else {
		checking_data.diagnostics_container.add_error(TypeCheckError::CouldNotFindType(
			reference,
			reference.get_position().into_owned(),
		));
		TypeId::ERROR_TYPE
	}
}

/// Takes [parser::GenericTypeConstraint] and types them
///
/// Also adds item constraints incrementally
///
/// TODO do not like it can take and mutate top level but we move
pub(crate) fn type_generic_type_constraints<T: crate::FSResolver, S: ContextType>(
	generic_constraints: &Vec<GenericTypeConstraint>,
	environment: &mut Context<S>,
	checking_data: &mut CheckingData<T>,
	existing_ids: Option<Vec<TypeId>>,
) -> GenericTypeParameters {
	GenericTypeParameters(
		generic_constraints
			.iter()
			.zip(match existing_ids {
				Some(v) => Left(v.into_iter().map(Some)),
				None => Right(iter::repeat(None)),
			})
			.map(|(generic_constraint, existing_id)| {
				let (name, constraint, default) = match generic_constraint {
					GenericTypeConstraint::Parameter { name, default } => {
						let default = default
							.as_ref()
							.map(|default| environment.get_type_handle_errors(default, checking_data));

						(
							name,
							// Strict ANY
							TypeId::ANY_TYPE,
							default,
						)
					}
					GenericTypeConstraint::Extends(name, ref extends) => {
						(name, environment.get_type_handle_errors(extends, checking_data), None)
					}
					GenericTypeConstraint::ExtendsKeyOf(name, extends_key_of) => todo!(),
					GenericTypeConstraint::Spread { .. } => todo!(),
				};

				// let id = if let Some(existing_id) = existing_id {
				// 	crate::utils::notify!("Existing id?");
				// 	if constraint != TypeId::ANY_TYPE {
				// 		environment.bases.connect_extends(existing_id, constraint);
				// 	}
				// 	environment.named_types.insert(name.clone(), existing_id);
				// 	existing_id
				// } else {
				let poly_nature = PolyNature::Generic {
					name: name.clone(),
					eager_fixed: crate::types::PolyPointer::Fixed(constraint),
				};
				let id = checking_data.types.new_type(Type::RootPolyType(poly_nature));
				environment.named_types.insert(name.clone(), id);
				// };

				GenericTypeParameter { name: name.clone(), id, default }
			})
			.collect(),
	)
}
