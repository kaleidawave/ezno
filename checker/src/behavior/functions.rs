use crate::{
	structures::parameters::SynthesizedParameters,
	types::poly_types::GenericFunctionTypeParameters, CheckingData, Environment, FSResolver,
	TypeId,
};

pub trait SynthesizableFunction {
	fn is_declare(&self) -> bool;

	/// **THIS FUNCTION IS EXPECTED TO PUT THE TYPE PARAMETERS INTO THE ENVIRONMENT WHILE SYNTHESIZING THEM**
	fn type_parameters<T: FSResolver>(
		&self,
		environment: &mut Environment,
		checking_data: &mut CheckingData<T>,
	) -> GenericFunctionTypeParameters;

	/// **THIS FUNCTION IS EXPECTED TO PUT THE PARAMETERS INTO THE ENVIRONMENT WHILE SYNTHESIZING THEM**
	fn parameters<T: FSResolver>(
		&self,
		environment: &mut Environment,
		checking_data: &mut CheckingData<T>,
	) -> SynthesizedParameters;

	fn body<T: FSResolver>(
		&self,
		environment: &mut Environment,
		checking_data: &mut CheckingData<T>,
	);

	fn return_type<T: FSResolver>(
		&self,
		environment: &mut Environment,
		checking_data: &mut CheckingData<T>,
	) -> Option<TypeId>;
}
