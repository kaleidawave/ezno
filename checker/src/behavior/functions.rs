use source_map::Span;

use crate::{
	structures::parameters::SynthesizedParameters, types::poly_types::GenericTypeParameters,
	CheckingData, Environment, FSResolver, TypeId,
};

pub trait SynthesizableFunction {
	fn is_declare(&self) -> bool;

	fn is_async(&self) -> bool;

	fn is_generator(&self) -> bool;

	/// **THIS FUNCTION IS EXPECTED TO PUT THE TYPE PARAMETERS INTO THE ENVIRONMENT WHILE SYNTHESIZING THEM**
	fn type_parameters<T: FSResolver>(
		&self,
		environment: &mut Environment,
		checking_data: &mut CheckingData<T>,
	) -> Option<GenericTypeParameters>;

	/// **THIS FUNCTION IS EXPECTED TO PUT THE PARAMETERS INTO THE ENVIRONMENT WHILE SYNTHESIZING THEM**
	fn parameters<T: FSResolver>(
		&self,
		environment: &mut Environment,
		checking_data: &mut CheckingData<T>,
	) -> SynthesizedParameters;

	/// Returned type is extracted from events, thus doesn't expect anything in return
	fn body<T: FSResolver>(
		&self,
		environment: &mut Environment,
		checking_data: &mut CheckingData<T>,
	);

	fn return_type_annotation<T: FSResolver>(
		&self,
		environment: &mut Environment,
		checking_data: &mut CheckingData<T>,
	) -> Option<(TypeId, Span)>;
}
