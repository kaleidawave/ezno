use crate::types::{
	CovariantContribution, GenericArguments, InformationChain, SliceArguments,
	SubstitutionArguments, TypeId, TypeRestrictions, TypeStore,
};

pub type GenericChain<'a> = Option<GenericChainLink<'a>>;
pub type GenericChainParent<'a> = Option<&'a GenericChainLink<'a>>;

/// - Used for printing and subtyping. Handles nested restrictions
/// - Uses lifetimes because lifetimes
#[derive(Clone, Copy, Debug)]
pub enum GenericChainLink<'a> {
	FunctionRoot {
		call_site_type_arguments: Option<&'a TypeRestrictions>,
		type_arguments: &'a crate::Map<TypeId, TypeId>,
		/// From whatever the function was on
		parent_arguments: Option<&'a GenericArguments>,
	},
	// For walking through [`Type::PartiallyAppliedGenerics`]
	PartiallyAppliedGenericArgumentsLink {
		from: TypeId,
		value: &'a GenericArguments,
		parent_link: GenericChainParent<'a>,
	},
	/// WIP. For the specialised arguments during property key lookup
	MappedPropertyLink { value: &'a SliceArguments, parent_link: GenericChainParent<'a> },
	SpecialGenericChainLink {
		special: SpecialGenericChainLink,
		parent_link: GenericChainParent<'a>,
	},
}

#[derive(Clone, Copy, Debug)]
pub enum SpecialGenericChainLink {
	Exclusive,
	CaseTransform { transform: TypeId },
	CaseInsensitive,
	// Don't think this is necesary?
	// Readonly,
}

impl<'a> GenericChainLink<'a> {
	pub fn get_value(self) -> Option<&'a GenericArguments> {
		if let Self::PartiallyAppliedGenericArgumentsLink { value, .. } = self {
			Some(value)
		} else {
			None
		}
	}

	pub fn get_origin(&self) -> Option<TypeId> {
		match self {
			Self::PartiallyAppliedGenericArgumentsLink { from, .. } => Some(*from),
			Self::FunctionRoot { .. } => None,
			Self::SpecialGenericChainLink { parent_link, .. }
			| Self::MappedPropertyLink { parent_link, .. } => parent_link.and_then(Self::get_origin),
		}
	}

	// TODO lifetimes here
	// TODO make iterator
	// fn get_parent(&self) -> GenericChainParent {
	// 	if let Self::PartiallyAppliedGenericArgumentsLink { parent_link, .. }
	// 	| Self::MappedPropertyLink { parent_link, .. }
	// 	| Self::CaseTransform { parent_link, .. }
	// 	| Self::Exclusive { parent_link, .. } = self
	// 	{
	// 		parent_link
	// 	} else {
	// 		None
	// 	}
	// }

	pub(crate) fn get_string_transform(&self) -> Option<TypeId> {
		match self {
			GenericChainLink::FunctionRoot { .. } => None,
			GenericChainLink::SpecialGenericChainLink {
				parent_link,
				special: SpecialGenericChainLink::CaseTransform { transform },
			} => Some(*transform),
			GenericChainLink::SpecialGenericChainLink { parent_link, special: _ }
			| GenericChainLink::PartiallyAppliedGenericArgumentsLink { parent_link, .. }
			| GenericChainLink::MappedPropertyLink { parent_link, .. } => {
				if let Some(parent_link) = parent_link {
					parent_link.get_string_transform()
				} else {
					None
				}
			}
		}
	}

	pub(crate) fn is_case_insensitive(&self) -> bool {
		match self {
			GenericChainLink::FunctionRoot { .. } => false,
			GenericChainLink::SpecialGenericChainLink {
				parent_link,
				special: SpecialGenericChainLink::CaseInsensitive,
			} => true,
			GenericChainLink::SpecialGenericChainLink { parent_link, special: _ }
			| GenericChainLink::PartiallyAppliedGenericArgumentsLink { parent_link, .. }
			| GenericChainLink::MappedPropertyLink { parent_link, .. } => {
				if let Some(parent_link) = parent_link {
					parent_link.is_case_insensitive()
				} else {
					false
				}
			}
		}
	}

	pub(crate) fn exclusive_mode(&self) -> bool {
		match self {
			GenericChainLink::FunctionRoot { .. } => false,
			GenericChainLink::SpecialGenericChainLink {
				parent_link,
				special: SpecialGenericChainLink::Exclusive,
			} => true,
			GenericChainLink::SpecialGenericChainLink { parent_link, special: _ }
			| GenericChainLink::PartiallyAppliedGenericArgumentsLink { parent_link, .. }
			| GenericChainLink::MappedPropertyLink { parent_link, .. } => {
				if let Some(parent_link) = parent_link {
					parent_link.exclusive_mode()
				} else {
					false
				}
			}
		}
	}

	// /// TODO wip
	// #[allow(unused)]
	// pub(crate) fn get_argument(
	// 	&self,
	// 	on: TypeId,
	// 	info: &impl InformationChain,
	// 	types: &TypeStore,
	// ) -> Option<Vec<TypeId>> {
	// 	match self {
	// 		GenericChainLink::PartiallyAppliedGenericArgumentsLink {
	// 			parent_link,
	// 			value,
	// 			from: _,
	// 		} => value
	// 			.get_argument_as_list(on, info, types)
	// 			.or_else(|| parent_link.and_then(|parent| parent.get_argument(on, info, types))),
	// 		GenericChainLink::FunctionRoot {
	// 			parent_arguments,
	// 			call_site_type_arguments,
	// 			type_arguments,
	// 		} => parent_arguments
	// 			.and_then(|parent| parent.get_argument_as_list(on, info, types))
	// 			.or_else(|| {
	// 				call_site_type_arguments.and_then(|ta1| ta1.get(&on).map(|(arg, _)| vec![*arg]))
	// 			})
	// 			.or_else(|| type_arguments.get(&on).map(|a| vec![*a])),
	// 		GenericChainLink::MappedPropertyLink { .. } => {
	// 			crate::utilities::notify!("TODO temp");
	// 			self.get_single_argument(on).map(|v| vec![v])
	// 		}
	// 		GenericChainLink::SpecialGenericChainLink { parent_link, special: _ } => {
	// 			parent_link.and_then(|f| f.get_argument(on, info, types))
	// 		}
	// 	}
	// }

	/// WIP I want to make this the main one
	pub(crate) fn get_argument_covariant(
		&self,
		on: TypeId,
		info: &impl InformationChain,
		types: &TypeStore,
	) -> Option<CovariantContribution> {
		match self {
			GenericChainLink::PartiallyAppliedGenericArgumentsLink {
				parent_link,
				value,
				from: _,
			} => value
				.get_structure_restriction(on)
				.map(CovariantContribution::TypeId)
				.or_else(|| parent_link.and_then(|f| f.get_argument_covariant(on, info, types))),
			GenericChainLink::FunctionRoot {
				parent_arguments,
				call_site_type_arguments,
				type_arguments,
			} => call_site_type_arguments
				.and_then(|parent| {
					parent.get(&on).map(|(arg, _)| *arg).map(CovariantContribution::TypeId)
				})
				.or_else(|| {
					call_site_type_arguments
						.and_then(|ta1| ta1.get(&on).map(|(arg, _)| *arg))
						.map(CovariantContribution::TypeId)
				})
				.or_else(|| type_arguments.get(&on).copied().map(CovariantContribution::TypeId)),
			GenericChainLink::MappedPropertyLink { parent_link, value } => value
				.get(&on)
				.map(|(arg, _)| arg.clone())
				.or_else(|| parent_link.and_then(|f| f.get_argument_covariant(on, info, types))),
			GenericChainLink::SpecialGenericChainLink { parent_link, special: _ } => {
				parent_link.and_then(|f| f.get_argument_covariant(on, info, types))
			}
		}
	}

	/// TODO WIP
	///
	/// between this and `extend_arguments` there needs to be something better
	pub(crate) fn into_substitutable(
		&self,
		types: &mut TypeStore,
	) -> SubstitutionArguments<'static> {
		match self {
			GenericChainLink::PartiallyAppliedGenericArgumentsLink {
				from: _,
				parent_link,
				value,
			} => {
				let mut args = value.into_substitutable();
				if let Some(parent_link) = parent_link {
					parent_link.extend_arguments(&mut args);
				}
				args
			}
			GenericChainLink::MappedPropertyLink { parent_link, value } => {
				crate::utilities::notify!("parent_link={:?}", parent_link);
				let arguments = value
					.iter()
					.map(|(k, (contribution, _))| (*k, contribution.clone().into_type(types)))
					.collect();
				let mut args =
					SubstitutionArguments { parent: None, arguments, closures: Default::default() };
				if let Some(parent_link) = parent_link {
					parent_link.extend_arguments(&mut args);
				}
				args
			}
			GenericChainLink::FunctionRoot { .. } => {
				todo!()
			}
			GenericChainLink::SpecialGenericChainLink { parent_link, special: _ } => {
				todo!("{:?}", parent_link.is_some())
			}
		}
	}

	pub(crate) fn append_to_link(
		from: TypeId,
		parent: GenericChainParent<'a>,
		value: &'a GenericArguments,
	) -> GenericChainLink<'a> {
		GenericChainLink::PartiallyAppliedGenericArgumentsLink { parent_link: parent, value, from }
	}

	#[allow(clippy::unnecessary_wraps)]
	pub(crate) fn append(
		from: TypeId,
		parent: GenericChainParent<'a>,
		value: &'a GenericArguments,
	) -> GenericChain<'a> {
		Some(GenericChainLink::append_to_link(from, parent, value))
	}

	/// Does not do 'lookup generics'. Which may be fine
	///
	/// - (swaps `get_argument_as_list` with `get_structure_restriction`)
	pub(crate) fn get_single_argument(&self, on: TypeId) -> Option<TypeId> {
		match self {
			GenericChainLink::PartiallyAppliedGenericArgumentsLink {
				parent_link,
				value,
				from: _,
			} => value
				.get_structure_restriction(on)
				.or_else(|| parent_link.and_then(|parent| parent.get_single_argument(on))),
			GenericChainLink::FunctionRoot {
				parent_arguments,
				call_site_type_arguments,
				type_arguments,
			} => parent_arguments
				.and_then(|parent| parent.get_structure_restriction(on))
				.or_else(|| {
					call_site_type_arguments.and_then(|ta1| ta1.get(&on).map(|(arg, _)| *arg))
				})
				.or_else(|| type_arguments.get(&on).copied()),
			GenericChainLink::MappedPropertyLink { parent_link, value } => value
				.get(&on)
				.cloned()
				.and_then(|(c, _)| {
					if let CovariantContribution::TypeId(t) = c {
						Some(t)
					} else {
						crate::utilities::notify!(
							"WARNING SKIPPING AS Contribution is {:?} (NOT TYPEID)",
							c
						);
						None
					}
				})
				.or_else(|| parent_link.and_then(|parent| parent.get_single_argument(on))),
			GenericChainLink::SpecialGenericChainLink { parent_link, special: _ } => {
				parent_link.and_then(|parent| parent.get_single_argument(on))
			}
		}
	}

	/// For building up [`SubstitutionArguments`] from a generic chain
	pub(crate) fn extend_arguments(&self, arguments: &mut SubstitutionArguments<'static>) {
		match self {
			GenericChainLink::PartiallyAppliedGenericArgumentsLink {
				from: _,
				parent_link,
				value,
			} => {
				match value {
					GenericArguments::ExplicitRestrictions(n) => {
						arguments.arguments.extend(n.iter().map(|(k, v)| {
							assert!(*k != v.0);
							(*k, v.0)
						}));
					}
					GenericArguments::Closure(n) => arguments.closures.extend(n.iter().copied()),
					GenericArguments::LookUp { .. } => todo!(),
				}
				if let Some(parent_link) = parent_link {
					parent_link.extend_arguments(arguments);
				}
			}
			GenericChainLink::FunctionRoot {
				parent_arguments,
				call_site_type_arguments: _,
				type_arguments,
			} => {
				arguments.arguments.extend(type_arguments.iter().map(|(k, v)| (*k, *v)));
				if let Some(value) = parent_arguments {
					match value {
						GenericArguments::ExplicitRestrictions(n) => {
							arguments.arguments.extend(n.iter().map(|(k, v)| (*k, v.0)));
						}
						GenericArguments::Closure(n) => {
							arguments.closures.extend(n.iter().copied());
						}
						GenericArguments::LookUp { .. } => todo!(),
					}
				}
			}

			GenericChainLink::MappedPropertyLink { .. } => todo!("need &mut TypeStore"),
			GenericChainLink::SpecialGenericChainLink { parent_link, special: _ } => {
				todo!("parent_link={:?}", parent_link)
			}
		}
	}
}
