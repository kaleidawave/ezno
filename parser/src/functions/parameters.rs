use std::fmt::Debug;

use crate::{
	derive_ASTNode, ASTNode, Expression, ParseError, ParseErrors, ParseResult, TypeAnnotation,
	VariableField, WithComment,
};

use derive_partial_eq_extras::PartialEqExtras;
use iterator_endiate::EndiateIteratorExt;
use source_map::Span;
use visitable_derive::Visitable;

#[apply(derive_ASTNode)]
#[derive(Debug, Clone, PartialEq, Visitable, get_field_by_type::GetFieldByType)]
#[get_field_by_type_target(Span)]
pub struct Parameter<V> {
	#[visit_skip_field]
	pub visibility: V,
	pub name: WithComment<VariableField>,
	pub type_annotation: Option<TypeAnnotation>,
	pub additionally: Option<ParameterData>,
	pub position: Span,
}

pub trait ParameterVisibility: Send + Sync + Sized + Debug + PartialEq + Clone + 'static {
	fn from_reader(reader: &mut crate::Lexer) -> Self;
}

impl ParameterVisibility for () {
	fn from_reader(_reader: &mut crate::Lexer) -> Self {}
}

impl ParameterVisibility for Option<crate::types::Visibility> {
	fn from_reader(reader: &mut crate::Lexer) -> Option<crate::types::Visibility> {
		if let Some(Some(keyword)) = reader
			.get_options()
			.type_annotations
			.then(|| reader.is_one_of_keywords_advance(&["private", "public", "protected"]))
		{
			Some(match keyword {
				"private" => crate::types::Visibility::Private,
				"public" => crate::types::Visibility::Public,
				"protected" => crate::types::Visibility::Protected,
				_ => unreachable!(),
			})
		} else {
			None
		}
	}
}

#[derive(Debug, Clone, PartialEq, Visitable)]
#[apply(derive_ASTNode)]
pub enum ParameterData {
	Optional,
	WithDefaultValue(Box<Expression>),
}

#[cfg(feature = "extras")]
#[cfg_attr(target_family = "wasm", tsify::declare)]
pub type SpreadParameterName = VariableField;

#[cfg(not(feature = "extras"))]
#[cfg_attr(target_family = "wasm", tsify::declare)]
pub type SpreadParameterName = crate::VariableIdentifier;

#[apply(derive_ASTNode)]
#[derive(Debug, Clone, PartialEq, Visitable)]
pub struct SpreadParameter {
	pub name: SpreadParameterName,
	pub type_annotation: Option<TypeAnnotation>,
	pub position: Span,
}

#[apply(derive_ASTNode)]
#[derive(Debug, Clone, PartialEqExtras, Visitable)]
#[partial_eq_ignore_types(Span)]
pub struct FunctionParameters<L, V> {
	#[visit_skip_field]
	pub leading: L,
	pub parameters: Vec<Parameter<V>>,
	pub rest_parameter: Option<Box<SpreadParameter>>,
	pub position: Span,
}

pub trait LeadingParameter: Send + Sync + Sized + Debug + PartialEq + Clone + 'static {
	fn try_make(
		this_annotation: Option<ThisParameter>,
		super_annotation: Option<SuperParameter>,
	) -> ParseResult<Self>;

	fn get_this_parameter(&self) -> Option<&ThisParameter>;
	fn get_super_parameter(&self) -> Option<&SuperParameter>;
}

#[apply(derive_ASTNode)]
#[derive(Debug, Clone, PartialEqExtras, Visitable)]
#[partial_eq_ignore_types(Span)]
pub struct ThisParameter {
	pub constraint: TypeAnnotation,
	pub position: Span,
}

/// TODO WIP!
#[apply(derive_ASTNode)]
#[derive(Debug, Clone, PartialEqExtras, Visitable)]
#[partial_eq_ignore_types(Span)]
pub struct SuperParameter {
	pub constraint: TypeAnnotation,
	pub position: Span,
}

impl LeadingParameter for () {
	fn try_make(
		this_annotation: Option<ThisParameter>,
		super_annotation: Option<SuperParameter>,
	) -> ParseResult<Self> {
		if this_annotation.is_some() || super_annotation.is_some() {
			let position =
				this_annotation.map_or(super_annotation.unwrap().position, |a| a.position);

			Err(ParseError::new(ParseErrors::CannotUseLeadingParameterHere, position))
		} else {
			Ok(())
		}
	}

	fn get_this_parameter(&self) -> Option<&ThisParameter> {
		None
	}
	fn get_super_parameter(&self) -> Option<&SuperParameter> {
		None
	}
}

impl LeadingParameter for Option<ThisParameter> {
	fn try_make(
		this_annotation: Option<ThisParameter>,
		super_annotation: Option<SuperParameter>,
	) -> ParseResult<Self> {
		if let Some(s) = super_annotation {
			Err(ParseError::new(ParseErrors::CannotUseLeadingParameterHere, s.position))
		} else {
			Ok(this_annotation)
		}
	}

	fn get_this_parameter(&self) -> Option<&ThisParameter> {
		self.as_ref()
	}
	fn get_super_parameter(&self) -> Option<&SuperParameter> {
		None
	}
}

impl LeadingParameter for (Option<ThisParameter>, Option<SuperParameter>) {
	fn try_make(
		this_annotation: Option<ThisParameter>,
		super_annotation: Option<SuperParameter>,
	) -> ParseResult<Self> {
		Ok((this_annotation, super_annotation))
	}

	fn get_this_parameter(&self) -> Option<&ThisParameter> {
		self.0.as_ref()
	}
	fn get_super_parameter(&self) -> Option<&SuperParameter> {
		self.1.as_ref()
	}
}

impl<L, V> ASTNode for FunctionParameters<L, V>
where
	L: LeadingParameter,
	V: ParameterVisibility,
{
	fn get_position(&self) -> Span {
		self.position
	}

	fn from_reader(reader: &mut crate::Lexer) -> ParseResult<Self> {
		let start = reader.expect_start('(')?;
		let mut parameters = Vec::new();

		let mut this_type = None::<ThisParameter>;
		let mut super_type = None::<SuperParameter>;
		let mut rest_parameter = None;

		// TODO want no owned version
		let mut names: Vec<String> = Vec::new();

		loop {
			reader.skip();
			let s = reader.after_comment_literals();
			if s.starts_with(")") {
				reader.skip_including_comments();
				break;
			}

			let start = reader.get_start();

			if reader.is_operator_advance("...") {
				let name = SpreadParameterName::from_reader(reader)?;
				let name_position = name.get_position();

				if !reader.get_options().skip_validation {
					let mut duplicate = None;
					#[cfg(feature = "extras")]
					{
						name.visit_names(&mut |name| {
							if duplicate.is_none() {
								duplicate = names
									.iter()
									.any(|existing| existing == name)
									.then_some(name.to_owned());
							}
							names.push(name.to_owned());
						});
					}

					#[cfg(not(feature = "extras"))]
					{
						duplicate = names
							.iter()
							.any(|existing| name == &**existing)
							.then_some(name.clone());
					}

					if let Some(_duplicate) = duplicate {
						return Err(ParseError::new(
							crate::ParseErrors::DuplicateParameterName,
							name_position,
						));
					}
				}

				let type_annotation = if reader.is_operator_advance(":") {
					Some(TypeAnnotation::from_reader(reader)?)
				} else {
					None
				};

				let position = start
					.union(type_annotation.as_ref().map_or(name_position, ASTNode::get_position));

				rest_parameter =
					Some(Box::new(SpreadParameter { name, type_annotation, position }));
				break;
			} else if parameters.is_empty() && reader.is_keyword_advance("this") {
				// Some(Token(_, start)) = reader.conditional_next(|tok| {
				// options.type_annotations
				// 	&& reader.expect(TSXToken::Colon)?;
				reader.expect(':')?;
				let constraint = TypeAnnotation::from_reader(reader)?;
				let position = start.union(constraint.get_position());
				this_type = Some(ThisParameter { constraint, position });
			} else if parameters.is_empty() && reader.is_keyword_advance("super") {
				reader.expect(':')?;
				// reader.expect(TSXToken::Colon)?;
				let constraint = TypeAnnotation::from_reader(reader)?;
				let position = start.union(constraint.get_position());
				super_type = Some(SuperParameter { constraint, position });
			} else {
				let visibility = V::from_reader(reader);

				let name = WithComment::<VariableField>::from_reader(reader)?;

				let (is_optional, type_annotation) = if reader.is_operator_advance("?:") {
					let type_annotation = TypeAnnotation::from_reader(reader)?;
					(true, Some(type_annotation))
				} else if reader.is_operator_advance(":") {
					let type_annotation = TypeAnnotation::from_reader(reader)?;
					(false, Some(type_annotation))
				} else if reader.is_operator_advance("?") {
					(true, None)
				} else {
					(false, None)
				};

				let value = if reader.is_operator_advance("=") {
					Some(Box::new(Expression::from_reader(reader)?))
				} else {
					None
				};

				let additionally = match (is_optional, value) {
					(true, Some(value)) => {
						return Err(ParseError::new(
							crate::ParseErrors::FunctionParameterOptionalAndDefaultValue,
							value.get_position(),
						));
					}
					// =
					(false, Some(value)) => Some(ParameterData::WithDefaultValue(value)),
					// ?:
					(true, None) => Some(ParameterData::Optional),
					(false, None) => None,
				};

				let end_position = if let Some(ParameterData::WithDefaultValue(e)) = &additionally {
					e.get_position()
				} else if let Some(ref type_annotation) = type_annotation {
					type_annotation.get_position()
				} else {
					name.get_position()
				};

				let position = name.get_position().union(end_position);

				if !reader.get_options().skip_validation {
					let mut duplicate = None;
					name.get_ast_ref().visit_names(&mut |name| {
						if duplicate.is_none() {
							duplicate = names
								.iter()
								.any(|existing| existing == name)
								.then_some(name.to_owned());
						}
						names.push(name.into());
					});
					if let Some(_duplicate) = duplicate {
						return Err(ParseError::new(
							crate::ParseErrors::DuplicateParameterName,
							position,
						));
					}
				}

				parameters.push(Parameter {
					visibility,
					name,
					type_annotation,
					additionally,
					position,
				});
			}

			if !reader.is_operator_advance(",") {
				break;
			}
		}
		let close = reader.expect(')')?;
		let leading = L::try_make(this_type, super_type)?;
		let position = start.union(close);
		Ok(FunctionParameters { leading, parameters, rest_parameter, position })
	}

	fn to_string_from_buffer<T: source_map::ToString>(
		&self,
		buf: &mut T,
		options: &crate::ToStringOptions,
		local: crate::LocalToStringInformation,
	) {
		let FunctionParameters { parameters, rest_parameter, .. } = self;
		let mut large = false;
		if options.enforce_limit_length_limit() && local.should_try_pretty_print {
			let room = options.max_line_length as usize;
			let mut buf = source_map::StringWithOptionalSourceMap {
				source: String::new(),
				source_map: None,
				quit_after: Some(room),
				since_new_line: 0,
			};
			// Not particularly accurate but does sort of work
			for parameter in parameters {
				parameter.name.to_string_from_buffer(&mut buf, options, local);
				let type_annotation = parameter.type_annotation.as_ref();
				type_annotation.inspect(|v| v.to_string_from_buffer(&mut buf, options, local));
				if let Some(ParameterData::WithDefaultValue(ref value)) = parameter.additionally {
					value.to_string_from_buffer(&mut buf, options, local);
				}
				large = buf.source.len() > room;
				if large {
					break;
				}
			}
			if let Some(rest_parameter) = rest_parameter {
				rest_parameter.name.to_string_from_buffer(&mut buf, options, local);
				let type_annotation = rest_parameter.type_annotation.as_ref();
				type_annotation.inspect(|v| v.to_string_from_buffer(&mut buf, options, local));
				large = buf.source.len() > room;
			}
		}

		let inner_local = if large { local.next_level() } else { local };

		buf.push('(');
		// let local = if large { local.next_level() } else { local };
		for (at_end, Parameter { name, type_annotation, additionally, .. }) in
			parameters.iter().endiate()
		{
			if large {
				buf.push_new_line();
				options.add_indent(inner_local.depth, buf);
			}
			// decorators_to_string_from_buffer(decorators, buf, options, inner_local);
			name.to_string_from_buffer(buf, options, inner_local);
			if let (true, Some(ref type_annotation)) =
				(options.include_type_annotations, type_annotation)
			{
				if let Some(ParameterData::Optional) = additionally {
					buf.push('?');
				}
				buf.push_str(": ");
				type_annotation.to_string_from_buffer(buf, options, inner_local);
			}
			if let Some(ParameterData::WithDefaultValue(value)) = additionally {
				buf.push_str(if options.pretty { " = " } else { "=" });
				value.to_string_from_buffer(buf, options, inner_local);
			}
			if !at_end || rest_parameter.is_some() {
				buf.push(',');
				options.push_gap_optionally(buf);
			}
		}
		if let Some(rest_parameter) = rest_parameter {
			if large {
				buf.push_new_line();
				options.add_indent(inner_local.depth, buf);
			}
			buf.push_str("...");
			rest_parameter.name.to_string_from_buffer(buf, options, inner_local);
			if let Some(ref type_annotation) = rest_parameter.type_annotation {
				buf.push_str(": ");
				type_annotation.to_string_from_buffer(buf, options, inner_local);
			}
		}
		if large {
			buf.push_new_line();
			options.add_indent(local.depth, buf);
		}
		buf.push(')');
	}
}
