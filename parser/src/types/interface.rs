use crate::{
	bracketed_items_from_reader, bracketed_items_to_string, derive_ASTNode,
	extensions::decorators::Decorated, functions::MethodHeader, property_key::PublicOrPrivate,
	types::type_annotations::TypeAnnotationFunctionParameters, ASTNode,
	ExpressionOrStatementPosition, ParseErrors, ParseOptions, ParseResult, PropertyKey, Span,
	StatementPosition, TypeAnnotation, TypeParameter, WithComment,
};

use get_field_by_type::GetFieldByType;
use iterator_endiate::EndiateIteratorExt;

#[apply(derive_ASTNode)]
#[derive(Debug, Clone, PartialEq, get_field_by_type::GetFieldByType)]
#[get_field_by_type_target(Span)]
pub struct InterfaceDeclaration {
	pub is_is_declare: bool,
	pub name: StatementPosition,
	// #[cfg(feature = "extras")]
	// pub is_nominal: bool,
	pub type_parameters: Option<Vec<TypeParameter>>,
	/// The document interface extends a multiple of other interfaces
	pub extends: Option<Vec<TypeAnnotation>>,
	pub members: Vec<WithComment<Decorated<InterfaceMember>>>,
	pub position: Span,
}

#[derive(Debug, Clone, PartialEq, Eq)]
#[apply(derive_ASTNode)]
pub enum Optionality {
	Default,
	Optional,
	// Will make existing optional fields required, whereas default does not change status
	Required,
}

impl ASTNode for InterfaceDeclaration {
	fn get_position(&self) -> Span {
		self.position
	}

	fn from_reader(reader: &mut crate::Lexer) -> ParseResult<Self> {
		let start = reader.expect_keyword("interface")?;

		// #[cfg(feature = "extras")]
		// let is_nominal = reader
		// 	.conditional_next(|t| matches!(t, TSXToken::Keyword(TSXKeyword::Nominal)))
		// 	.is_some();

		let name = StatementPosition::from_reader(reader)?;
		let type_parameters = if reader.is_operator_advance("<") {
			let (params, _) = crate::bracketed_items_from_reader(reader, ">")?;
			Some(params)
		} else {
			None
		};

		let extends = if reader.is_keyword_advance("extends") {
			let type_annotation = TypeAnnotation::from_reader(reader)?;
			let mut extends = vec![type_annotation];
			while reader.is_operator_advance(",") {
				extends.push(TypeAnnotation::from_reader(reader)?);
			}
			Some(extends)
		} else {
			None
		};

		let _ = reader.expect('{')?;
		let members = interface_members_from_reader(reader)?;
		let position = start.union(reader.expect('}')?);
		Ok(InterfaceDeclaration {
			name,
			is_is_declare: false,
			// #[cfg(feature = "extras")]
			// is_nominal,
			type_parameters,
			extends,
			members,
			position,
		})
	}

	fn to_string_from_buffer<T: source_map::ToString>(
		&self,
		buf: &mut T,
		options: &crate::ToStringOptions,
		local: crate::LocalToStringInformation,
	) {
		if options.include_type_annotations {
			if self.name.is_declare() {
				buf.push_str("declare ");
			}
			buf.push_str("interface ");
			self.name.identifier.to_string_from_buffer(buf, options, local);
			if let Some(type_parameters) = &self.type_parameters {
				bracketed_items_to_string(type_parameters, ('<', '>'), buf, options, local);
				options.push_gap_optionally(buf);
			}
			if let Some(extends) = &self.extends {
				buf.push_str(" extends ");
				for (at_end, extends) in extends.iter().endiate() {
					extends.to_string_from_buffer(buf, options, local);
					if !at_end {
						buf.push(',');
						options.push_gap_optionally(buf);
					}
				}
			}
			options.push_gap_optionally(buf);
			buf.push('{');
			if options.pretty && !self.members.is_empty() {
				buf.push_new_line();
			}
			for member in &self.members {
				options.add_indent(local.depth + 1, buf);
				member.to_string_from_buffer(buf, options, local.next_level());
				if options.pretty {
					buf.push_new_line();
				}
			}
			options.add_indent(local.depth, buf);
			buf.push('}');
		}
	}
}

/// For some reason mapped types can have a negated a readonly keyword
#[apply(derive_ASTNode)]
#[derive(Debug, Clone, Copy, PartialEq)]
pub enum MappedReadonlyKind {
	Negated,
	Always,
	False,
}

/// This is also used for [`TypeAnnotation::ObjectLiteral`]
#[apply(derive_ASTNode)]
#[derive(Debug, Clone, PartialEq, GetFieldByType)]
#[get_field_by_type_target(Span)]
pub enum InterfaceMember {
	Method {
		header: MethodHeader,
		name: PropertyKey<PublicOrPrivate>,
		type_parameters: Option<Vec<TypeParameter>>,
		parameters: TypeAnnotationFunctionParameters,
		return_type: Option<TypeAnnotation>,
		is_optional: bool,
		position: Span,
	},
	Property {
		name: PropertyKey<PublicOrPrivate>,
		type_annotation: TypeAnnotation,
		is_readonly: bool,
		/// Marked with `?:`
		is_optional: bool,
		position: Span,
	},
	Indexer {
		name: String,
		indexer_type: TypeAnnotation,
		return_type: TypeAnnotation,
		is_readonly: bool,
		position: Span,
	},
	/// Example
	/// ```ts
	/// new (...params: any[]): HTMLElement
	/// ```
	Constructor {
		type_parameters: Option<Vec<TypeParameter>>,
		parameters: TypeAnnotationFunctionParameters,
		return_type: Option<TypeAnnotation>,
		is_readonly: bool,
		position: Span,
	},
	Caller {
		type_parameters: Option<Vec<TypeParameter>>,
		parameters: TypeAnnotationFunctionParameters,
		return_type: Option<TypeAnnotation>,
		is_readonly: bool,
		position: Span,
	},
	/// [For mapped types](https://www.typescriptlang.org/docs/handbook/2/mapped-types.html)
	Rule {
		parameter: String,
		matching_type: Box<TypeAnnotation>,
		as_type: Option<Box<TypeAnnotation>>,
		optionality: Optionality,
		is_readonly: MappedReadonlyKind,
		output_type: Box<TypeAnnotation>,
		position: Span,
	},
	Comment(String, bool, Span),
}

#[allow(clippy::similar_names)]
impl ASTNode for InterfaceMember {
	fn get_position(&self) -> Span {
		*GetFieldByType::get(self)
	}

	fn from_reader(reader: &mut crate::Lexer) -> ParseResult<Self> {
		let start = reader.get_start();
		let is_readonly = reader.is_keyword_advance("readonly");

		// This match will early return if not a method
		if reader.is_operator("(") {
			// Calling self
			let parameters = TypeAnnotationFunctionParameters::from_reader(reader)?;
			// let parameters = function_parameters_from_reader(reader)?;
			let return_type = if reader.is_operator_advance(":") {
				Some(TypeAnnotation::from_reader(reader)?)
			} else {
				None
			};
			// TODO parameter.pos can be union'ed with itself
			let position = start
				.union(return_type.as_ref().map_or(parameters.position, ASTNode::get_position));
			Ok(InterfaceMember::Caller {
				is_readonly,
				position,
				parameters,
				return_type,
				type_parameters: None,
			})
		} else if reader.is_operator("<") {
			// Caller self with generic parameters
			let (type_parameters, _) = bracketed_items_from_reader(reader, ">")?;
			let parameters = TypeAnnotationFunctionParameters::from_reader(reader)?;
			let return_type = if reader.is_operator_advance(":") {
				Some(TypeAnnotation::from_reader(reader)?)
			} else {
				None
			};
			// TODO union start
			let position = return_type.as_ref().map_or(parameters.position, ASTNode::get_position);

			Ok(InterfaceMember::Caller {
				is_readonly,
				position,
				parameters,
				type_parameters: Some(type_parameters),
				return_type,
			})
		} else if reader.is_keyword_advance("new") {
			// Constructor
			let type_parameters = reader
				.is_operator_advance("<")
				.then(|| bracketed_items_from_reader(reader, ">"))
				.transpose()?
				.map(|(tp, _)| tp);

			let parameters = TypeAnnotationFunctionParameters::from_reader(reader)?;

			let return_type = if reader.is_operator_advance(":") {
				Some(TypeAnnotation::from_reader(reader)?)
			} else {
				None
			};

			let end = return_type.as_ref().map_or(parameters.position, ASTNode::get_position);

			let position = start.union(end);

			Ok(InterfaceMember::Constructor {
				is_readonly,
				position,
				parameters,
				type_parameters,
				return_type,
			})
		} else if reader.is_operator_advance("-") {
			// Little bit weird, but prevents a lot of duplication
			let inner = Self::from_reader(reader)?;
			if let Self::Rule {
				parameter,
				matching_type,
				as_type,
				optionality,
				is_readonly: MappedReadonlyKind::Always,
				output_type,
				position,
			} = inner
			{
				Ok(Self::Rule {
					parameter,
					matching_type,
					as_type,
					optionality,
					is_readonly: MappedReadonlyKind::Negated,
					output_type,
					position,
				})
			} else {
				Err(crate::ParseError::new(ParseErrors::ExpectedRule, start.with_length(1)))
			}
		} else if let Some(comment_prefix) = reader.is_one_of(&["//", "/*"]) {
			let start = reader.get_start();
			let is_multiline = comment_prefix == "/*";
			reader.advance(2);
			let content = reader.parse_comment_literal(is_multiline)?.to_owned();
			let position = start.union(reader.get_end());
			Ok(InterfaceMember::Comment(content, is_multiline, position))
		} else {
			let header = MethodHeader::from_reader(reader);

			let name = if reader.is_operator_advance("[") {
				if reader.starts_with_string_delimeter() {
					let (content, quoted) = reader.parse_string_literal()?;
					let position = start.with_length(content.len() + 2);
					PropertyKey::StringLiteral(content.to_owned(), quoted, position)
				} else if reader.starts_with_number() {
					let (value, length) = reader.parse_number_literal()?;
					let position = start.with_length(length as usize);
					PropertyKey::NumberLiteral(value, position)
				} else {
					use crate::Expression;
					// "name" is the name of the parameter name for indexing
					let start = reader.get_start();
					let name = reader.parse_identifier("interface parameter name", false)?;

					// Catch for computed symbol: e.g. `[Symbol.instanceOf()]`, rather than indexer
					if reader.is_operator(".") {
						let top = Expression::VariableReference(
							name.into(),
							start.with_length(name.len()),
						);
						let expression =
							Expression::from_reader_after_first_expression(reader, 0, top)?;
						let end = reader.expect(']')?;
						PropertyKey::Computed(Box::new(expression), start.union(end))
					} else if reader.is_operator_advance(":") && header.is_no_modifiers() {
						// Indexed type
						let indexer_type = TypeAnnotation::from_reader(reader)?;
						reader.expect(']')?;
						reader.expect(':')?;
						let return_type = TypeAnnotation::from_reader(reader)?;
						return Ok(InterfaceMember::Indexer {
							name: name.to_owned(),
							is_readonly,
							indexer_type,
							position: start.union(return_type.get_position()),
							return_type,
						});
					} else if reader.is_keyword_advance("in") && header.is_no_modifiers() {
						// For mapped types
						let matching_type = TypeAnnotation::from_reader(reader)?;

						let as_type = if reader.is_keyword_advance("as") {
							Some(Box::new(TypeAnnotation::from_reader(reader)?))
						} else {
							None
						};

						reader.expect(']')?;
						let optionality = if reader.is_operator_advance("?:") {
							Optionality::Optional
						} else if reader.is_operator_advance("-?:") {
							Optionality::Required
						} else if reader.is_operator_advance(":") {
							Optionality::Default
						} else {
							return Err(crate::lexer::utilities::expected_one_of_items(
								reader,
								&["?:", "-?:", ":"],
							));
						};

						let output_type = TypeAnnotation::from_reader(reader)?;

						let position = start.union(output_type.get_position());

						let is_readonly = if is_readonly {
							MappedReadonlyKind::Always
						} else {
							MappedReadonlyKind::False
						};

						return Ok(InterfaceMember::Rule {
							parameter: name.to_owned(),
							optionality,
							is_readonly,
							matching_type: Box::new(matching_type),
							output_type: Box::new(output_type),
							position,
							as_type,
						});
					} else {
						return Err(if header.is_no_modifiers() {
							crate::lexer::utilities::expected_one_of_items(
								reader,
								&[".", ":", "in"],
							)
						} else {
							crate::lexer::utilities::expected_one_of_items(reader, &["."])
						});
					}
				}
			} else {
				let start = reader.get_start();
				let name = reader.parse_identifier("interface parameter name", false)?;
				// TODO...?
				let privacy = PublicOrPrivate::Public;
				PropertyKey::Identifier(name.to_owned(), start.with_length(name.len()), privacy)
			};

			let type_parameters = reader
				.is_operator_advance("<")
				.then(|| bracketed_items_from_reader(reader, ">"))
				.transpose()?
				.map(|(tp, _)| tp);

			if !header.is_no_modifiers() || reader.is_operator("(") || reader.is_operator("?(") {
				let is_optional = reader.is_operator_advance("?");
				// This will eat the first parenthesis, thus not eating above
				let parameters = TypeAnnotationFunctionParameters::from_reader(reader)?;
				let mut position = start.union(parameters.position);
				let return_type = if reader.is_operator_advance(":") {
					let type_annotation = TypeAnnotation::from_reader(reader)?;
					position = position.union(type_annotation.get_position());
					Some(type_annotation)
				} else {
					None
				};

				Ok(InterfaceMember::Method {
					header,
					name,
					parameters,
					type_parameters,
					return_type,
					is_optional,
					position,
				})
			} else if let Some(seperator) = reader.is_one_of_operators(&["?:", ":"]) {
				// if let Some(header) = header {
				// 	Err(crate::ParseError::new(ParseErrors::UnexpectedHeader, header.get_position()))
				// } else {
				let is_optional = "?:" == seperator;
				reader.advance(if is_optional { 2 } else { 1 });
				let type_annotation = TypeAnnotation::from_reader(reader)?;
				let position = start.union(type_annotation.get_position());
				Ok(InterfaceMember::Property {
					position,
					name,
					type_annotation,
					is_optional,
					is_readonly,
				})
				// }
			} else {
				Err(crate::lexer::utilities::expected_one_of_items(reader, &["(", "?", ":", "?:"]))
			}
		}
	}

	fn to_string_from_buffer<T: source_map::ToString>(
		&self,
		buf: &mut T,
		options: &crate::ToStringOptions,
		local: crate::LocalToStringInformation,
	) {
		match self {
			InterfaceMember::Property { name, type_annotation, is_readonly, .. } => {
				if *is_readonly {
					buf.push_str("readonly ");
				}
				name.to_string_from_buffer(buf, options, local);
				buf.push(':');
				options.push_gap_optionally(buf);
				type_annotation.to_string_from_buffer(buf, options, local);
			}
			InterfaceMember::Method {
				name,
				type_parameters,
				parameters,
				return_type,
				is_optional,
				..
			} => {
				name.to_string_from_buffer(buf, options, local);
				if *is_optional {
					buf.push('?');
				}
				if let Some(type_parameters) = &type_parameters {
					bracketed_items_to_string(type_parameters, ('<', '>'), buf, options, local);
				}
				parameters.to_string_from_buffer(buf, options, local);
				if let Some(return_type) = return_type {
					buf.push_str(": ");
					return_type.to_string_from_buffer(buf, options, local);
				}
			}
			InterfaceMember::Indexer { name, indexer_type, return_type, is_readonly, .. } => {
				if *is_readonly {
					buf.push_str("readonly ");
				}
				buf.push('[');
				buf.push_str(name.as_str());
				buf.push(':');
				indexer_type.to_string_from_buffer(buf, options, local);
				buf.push_str("]: ");
				return_type.to_string_from_buffer(buf, options, local);
			}
			InterfaceMember::Constructor {
				parameters,
				type_parameters,
				return_type,
				is_readonly,
				position: _,
			} => {
				if *is_readonly {
					buf.push_str("readonly ");
				}
				buf.push_str("new ");
				if let Some(ref type_parameters) = type_parameters {
					bracketed_items_to_string(type_parameters, ('<', '>'), buf, options, local);
				}
				parameters.to_string_from_buffer(buf, options, local);
				if let Some(ref return_type) = return_type {
					buf.push_str(": ");
					return_type.to_string_from_buffer(buf, options, local);
				}
			}
			InterfaceMember::Caller {
				parameters,
				type_parameters,
				return_type,
				is_readonly,
				position: _,
			} => {
				if *is_readonly {
					buf.push_str("readonly ");
				}
				if let Some(ref type_parameters) = type_parameters {
					bracketed_items_to_string(type_parameters, ('<', '>'), buf, options, local);
				}
				parameters.to_string_from_buffer(buf, options, local);
				if let Some(ref return_type) = return_type {
					buf.push_str(": ");
					return_type.to_string_from_buffer(buf, options, local);
				}
			}
			InterfaceMember::Rule {
				is_readonly,
				matching_type,
				optionality,
				output_type,
				as_type,
				parameter,
				position: _,
			} => {
				buf.push_str(match is_readonly {
					MappedReadonlyKind::Negated => "-readonly ",
					MappedReadonlyKind::Always => "readonly ",
					MappedReadonlyKind::False => "",
				});
				buf.push('[');
				buf.push_str(parameter.as_str());
				buf.push_str(" in ");
				matching_type.to_string_from_buffer(buf, options, local);
				if let Some(as_type) = as_type {
					buf.push_str(" as ");
					as_type.to_string_from_buffer(buf, options, local);
				}
				buf.push(']');
				buf.push_str(match optionality {
					Optionality::Default => ": ",
					Optionality::Optional => "?:",
					Optionality::Required => "-?:",
				});
				output_type.to_string_from_buffer(buf, options, local);
			}
			InterfaceMember::Comment(content, is_multiline, _) => {
				if *is_multiline {
					buf.push_str("/*");
					buf.push_str(content);
					buf.push_str("*/");
				} else {
					buf.push_str("//");
					buf.push_str(content);
					buf.push_new_line();
				}
			}
		}
	}
}

pub(crate) fn interface_members_from_reader(
	reader: &mut crate::Lexer,
) -> ParseResult<Vec<WithComment<Decorated<InterfaceMember>>>> {
	let mut members = Vec::new();
	loop {
		reader.skip();
		if reader.is_operator("}") {
			break;
		}
		let decorated_member = WithComment::from_reader(reader)?;

		if reader.is_operator("}") {
			members.push(decorated_member);
			break;
		}

		let comma = reader.is_keyword_advance(",");
		if !comma {
			reader.expect_semi_colon()?;
		}
		members.push(decorated_member);
	}
	Ok(members)
}
