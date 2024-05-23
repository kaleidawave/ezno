use std::ops::Neg;

use crate::{
	derive_ASTNode, parse_bracketed, throw_unexpected_token_with_token, to_string_bracketed,
	ListItem, ParseErrors, Quoted,
};
use crate::{
	errors::parse_lexing_error, expressions::TemplateLiteralPart,
	extensions::decorators::Decorated, Decorator, Marker, ParseResult, VariableField, WithComment,
};
use derive_partial_eq_extras::PartialEqExtras;
use iterator_endiate::EndiateIteratorExt;
use tokenizer_lib::sized_tokens::{SizedToken, TokenEnd, TokenReaderWithTokenEnds, TokenStart};

use super::{
	interface::{parse_interface_members, InterfaceMember},
	type_declarations::TypeParameter,
};

use crate::{
	ast::assignments::LHSOfAssignment, tokens::token_as_identifier, ASTNode, NumberRepresentation,
	ParseError, ParseOptions, Span, TSXKeyword, TSXToken, Token, TokenReader,
};

/// A reference to a type
#[apply(derive_ASTNode)]
#[derive(Debug, Clone, PartialEqExtras, get_field_by_type::GetFieldByType)]
#[get_field_by_type_target(Span)]
#[partial_eq_ignore_types(Span)]
pub enum TypeAnnotation {
	/// A name e.g. `IPost`
	Name(String, Span),
	CommonName(CommonTypes, Span),
	/// A name e.g. `Intl.IPost`. TODO can there be more than 2 members
	NamespacedName(String, String, Span),
	/// A name with generics e.g. `Array<number>`
	NameWithGenericArguments(String, Vec<TypeAnnotation>, Span),
	/// Union e.g. `number | string`
	Union(Vec<TypeAnnotation>, Span),
	/// Intersection e.g. `c & d`
	Intersection(Vec<TypeAnnotation>, Span),
	/// String literal e.g. `"foo"`
	StringLiteral(String, Quoted, Span),
	/// Number literal e.g. `45`
	NumberLiteral(NumberRepresentation, Span),
	/// Boolean literal e.g. `true`
	BooleanLiteral(bool, Span),
	/// Array literal e.g. `string[]`. This is syntactic sugar for `Array` with type arguments. **This is not the same
	/// as a [TypeAnnotation::TupleLiteral]**
	ArrayLiteral(Box<TypeAnnotation>, Span),
	/// Function literal e.g. `(x: string) => string`
	FunctionLiteral {
		type_parameters: Option<Vec<TypeParameter>>,
		parameters: TypeAnnotationFunctionParameters,
		return_type: Box<TypeAnnotation>,
		position: Span,
	},
	/// Construction literal e.g. `new (x: string) => string`
	ConstructorLiteral {
		type_parameters: Option<Vec<TypeParameter>>,
		parameters: TypeAnnotationFunctionParameters,
		return_type: Box<TypeAnnotation>,
		position: Span,
	},
	/// Object literal e.g. `{ y: string }`
	ObjectLiteral(Vec<WithComment<Decorated<InterfaceMember>>>, Span),
	/// Tuple literal e.g. `[number, x: string]`
	TupleLiteral(Vec<TupleLiteralElement>, Span),
	/// ?
	TemplateLiteral(Vec<TemplateLiteralPart<AnnotationWithBinder>>, Span),
	/// Declares type as not assignable (still has interior mutability) e.g. `readonly number`
	Readonly(Box<TypeAnnotation>, Span),
	/// Declares type as being union type of all property types e.g. `T[K]`
	Index(Box<TypeAnnotation>, Box<TypeAnnotation>, Span),
	/// KeyOf
	KeyOf(Box<TypeAnnotation>, Span),
	TypeOf(Box<LHSOfAssignment>, Span),
	Infer(String, Span),
	/// This is technically a special return type in TypeScript but we can make a superset behavior here
	Asserts(Box<TypeAnnotation>, Span),
	Extends {
		item: Box<TypeAnnotation>,
		extends: Box<TypeAnnotation>,
		position: Span,
	},
	Is {
		item: Box<TypeAnnotation>,
		is: Box<TypeAnnotation>,
		position: Span,
	},
	Conditional {
		condition: Box<TypeAnnotation>,
		resolve_true: Box<TypeAnnotation>,
		resolve_false: Box<TypeAnnotation>,
		position: Span,
	},
	Symbol {
		/// TODO unsure
		unique: bool,
		#[cfg(feature = "extras")]
		name: Option<String>,
		position: Span,
	},
	/// For operation precedence reasons
	ParenthesizedReference(Box<TypeAnnotation>, Span),
	Decorated(
		Decorator,
		#[cfg_attr(target_family = "wasm", tsify(type = "TypeAnnotation"))] Box<Self>,
		Span,
	),
	#[cfg_attr(feature = "self-rust-tokenize", self_tokenize_field(0))]
	Marker(Marker<TypeAnnotation>, Span),
}

impl ListItem for TypeAnnotation {}

#[derive(Debug, Clone, PartialEq)]
#[apply(derive_ASTNode)]
pub enum AnnotationWithBinder {
	Annotated { name: String, ty: TypeAnnotation, position: Span },
	NoAnnotation(TypeAnnotation),
}

impl ASTNode for AnnotationWithBinder {
	fn get_position(&self) -> Span {
		match self {
			AnnotationWithBinder::Annotated { position, .. } => *position,
			AnnotationWithBinder::NoAnnotation(ty) => ty.get_position(),
		}
	}

	fn from_reader(
		reader: &mut impl TokenReader<TSXToken, crate::TokenStart>,
		state: &mut crate::ParsingState,
		options: &ParseOptions,
	) -> ParseResult<Self> {
		if let Some(Token(TSXToken::Colon, _)) = reader.peek_n(1) {
			let (name, pos) =
				token_as_identifier(reader.next().unwrap(), "tuple literal named item")?;
			reader.next();
			let ty = TypeAnnotation::from_reader(reader, state, options)?;
			Ok(AnnotationWithBinder::Annotated { position: pos.union(ty.get_position()), name, ty })
		} else {
			TypeAnnotation::from_reader(reader, state, options).map(Self::NoAnnotation)
		}
	}

	fn to_string_from_buffer<T: source_map::ToString>(
		&self,
		buf: &mut T,
		options: &crate::ToStringOptions,
		local: crate::LocalToStringInformation,
	) {
		if let AnnotationWithBinder::Annotated { name, .. } = self {
			buf.push_str(name);
			buf.push_str(": ");
		}
		self.get_inner_ref().to_string_from_buffer(buf, options, local);
	}
}

impl AnnotationWithBinder {
	#[must_use]
	pub fn get_inner_ref(&self) -> &TypeAnnotation {
		match self {
			AnnotationWithBinder::Annotated { ty, .. } | AnnotationWithBinder::NoAnnotation(ty) => {
				ty
			}
		}
	}
}

#[derive(Debug, Clone, PartialEq, Eq)]
#[apply(derive_ASTNode)]
pub enum TupleElementKind {
	Standard,
	Spread,
	Optional,
}

/// Reduces string allocation and type lookup overhead
#[derive(Debug, Clone, PartialEq, Eq)]
#[apply(derive_ASTNode)]
pub enum CommonTypes {
	String,
	Number,
	Boolean,
}

impl ASTNode for TypeAnnotation {
	fn from_reader(
		reader: &mut impl TokenReader<TSXToken, crate::TokenStart>,
		state: &mut crate::ParsingState,
		options: &ParseOptions,
	) -> ParseResult<Self> {
		Self::from_reader_with_config(reader, state, options, None, None)
	}

	fn to_string_from_buffer<T: source_map::ToString>(
		&self,
		buf: &mut T,
		options: &crate::ToStringOptions,
		local: crate::LocalToStringInformation,
	) {
		match self {
			Self::Marker(..) => {
				assert!(options.expect_markers,);
			}
			Self::CommonName(name, _) => buf.push_str(match name {
				CommonTypes::String => "string",
				CommonTypes::Number => "number",
				CommonTypes::Boolean => "boolean",
			}),
			Self::Decorated(decorator, on_type_annotation, _) => {
				decorator.to_string_from_buffer(buf, options, local);
				buf.push(' ');
				on_type_annotation.to_string_from_buffer(buf, options, local);
			}
			Self::Name(name, _) => buf.push_str(name),
			Self::NameWithGenericArguments(name, arguments, _) => {
				buf.push_str(name);
				to_string_bracketed(arguments, ('<', '>'), buf, options, local);
			}
			Self::FunctionLiteral { type_parameters, parameters, return_type, .. } => {
				if let Some(type_parameters) = type_parameters {
					to_string_bracketed(type_parameters, ('<', '>'), buf, options, local);
				}
				parameters.to_string_from_buffer(buf, options, local);
				buf.push_str(" => ");
				return_type.to_string_from_buffer(buf, options, local);
			}
			Self::BooleanLiteral(expression, _) => {
				buf.push_str(if *expression { "true" } else { "false" });
			}
			Self::NumberLiteral(value, _) => {
				buf.push_str(&value.to_string());
			}
			Self::StringLiteral(expression, quoted, _) => {
				buf.push(quoted.as_char());
				buf.push_str(expression.as_str());
				buf.push(quoted.as_char());
			}
			Self::Union(union_members, _) => {
				for (at_end, member) in union_members.iter().endiate() {
					member.to_string_from_buffer(buf, options, local);
					if !at_end {
						buf.push_str(" | ");
					}
				}
			}
			Self::Intersection(intersection_members, _) => {
				for (at_end, member) in intersection_members.iter().endiate() {
					member.to_string_from_buffer(buf, options, local);
					if !at_end {
						buf.push_str(" & ");
					}
				}
			}
			Self::TypeOf(on, _pos) => {
				buf.push_str("typeof ");
				on.to_string_from_buffer(buf, options, local);
			}
			Self::Infer(name, _pos) => {
				buf.push_str("infer ");
				buf.push_str(name.as_str());
			}
			Self::NamespacedName(..) => todo!(),
			Self::ObjectLiteral(members, _) => {
				to_string_bracketed(members, ('{', '}'), buf, options, local);
			}
			Self::TupleLiteral(members, _) => {
				to_string_bracketed(members, ('[', ']'), buf, options, local);
			}
			Self::Index(on, with, _) => {
				on.to_string_from_buffer(buf, options, local);
				buf.push('[');
				with.to_string_from_buffer(buf, options, local);
				buf.push(']');
			}
			Self::KeyOf(item, _) => {
				buf.push_str("keyof ");
				item.to_string_from_buffer(buf, options, local);
			}
			Self::Conditional { condition, resolve_true, resolve_false, .. } => {
				// Same as expression::condition
				let split_lines = crate::are_nodes_over_length(
					[condition, resolve_true, resolve_false].iter().map(AsRef::as_ref),
					options,
					local,
					Some(
						u32::from(options.max_line_length)
							.saturating_sub(buf.characters_on_current_line()),
					),
					true,
				);
				condition.to_string_from_buffer(buf, options, local);
				if split_lines {
					buf.push_new_line();
					options.add_indent(local.depth + 1, buf);
					buf.push_str("? ");
				} else {
					buf.push_str(if options.pretty { " ? " } else { "?" });
				}
				resolve_true.to_string_from_buffer(buf, options, local);
				if split_lines {
					buf.push_new_line();
					options.add_indent(local.depth + 1, buf);
					buf.push_str(": ");
				} else {
					buf.push_str(if options.pretty { " : " } else { ":" });
				}
				resolve_false.to_string_from_buffer(buf, options, local);
			}
			Self::ArrayLiteral(item, _) => {
				item.to_string_from_buffer(buf, options, local);
				buf.push_str("[]");
			}
			Self::ConstructorLiteral { parameters, type_parameters, return_type, .. } => {
				buf.push_str("new ");
				if let Some(type_parameters) = type_parameters {
					to_string_bracketed(type_parameters, ('<', '>'), buf, options, local);
				}
				parameters.to_string_from_buffer(buf, options, local);
				buf.push_str(" => ");
				return_type.to_string_from_buffer(buf, options, local);
			}
			Self::Readonly(readonly_type, _) => {
				buf.push_str("readonly ");
				readonly_type.to_string_from_buffer(buf, options, local);
			}
			Self::ParenthesizedReference(reference, _) => {
				buf.push('(');
				reference.to_string_from_buffer(buf, options, local);
				buf.push(')');
			}
			Self::TemplateLiteral(parts, _) => {
				buf.push('`');
				for part in parts {
					match part {
						TemplateLiteralPart::Static(chunk) => buf.push_str(chunk),
						TemplateLiteralPart::Dynamic(reference) => {
							buf.push_str("${");
							reference.to_string_from_buffer(buf, options, local);
							buf.push('}');
						}
					}
				}
				buf.push('`');
			}
			Self::Symbol { .. } => buf.push_str("symbol"),
			Self::Extends { item, extends, .. } => {
				item.to_string_from_buffer(buf, options, local);
				buf.push_str(" extends ");
				extends.to_string_from_buffer(buf, options, local);
			}
			Self::Is { item, is, .. } => {
				item.to_string_from_buffer(buf, options, local);
				buf.push_str(" is ");
				is.to_string_from_buffer(buf, options, local);
			}
			Self::Asserts(predicate, _pos) => {
				buf.push_str("asserts ");
				predicate.to_string_from_buffer(buf, options, local);
			}
		}
	}

	fn get_position(&self) -> Span {
		*get_field_by_type::GetFieldByType::get(self)
	}
}

/// For parsing
#[derive(Clone, Copy)]
pub(crate) enum TypeOperatorKind {
	Union,
	Intersection,
	// not an implication, not an implication, not an implication
	Function,
	Query,
}

impl TypeAnnotation {
	/// Also returns the local the generic arguments ran over
	/// TODO refactor and tidy a lot of this, precedence rather than config
	pub(crate) fn from_reader_with_config(
		reader: &mut impl TokenReader<TSXToken, crate::TokenStart>,
		state: &mut crate::ParsingState,
		options: &ParseOptions,
		parent_kind: Option<TypeOperatorKind>,
		start: Option<TokenStart>,
	) -> ParseResult<Self> {
		if let (true, Some(Token(peek, at))) = (options.partial_syntax, reader.peek()) {
			let next_is_not_type_annotation_like = matches!(
				peek,
				TSXToken::CloseParentheses
					| TSXToken::CloseBracket
					| TSXToken::CloseBrace
					| TSXToken::Comma | TSXToken::OpenChevron
			) || peek.is_assignment()
				|| (start.map_or(false, |start| {
					peek.is_statement_or_declaration_start()
						&& state
							.line_starts
							.byte_indexes_on_different_lines(start.0 as usize, at.0 as usize)
				}));

			if next_is_not_type_annotation_like {
				let point = start.unwrap_or(*at);
				return Ok(TypeAnnotation::Marker(
					state.new_partial_point_marker(point),
					point.with_length(0),
				));
			}
		}

		while let Some(Token(TSXToken::Comment(_) | TSXToken::MultiLineComment(_), _)) =
			reader.peek()
		{
			reader.next();
		}

		if let (None, Some(Token(TSXToken::BitwiseOr, _))) = (parent_kind, reader.peek()) {
			reader.next();
		}
		if let (None, Some(Token(TSXToken::BitwiseAnd, _))) = (parent_kind, reader.peek()) {
			reader.next();
		}

		let mut reference = match reader.next().ok_or_else(parse_lexing_error)? {
			// Literals:
			t @ Token(TSXToken::Keyword(TSXKeyword::True), _) => {
				Self::BooleanLiteral(true, t.get_span())
			}
			t @ Token(TSXToken::Keyword(TSXKeyword::False), _) => {
				Self::BooleanLiteral(false, t.get_span())
			}
			Token(TSXToken::Keyword(TSXKeyword::Infer), start) => {
				let token = reader.next().ok_or_else(parse_lexing_error)?;
				let (name, position) = token_as_identifier(token, "infer name")?;
				let position = start.union(position);
				Self::Infer(name, position)
			}
			Token(TSXToken::Keyword(TSXKeyword::Asserts), start) => {
				let predicate = TypeAnnotation::from_reader_with_config(
					reader,
					state,
					options,
					parent_kind,
					Some(start),
				)?;
				let position = start.union(predicate.get_position());
				Self::Asserts(Box::new(predicate), position)
			}
			t @ Token(TSXToken::Keyword(TSXKeyword::Symbol), _) => {
				let position = t.get_span();
				#[cfg(feature = "extras")]
				let name =
					reader.conditional_next(|t| matches!(t, TSXToken::StringLiteral(..))).map(
						|t| {
							if let Token(TSXToken::StringLiteral(content, _), _) = t {
								content
							} else {
								unreachable!()
							}
						},
					);
				Self::Symbol {
					unique: false,
					position,
					#[cfg(feature = "extras")]
					name,
				}
			}
			t @ Token(TSXToken::Keyword(TSXKeyword::Unique), _) => {
				let sym_pos = reader.expect_next(TSXToken::Keyword(TSXKeyword::Symbol))?;
				let position = t.get_span().union(sym_pos.with_length("symbol".len()));
				#[cfg(feature = "extras")]
				let name =
					reader.conditional_next(|t| matches!(t, TSXToken::StringLiteral(..))).map(
						|t| {
							if let Token(TSXToken::StringLiteral(content, _), _) = t {
								content
							} else {
								unreachable!()
							}
						},
					);
				Self::Symbol {
					unique: false,
					position,
					#[cfg(feature = "extras")]
					name,
				}
			}
			Token(TSXToken::NumberLiteral(num), start) => {
				let pos = start.with_length(num.len());
				Self::NumberLiteral(num.parse::<NumberRepresentation>().unwrap(), pos)
			}
			Token(TSXToken::Subtract, start) => {
				let Token(token, pos) = reader.next().ok_or_else(parse_lexing_error)?;
				if let TSXToken::NumberLiteral(num) = token {
					let pos = pos.union(start.with_length(num.len()));
					let number_representation = num.parse::<NumberRepresentation>().unwrap();
					// important negation here
					Self::NumberLiteral(number_representation.neg(), pos)
				} else {
					return Err(ParseError::new(
						ParseErrors::ExpectedNumberLiteral,
						start.with_length(token.length() as usize + 1),
					));
				}
			}
			Token(TSXToken::StringLiteral(content, quoted), start) => {
				let pos = start.with_length(content.len() + 2);
				Self::StringLiteral(content, quoted, pos)
			}
			Token(TSXToken::At, pos) => {
				let decorator = Decorator::from_reader_sub_at_symbol(reader, state, options, pos)?;
				// TODO ...
				let this_declaration = Self::from_reader_with_config(
					reader,
					state,
					options,
					Some(TypeOperatorKind::Query),
					start,
				)?;
				let position = pos.union(this_declaration.get_position());
				Self::Decorated(decorator, Box::new(this_declaration), position)
			}
			// Function literal or group
			Token(TSXToken::OpenParentheses, start) => {
				// Discern between group or arrow function:
				let mut bracket_count = 1;
				let next = reader.scan(|t, _| {
					match t {
						TSXToken::OpenParentheses => {
							bracket_count += 1;
						}
						TSXToken::CloseParentheses => {
							bracket_count -= 1;
						}
						_ => {}
					}
					bracket_count == 0
				});
				// If arrow function OR group
				if let Some(Token(TSXToken::Arrow, _)) = next {
					let parameters =
						TypeAnnotationFunctionParameters::from_reader_sub_open_parenthesis(
							reader, state, options, start,
						)?;
					reader.expect_next(TSXToken::Arrow)?;
					let return_type = Self::from_reader(reader, state, options)?;
					Self::FunctionLiteral {
						position: start.union(return_type.get_position()),
						type_parameters: None,
						parameters,
						return_type: Box::new(return_type),
					}
				} else {
					let type_annotation = Self::from_reader(reader, state, options)?;
					let position =
						start.union(reader.expect_next_get_end(TSXToken::CloseParentheses)?);
					Self::ParenthesizedReference(type_annotation.into(), position)
				}
			}
			Token(TSXToken::OpenChevron, start) => {
				let (type_parameters, _) =
					parse_bracketed(reader, state, options, None, TSXToken::CloseChevron)?;
				let parameters =
					TypeAnnotationFunctionParameters::from_reader(reader, state, options)?;
				reader.expect_next(TSXToken::Arrow)?;
				let return_type = Self::from_reader(reader, state, options)?;
				Self::FunctionLiteral {
					position: start.union(return_type.get_position()),
					type_parameters: Some(type_parameters),
					parameters,
					return_type: Box::new(return_type),
				}
			}
			// Object literal type
			Token(TSXToken::OpenBrace, start) => {
				let members = parse_interface_members(reader, state, options)?;
				let position = start.union(reader.expect_next_get_end(TSXToken::CloseBrace)?);
				Self::ObjectLiteral(members, position)
			}
			// Tuple literal type
			Token(TSXToken::OpenBracket, start) => {
				let (members, end) =
					parse_bracketed(reader, state, options, None, TSXToken::CloseBracket)?;
				let position = start.union(end);
				Self::TupleLiteral(members, position)
			}
			Token(TSXToken::TemplateLiteralStart, start) => {
				let mut parts = Vec::new();
				let mut end = None;
				while end.is_none() {
					match reader.next().ok_or_else(parse_lexing_error)? {
						Token(TSXToken::TemplateLiteralChunk(chunk), _) => {
							parts.push(TemplateLiteralPart::Static(chunk));
						}
						Token(TSXToken::TemplateLiteralExpressionStart, _) => {
							let expression =
								AnnotationWithBinder::from_reader(reader, state, options)?;
							reader.expect_next(TSXToken::TemplateLiteralExpressionEnd)?;
							parts.push(TemplateLiteralPart::Dynamic(Box::new(expression)));
						}
						Token(TSXToken::TemplateLiteralEnd, end_position) => {
							end = Some(TokenEnd::new(end_position.0));
						}
						token => {
							eprintln!("Found token {token:?}");
							return Err(parse_lexing_error());
						}
					}
				}
				Self::TemplateLiteral(parts, start.union(end.unwrap()))
			}
			Token(TSXToken::Keyword(TSXKeyword::Readonly), start) => {
				let readonly_type = TypeAnnotation::from_reader(reader, state, options)?;
				let position = start.union(readonly_type.get_position());
				return Ok(TypeAnnotation::Readonly(Box::new(readonly_type), position));
			}
			Token(TSXToken::Keyword(TSXKeyword::KeyOf), start) => {
				let key_of_type = TypeAnnotation::from_reader(reader, state, options)?;
				let position = start.union(key_of_type.get_position());
				return Ok(TypeAnnotation::KeyOf(Box::new(key_of_type), position));
			}
			Token(TSXToken::Keyword(TSXKeyword::New), start) => {
				let type_parameters = reader
					.conditional_next(|token| *token == TSXToken::OpenChevron)
					.is_some()
					.then(|| {
						parse_bracketed(reader, state, options, None, TSXToken::CloseChevron)
							.map(|(params, _items)| params)
					})
					.transpose()?;
				let parameters =
					TypeAnnotationFunctionParameters::from_reader(reader, state, options)?;

				reader.expect_next(TSXToken::Arrow)?;
				let return_type = Self::from_reader(reader, state, options)?;
				Self::ConstructorLiteral {
					position: start.union(return_type.get_position()),
					parameters,
					type_parameters,
					return_type: Box::new(return_type),
				}
			}
			token => {
				let (name, pos) = token_as_identifier(token, "type reference")?;
				match name.as_str() {
					"string" => Self::CommonName(CommonTypes::String, pos),
					"number" => Self::CommonName(CommonTypes::Number, pos),
					"boolean" => Self::CommonName(CommonTypes::Boolean, pos),
					_ => Self::Name(name, pos),
				}
			}
		};
		// Namespaced name
		if let Some(Token(TSXToken::Dot, _)) = reader.peek() {
			let Self::Name(name, start) = reference else { return Ok(reference) };
			reader.next();
			let (namespace_member, end) =
				token_as_identifier(reader.next().unwrap(), "namespace name")?;
			let position = start.union(end);
			return Ok(TypeAnnotation::NamespacedName(name, namespace_member, position));
		}
		// Generics arguments:
		if let Some(Token(TSXToken::OpenChevron, _position)) = reader.peek() {
			// Assert its a Self::Name
			let Self::Name(name, start_span) = reference else {
				let position = reader.next().unwrap().get_span();
				return Err(ParseError::new(
					crate::ParseErrors::TypeArgumentsNotValidOnReference,
					position,
				));
			};
			reader.next();
			let (generic_arguments, end) =
				generic_arguments_from_reader_sub_open_angle(reader, state, options, parent_kind)?;
			reference =
				Self::NameWithGenericArguments(name, generic_arguments, start_span.union(end));
		};

		// Array shorthand & indexing type references. Loops as number[][]
		// unsure if index type can be looped
		while reader.conditional_next(|tok| *tok == TSXToken::OpenBracket).is_some() {
			let start = reference.get_position();
			if let Some(Token(TSXToken::CloseBracket, _)) = reader.peek() {
				let position = reference
					.get_position()
					.union(reader.next().ok_or_else(parse_lexing_error)?.get_end());
				reference = Self::ArrayLiteral(Box::new(reference), position);
			} else {
				// E.g type allTypes = Person[keyof Person];
				let indexer = TypeAnnotation::from_reader(reader, state, options)?;
				let position = start.union(reader.expect_next_get_end(TSXToken::CloseBracket)?);
				reference = Self::Index(Box::new(reference), Box::new(indexer), position);
			}
		}

		match reader.peek() {
			Some(Token(TSXToken::Keyword(TSXKeyword::Extends), _)) => {
				reader.next();
				let extends_type = TypeAnnotation::from_reader_with_config(
					reader,
					state,
					options,
					Some(TypeOperatorKind::Query),
					start,
				)?;
				// TODO local
				let position = reference.get_position().union(extends_type.get_position());
				reference = TypeAnnotation::Extends {
					item: Box::new(reference),
					extends: Box::new(extends_type),
					position,
				};
			}
			Some(Token(TSXToken::Keyword(TSXKeyword::Is), _)) => {
				reader.next();
				let is_type = TypeAnnotation::from_reader_with_config(
					reader,
					state,
					options,
					Some(TypeOperatorKind::Query),
					start,
				)?;
				// TODO local
				let position = reference.get_position().union(is_type.get_position());

				reference = TypeAnnotation::Is {
					item: Box::new(reference),
					is: Box::new(is_type),
					position,
				};
			}
			_ => {}
		}

		// Extends, Is, Intersections & Unions or implicit function literals
		match reader.peek() {
			Some(Token(TSXToken::BitwiseOr, _)) => {
				if matches!(parent_kind, Some(TypeOperatorKind::Query | TypeOperatorKind::Function))
				{
					return Ok(reference);
				}
				let mut union_members = vec![reference];
				while let Some(Token(TSXToken::BitwiseOr, _)) = reader.peek() {
					reader.next();
					union_members.push(Self::from_reader_with_config(
						reader,
						state,
						options,
						Some(TypeOperatorKind::Union),
						start,
					)?);
				}
				let position = union_members
					.first()
					.unwrap()
					.get_position()
					.union(union_members.last().unwrap().get_position());
				Ok(Self::Union(union_members, position))
			}
			Some(Token(TSXToken::BitwiseAnd, _)) => {
				if matches!(
					parent_kind,
					Some(
						TypeOperatorKind::Union
							| TypeOperatorKind::Query | TypeOperatorKind::Function
					)
				) {
					return Ok(reference);
				}
				let mut intersection_members = vec![reference];
				while let Some(Token(TSXToken::BitwiseAnd, _)) = reader.peek() {
					reader.next();
					intersection_members.push(Self::from_reader_with_config(
						reader,
						state,
						options,
						Some(TypeOperatorKind::Intersection),
						start,
					)?);
				}
				let position = intersection_members
					.first()
					.unwrap()
					.get_position()
					.union(intersection_members.last().unwrap().get_position());

				Ok(Self::Intersection(intersection_members, position))
			}
			Some(Token(TSXToken::Arrow, _)) => {
				if matches!(parent_kind, Some(TypeOperatorKind::Query | TypeOperatorKind::Function))
				{
					return Ok(reference);
				}
				reader.next();
				let return_type = Self::from_reader_with_config(
					reader,
					state,
					options,
					Some(TypeOperatorKind::Function),
					start,
				)?;
				let parameters_position = reference.get_position();
				let position = parameters_position.union(return_type.get_position());
				Ok(Self::FunctionLiteral {
					position,
					type_parameters: None,
					parameters: TypeAnnotationFunctionParameters {
						parameters: vec![TypeAnnotationFunctionParameter {
							position,
							name: None,
							type_annotation: reference,
							is_optional: false,
							decorators: Default::default(),
						}],
						rest_parameter: None,
						position: parameters_position,
					},
					return_type: Box::new(return_type),
				})
			}
			Some(Token(TSXToken::QuestionMark, _)) => {
				if let Some(TypeOperatorKind::Query) = parent_kind {
					return Ok(reference);
				}
				reader.next();
				let lhs = TypeAnnotation::from_reader(reader, state, options)?;
				reader.expect_next(TSXToken::Colon)?;
				let rhs = TypeAnnotation::from_reader(reader, state, options)?;
				let position = reference.get_position().union(rhs.get_position());
				// TODO zero here ..?
				Ok(TypeAnnotation::Conditional {
					condition: Box::new(reference),
					resolve_true: Box::new(lhs),
					resolve_false: Box::new(rhs),
					position,
				})
			}
			_ => Ok(reference),
		}
	}
}

/// Parses the arguments (vector of [`TypeAnnotation`]s) parsed to to a type reference or function call.
/// Returns arguments and the closing span.
/// TODO could use parse bracketed but needs to have the more complex logic inside
pub(crate) fn generic_arguments_from_reader_sub_open_angle(
	reader: &mut impl TokenReader<TSXToken, crate::TokenStart>,
	state: &mut crate::ParsingState,
	options: &ParseOptions,
	kind: Option<TypeOperatorKind>,
) -> ParseResult<(Vec<TypeAnnotation>, TokenEnd)> {
	let mut generic_arguments = Vec::new();

	loop {
		let argument = TypeAnnotation::from_reader_with_config(reader, state, options, kind, None)?;

		generic_arguments.push(argument);

		// Handling for the fact that concessive chevrons are grouped into bitwise shifts
		// One option is to keep track of local but as a simpler way mutate the upcoming token
		// TODO spans

		let peek_mut = reader.peek_mut();

		if let Some(Token(t @ TSXToken::BitwiseShiftRight, start)) = peek_mut {
			let end = TokenEnd::new(start.0 + 1);
			start.0 += 1;
			*t = TSXToken::CloseChevron;
			return Ok((generic_arguments, end));
		}

		if let Some(Token(t @ TSXToken::BitwiseShiftRightUnsigned, start)) = peek_mut {
			let end = TokenEnd::new(start.0 + 2);
			start.0 += 2;
			*t = TSXToken::CloseChevron;
			return Ok((generic_arguments, end));
		}

		match reader.next().ok_or_else(parse_lexing_error)? {
			Token(TSXToken::Comma, _) => {}
			t @ Token(TSXToken::CloseChevron, _) => return Ok((generic_arguments, t.get_end())),
			token => {
				return throw_unexpected_token_with_token(
					token,
					&[TSXToken::CloseChevron, TSXToken::Comma],
				)
			}
		};
	}
}

/// Mirrors [`crate::FunctionParameters`]
#[derive(Debug, Clone, PartialEq)]
#[apply(derive_ASTNode)]
pub struct TypeAnnotationFunctionParameters {
	pub parameters: Vec<TypeAnnotationFunctionParameter>,
	pub rest_parameter: Option<Box<TypeAnnotationSpreadFunctionParameter>>,
	pub position: Span,
}

impl ASTNode for TypeAnnotationFunctionParameters {
	fn get_position(&self) -> Span {
		self.position
	}

	fn from_reader(
		reader: &mut impl TokenReader<TSXToken, crate::TokenStart>,
		state: &mut crate::ParsingState,
		options: &ParseOptions,
	) -> ParseResult<Self> {
		let start = reader.expect_next(TSXToken::OpenParentheses)?;
		Self::from_reader_sub_open_parenthesis(reader, state, options, start)
	}

	fn to_string_from_buffer<T: source_map::ToString>(
		&self,
		buf: &mut T,
		options: &crate::ToStringOptions,
		local: crate::LocalToStringInformation,
	) {
		buf.push('(');
		for parameter in &self.parameters {
			if let Some(ref name) = parameter.name {
				name.to_string_from_buffer(buf, options, local);
			}
			if parameter.is_optional {
				buf.push_str("?: ");
			} else {
				buf.push_str(": ");
			}
			parameter.type_annotation.to_string_from_buffer(buf, options, local);
		}
		if let Some(ref rest_parameter) = self.rest_parameter {
			buf.push_str("...");
			buf.push_str(&rest_parameter.name);
			rest_parameter.type_annotation.to_string_from_buffer(buf, options, local);
		}
		buf.push(')');
	}
}

impl TypeAnnotationFunctionParameters {
	pub(crate) fn from_reader_sub_open_parenthesis(
		reader: &mut impl TokenReader<TSXToken, crate::TokenStart>,
		state: &mut crate::ParsingState,
		options: &ParseOptions,
		start: TokenStart,
	) -> ParseResult<Self> {
		let mut parameters = Vec::new();
		let mut rest_parameter = None;
		while !matches!(reader.peek(), Some(Token(TSXToken::CloseParentheses, _))) {
			while reader.peek().map_or(false, |Token(ty, _)| ty.is_comment()) {
				reader.next();
			}
			let mut decorators = Vec::<Decorator>::new();
			while let Some(Token(TSXToken::At, _)) = reader.peek() {
				decorators.push(Decorator::from_reader(reader, state, options)?);
			}

			if let Some(Token(TSXToken::Spread, _)) = reader.peek() {
				let token = reader.next().unwrap();
				let (name, _) = token_as_identifier(
					reader.next().ok_or_else(parse_lexing_error)?,
					"spread function parameter",
				)?;
				reader.expect_next(TSXToken::Colon)?;
				let type_annotation = TypeAnnotation::from_reader(reader, state, options)?;
				rest_parameter = Some(Box::new(TypeAnnotationSpreadFunctionParameter {
					position: token.get_span().union(type_annotation.get_position()),
					name,
					type_annotation,
					decorators,
				}));
				break;
			}

			let mut local = 0;
			let after_variable_field = reader.scan(|token, _| match token {
				TSXToken::OpenBracket | TSXToken::OpenBrace | TSXToken::OpenParentheses => {
					local += 1;
					false
				}
				TSXToken::CloseBracket | TSXToken::CloseBrace | TSXToken::CloseParentheses => {
					local -= 1;
					local == 0
				}
				_ => local == 0,
			});
			let name: Option<WithComment<VariableField>> =
				if let Some(Token(TSXToken::Colon | TSXToken::OptionalMember, _)) =
					after_variable_field
				{
					Some(ASTNode::from_reader(reader, state, options)?)
				} else {
					None
				};
			let is_optional = match reader.next().ok_or_else(parse_lexing_error)? {
				Token(TSXToken::Colon, _) => false,
				Token(TSXToken::OptionalMember, _) => true,
				token => {
					return throw_unexpected_token_with_token(
						token,
						&[TSXToken::Colon, TSXToken::OptionalMember],
					)
				}
			};
			let type_annotation = TypeAnnotation::from_reader(reader, state, options)?;
			let position = name
				.as_ref()
				.map_or(type_annotation.get_position(), ASTNode::get_position)
				.union(type_annotation.get_position());

			parameters.push(TypeAnnotationFunctionParameter {
				decorators,
				name,
				type_annotation,
				is_optional,
				position,
			});

			if reader.conditional_next(|tok| matches!(tok, TSXToken::Comma)).is_none() {
				break;
			}
		}
		let end = reader.expect_next_get_end(TSXToken::CloseParentheses)?;
		Ok(TypeAnnotationFunctionParameters {
			position: start.union(end),
			parameters,
			rest_parameter,
		})
	}
}

#[derive(Debug, Clone, PartialEq)]
#[apply(derive_ASTNode)]
pub struct TypeAnnotationFunctionParameter {
	pub decorators: Vec<Decorator>,
	/// Ooh nice optional
	pub name: Option<WithComment<VariableField>>,
	pub type_annotation: TypeAnnotation,
	pub is_optional: bool,
	pub position: Span,
}

#[derive(Debug, Clone, PartialEq)]
#[apply(derive_ASTNode)]
pub struct TypeAnnotationSpreadFunctionParameter {
	pub decorators: Vec<Decorator>,
	pub name: String,
	pub type_annotation: TypeAnnotation,
	pub position: Span,
}

#[derive(Debug, Clone, PartialEq)]
#[apply(derive_ASTNode)]
pub struct TupleLiteralElement(pub TupleElementKind, pub AnnotationWithBinder, pub Span);

impl ASTNode for TupleLiteralElement {
	fn get_position(&self) -> Span {
		self.2
	}

	fn from_reader(
		reader: &mut impl TokenReader<TSXToken, crate::TokenStart>,
		state: &mut crate::ParsingState,
		options: &crate::ParseOptions,
	) -> ParseResult<Self> {
		let is_spread = reader.conditional_next(|token| matches!(token, TSXToken::Spread));

		let annotation_with_binder = AnnotationWithBinder::from_reader(reader, state, options)?;

		let (kind, position) = if let Some(spread) = is_spread {
			(TupleElementKind::Spread, spread.1.union(annotation_with_binder.get_position()))
		} else if let Some(trailing_question) =
			reader.conditional_next(|token| matches!(token, TSXToken::QuestionMark))
		{
			(
				TupleElementKind::Optional,
				annotation_with_binder.get_position().union(trailing_question.get_end()),
			)
		} else {
			(TupleElementKind::Standard, annotation_with_binder.get_position())
		};

		Ok(Self(kind, annotation_with_binder, position))
	}

	fn to_string_from_buffer<T: source_map::ToString>(
		&self,
		buf: &mut T,
		options: &crate::ToStringOptions,
		local: crate::LocalToStringInformation,
	) {
		if let TupleElementKind::Spread = self.0 {
			buf.push_str("...");
		}
		self.1.to_string_from_buffer(buf, options, local);
		if let TupleElementKind::Optional = self.0 {
			buf.push('?');
		}
	}
}

impl ListItem for TupleLiteralElement {
	const EMPTY: Option<Self> = None;

	fn allow_comma_after(&self) -> bool {
		true
	}
}

#[cfg(test)]
mod tests {
	use super::*;
	use crate::{assert_matches_ast, span, NumberRepresentation};

	#[test]
	fn name() {
		assert_matches_ast!("something", TypeAnnotation::Name(Deref @ "something", span!(0, 9)));
		assert_matches_ast!("string", TypeAnnotation::CommonName(CommonTypes::String, span!(0, 6)));
	}

	#[test]
	fn literals() {
		assert_matches_ast!(
			"\"my_string\"",
			TypeAnnotation::StringLiteral(Deref @ "my_string", Quoted::Double, span!(0, 11))
		);
		assert_matches_ast!(
			"45",
			TypeAnnotation::NumberLiteral(NumberRepresentation::Number { .. }, span!(0, 2))
		);
		assert_matches_ast!("true", TypeAnnotation::BooleanLiteral(true, span!(0, 4)));
	}

	#[test]
	fn generics() {
		assert_matches_ast!(
			"Array<string>",
			TypeAnnotation::NameWithGenericArguments(
				Deref @ "Array",
				Deref @ [TypeAnnotation::CommonName(CommonTypes::String, span!(6, 12))],
				span!(0, 13),
			)
		);

		assert_matches_ast!(
			"Map<string, number>",
			TypeAnnotation::NameWithGenericArguments(
				Deref @ "Map",
				Deref @
				[TypeAnnotation::CommonName(CommonTypes::String, span!(4, 10)), TypeAnnotation::CommonName(CommonTypes::Number, span!(12, 18))],
				span!(0, 19),
			)
		);

		assert_matches_ast!(
			"Array<Array<string>>",
			TypeAnnotation::NameWithGenericArguments(
				Deref @ "Array",
				Deref @ [TypeAnnotation::NameWithGenericArguments(
					Deref @ "Array",
					Deref @ [TypeAnnotation::CommonName(CommonTypes::String, span!(12, 18))],
					span!(6, 19),
				)],
				span!(0, 20),
			)
		);
	}

	#[test]
	fn union() {
		assert_matches_ast!(
			"string | number",
			TypeAnnotation::Union(
				Deref @
				[TypeAnnotation::CommonName(CommonTypes::String, span!(0, 6)), TypeAnnotation::CommonName(CommonTypes::Number, span!(9, 15))],
				_,
			)
		);

		// Leading | is valid
		assert_matches_ast!(
			"| string | number",
			TypeAnnotation::Union(
				Deref @
				[TypeAnnotation::CommonName(CommonTypes::String, span!(2, 8)), TypeAnnotation::CommonName(CommonTypes::Number, span!(11, 17))],
				_,
			)
		);
	}

	#[test]
	fn intersection() {
		assert_matches_ast!(
			"string & number",
			TypeAnnotation::Intersection(
				Deref @
				[TypeAnnotation::CommonName(CommonTypes::String, span!(0, 6)), TypeAnnotation::CommonName(CommonTypes::Number, span!(9, 15))],
				_,
			)
		);
	}

	#[test]
	fn tuple_literal() {
		assert_matches_ast!(
			"[number, x: string]",
			TypeAnnotation::TupleLiteral(
				Deref @ [TupleLiteralElement(
					TupleElementKind::Standard,
					AnnotationWithBinder::NoAnnotation(TypeAnnotation::CommonName(
						CommonTypes::Number,
						span!(1, 7),
					)),
					_,
				), TupleLiteralElement(
					TupleElementKind::Standard,
					AnnotationWithBinder::Annotated {
						name: Deref @ "x",
						ty: TypeAnnotation::CommonName(CommonTypes::String, span!(12, 18)),
						position: _,
					},
					_,
				)],
				span!(0, 19),
			)
		);
	}

	#[test]
	fn functions() {
		assert_matches_ast!(
			"T => T",
			TypeAnnotation::FunctionLiteral {
				type_parameters: None,
				parameters: TypeAnnotationFunctionParameters {
					parameters: Deref @ [ TypeAnnotationFunctionParameter { .. } ],
					..
				},
				return_type: Deref @ TypeAnnotation::Name(Deref @ "T", span!(5, 6)),
				..
			}
		);
		// TODO more
	}

	#[test]
	fn template_literal() {
		assert_matches_ast!(
			"`test-${X}`",
			TypeAnnotation::TemplateLiteral(
				Deref
				@ [TemplateLiteralPart::Static(Deref @ "test-"), TemplateLiteralPart::Dynamic(
					Deref @ AnnotationWithBinder::NoAnnotation(TypeAnnotation::Name(
						Deref @ "X",
						span!(8, 9),
					)),
				)],
				_,
			)
		);
	}

	#[test]
	fn array_shorthand() {
		assert_matches_ast!(
			"string[]",
			TypeAnnotation::ArrayLiteral(
				Deref @ TypeAnnotation::CommonName(CommonTypes::String, span!(0, 6)),
				span!(0, 8),
			)
		);
		assert_matches_ast!(
			"(number | null)[]",
			TypeAnnotation::ArrayLiteral(
				Deref @ TypeAnnotation::ParenthesizedReference(
					Deref @ TypeAnnotation::Union(
						Deref @
						[TypeAnnotation::CommonName(CommonTypes::Number, span!(1, 7)), TypeAnnotation::Name(Deref @ "null", span!(10, 14))],
						_,
					),
					span!(0, 15),
				),
				span!(0, 17),
			)
		);
		assert_matches_ast!(
			"string[][]",
			TypeAnnotation::ArrayLiteral(
				Deref @ TypeAnnotation::ArrayLiteral(
					Deref @ TypeAnnotation::CommonName(CommonTypes::String, span!(0, 6)),
					span!(0, 8),
				),
				span!(0, 10),
			)
		);
	}
}
