use std::ops::Neg;

use crate::{
	ast::VariableOrPropertyAccess, bracketed_items_from_reader, bracketed_items_to_string,
	derive_ASTNode, extensions::decorators::Decorated, number::NumberRepresentation, ASTNode,
	Decorator, ListItem, Marker, ParseError, ParseErrors, ParseResult, Quoted, Span, VariableField,
	WithComment,
};
use iterator_endiate::EndiateIteratorExt;

use super::{interface::InterfaceMember, type_declarations::TypeParameter};

/// A reference to a type
#[apply(derive_ASTNode)]
#[derive(Debug, Clone, get_field_by_type::GetFieldByType)]
#[get_field_by_type_target(Span)]
pub enum TypeAnnotation {
	/// Common types that don't have to allocate a string for
	CommonName(CommonTypes, Span),
	/// A name e.g. `IPost`
	Name(TypeName, Span),
	/// A name with generics e.g. `Array<number>`
	NameWithGenericArguments(TypeName, Vec<TypeAnnotation>, Span),
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
		parameters: Box<TypeAnnotationFunctionParameters>,
		return_type: Box<TypeAnnotation>,
		position: Span,
	},
	/// Construction literal e.g. `new (x: string) => string`
	ConstructorLiteral {
		type_parameters: Option<Vec<TypeParameter>>,
		parameters: Box<TypeAnnotationFunctionParameters>,
		return_type: Box<TypeAnnotation>,
		position: Span,
	},
	/// Object literal e.g. `{ y: string }`
	ObjectLiteral(Vec<WithComment<Decorated<InterfaceMember>>>, Span),
	/// Tuple literal e.g. `[number, x: string]`
	TupleLiteral(Vec<TupleLiteralElement>, Span),
	/// ?
	TemplateLiteral {
		parts: Vec<(String, AnnotationWithBinder)>,
		final_part: String,
		position: Span,
	},
	/// Declares type as not assignable (still has interior mutability) e.g. `readonly number`
	Readonly(Box<TypeAnnotation>, Span),
	/// I have no idea what this is for?
	Abstract(Box<TypeAnnotation>, Span),
	/// Declares type as being union type of all property types e.g. `T[K]`
	Index(Box<TypeAnnotation>, Box<TypeAnnotation>, Span),
	/// KeyOf
	KeyOf(Box<TypeAnnotation>, Span),
	TypeOf(Box<VariableOrPropertyAccess>, Span),
	Infer {
		name: String,
		extends: Option<Box<TypeAnnotation>>,
		position: Span,
	},
	/// This is technically a special return type in TypeScript but we can make a superset behavior here
	Asserts(Box<TypeAnnotation>, Span),
	Extends {
		item: Box<TypeAnnotation>,
		extends: Box<TypeAnnotation>,
		position: Span,
	},
	Is {
		reference: IsItem,
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
	/// With decorators
	Decorated(Box<Decorator>, Box<TypeAnnotation>, Span),
	/// Allowed in certain positions
	This(Span),
	#[cfg(feature = "extras")]
	/// *almost* Shorthand for ClassName & { ... }
	NameWithProperties(TypeName, Vec<BinderWithAnnotation>, Span),
	#[cfg_attr(feature = "self-rust-tokenize", self_tokenize_field(0))]
	Marker(Marker<TypeAnnotation>, Span),
}

impl ListItem for TypeAnnotation {
	type LAST = ();
}

#[derive(Debug, Clone)]
#[apply(derive_ASTNode)]
pub struct AnnotationWithBinder {
	pub name: Option<String>,
	pub type_annotation: TypeAnnotation,
	pub position: Span,
}

impl ASTNode for AnnotationWithBinder {
	fn get_position(&self) -> Span {
		self.position
	}

	fn from_reader(reader: &mut crate::Lexer) -> ParseResult<Self> {
		let start = reader.get_start();
		let name = if reader.after_identifier().starts_with(':') {
			let name = reader.parse_identifier("type annotation binder", false)?.to_owned();
			let _ = reader.expect(':')?;
			Some(name)
		} else {
			None
		};
		let type_annotation = TypeAnnotation::from_reader(reader)?;
		Ok(AnnotationWithBinder { position: start.union(reader.get_end()), name, type_annotation })
	}

	fn to_string_from_buffer<T: source_map::ToString>(
		&self,
		buf: &mut T,
		options: &crate::ToStringOptions,
		local: crate::LocalToStringInformation,
	) {
		if let Some(name) = &self.name {
			buf.push_str(name);
			buf.push_str(": ");
		}
		self.type_annotation.to_string_from_buffer(buf, options, local);
	}
}

#[derive(Debug, Clone)]
#[apply(derive_ASTNode)]
pub struct BinderWithAnnotation {
	pub name: crate::PropertyKey<crate::property_key::AlwaysPublic>,
	pub ty: Option<TypeAnnotation>,
	pub position: Span,
}

impl ASTNode for BinderWithAnnotation {
	fn get_position(&self) -> Span {
		self.position
	}

	fn from_reader(reader: &mut crate::Lexer) -> ParseResult<Self> {
		let start = reader.get_start();
		let name = crate::PropertyKey::from_reader(reader)?;
		let ty = if reader.is_operator_advance(":") {
			Some(TypeAnnotation::from_reader(reader)?)
		} else {
			None
		};
		Ok(BinderWithAnnotation { position: start.union(reader.get_end()), name, ty })
	}

	fn to_string_from_buffer<T: source_map::ToString>(
		&self,
		buf: &mut T,
		options: &crate::ToStringOptions,
		local: crate::LocalToStringInformation,
	) {
		self.name.to_string_from_buffer(buf, options, local);
		if let Some(ty) = &self.ty {
			buf.push_str(": ");
			ty.to_string_from_buffer(buf, options, local);
		}
	}
}

#[derive(Debug, Clone)]
#[apply(derive_ASTNode)]
pub enum TupleElementKind {
	Standard,
	Spread,
	Optional,
}

/// Reduces string allocation and type lookup overhead. This always point to the same type regardless of context (because no type is allowed to be named these)
#[derive(Debug, Clone)]
#[apply(derive_ASTNode)]
pub enum CommonTypes {
	String,
	Number,
	Boolean,
	Any,
	Null,
	Undefined,
	Unknown,
	Never,
}

impl CommonTypes {
	fn name(&self) -> &'static str {
		match self {
			CommonTypes::String => "string",
			CommonTypes::Number => "number",
			CommonTypes::Boolean => "boolean",
			CommonTypes::Any => "any",
			CommonTypes::Null => "null",
			CommonTypes::Undefined => "undefined",
			CommonTypes::Never => "never",
			CommonTypes::Unknown => "unknown",
		}
	}
}

#[apply(derive_ASTNode)]
#[derive(Debug, Clone)]
pub struct TypeName(pub(crate) String);

impl TypeName {
	pub fn from_parts<'a>(parts: impl Iterator<Item = &'a str>) -> Self {
		let mut buf = String::new();
		for part in parts {
			if !buf.is_empty() {
				buf.push('.');
			}
			buf.push_str(part);
		}
		TypeName(buf)
	}

	pub fn is_namespace_reference(&self) -> bool {
		self.0.contains('.')
	}

	pub fn parts(&self) -> impl Iterator<Item = &str> + '_ {
		self.0.split('.')
	}

	pub fn raw(&self) -> &str {
		&self.0
	}

	pub fn from_raw(on: String) -> Self {
		Self(on)
	}

	pub(crate) fn to_string_from_buffer<T: source_map::ToString>(&self, buf: &mut T) {
		buf.push_str(&self.0);
	}
}

#[apply(derive_ASTNode)]
#[derive(Debug, Clone)]
pub enum IsItem {
	Reference(String),
	This,
}

impl ASTNode for TypeAnnotation {
	fn get_position(&self) -> Span {
		*get_field_by_type::GetFieldByType::get(self)
	}

	fn from_reader(reader: &mut crate::Lexer) -> ParseResult<Self> {
		Self::from_reader_with_precedence(reader, TypeOperatorKind::None)
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
			Self::CommonName(name, _) => buf.push_str(name.name()),
			Self::Decorated(decorator, on_type_annotation, _) => {
				decorator.to_string_from_buffer(buf, options, local);
				buf.push(' ');
				on_type_annotation.to_string_from_buffer(buf, options, local);
			}
			Self::Name(name, _) => name.to_string_from_buffer(buf),
			Self::NameWithGenericArguments(name, arguments, _) => {
				name.to_string_from_buffer(buf);
				bracketed_items_to_string(arguments, ('<', '>'), buf, options, local);
			}
			#[cfg(feature = "extras")]
			Self::NameWithProperties(name, properties, _) => {
				name.to_string_from_buffer(buf);
				bracketed_items_to_string(properties, ('{', '}'), buf, options, local);
			}
			Self::FunctionLiteral { type_parameters, parameters, return_type, .. } => {
				if let Some(type_parameters) = type_parameters {
					bracketed_items_to_string(type_parameters, ('<', '>'), buf, options, local);
				}
				parameters.to_string_from_buffer(buf, options, local);
				buf.push_str(" => ");
				return_type.to_string_from_buffer(buf, options, local);
			}
			Self::BooleanLiteral(true, _) => buf.push_str("true"),
			Self::BooleanLiteral(false, _) => buf.push_str("false"),
			Self::This(..) => buf.push_str("this"),
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
			Self::Infer { name, extends, position: _ } => {
				buf.push_str("infer ");
				buf.push_str(name.as_str());
				if let Some(ref extends) = extends {
					buf.push_str(" extends ");
					extends.to_string_from_buffer(buf, options, local);
				}
			}
			Self::ObjectLiteral(members, _) => {
				bracketed_items_to_string(members, ('{', '}'), buf, options, local);
			}
			Self::TupleLiteral(members, _) => {
				bracketed_items_to_string(members, ('[', ']'), buf, options, local);
			}
			Self::Index(on, with, _) => {
				on.to_string_from_buffer(buf, options, local);
				buf.push('[');
				with.to_string_from_buffer(buf, options, local);
				buf.push(']');
			}
			Self::Abstract(item, _) => {
				buf.push_str("abstract ");
				item.to_string_from_buffer(buf, options, local);
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
					bracketed_items_to_string(type_parameters, ('<', '>'), buf, options, local);
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
			Self::TemplateLiteral { parts, final_part, .. } => {
				buf.push('`');
				for (static_part, dynamic_part) in parts {
					buf.push_str_contains_new_line(static_part.as_str());

					buf.push_str("${");
					dynamic_part.to_string_from_buffer(buf, options, local);
					buf.push('}');
				}
				buf.push_str_contains_new_line(final_part.as_str());
				buf.push('`');
			}
			Self::Symbol { unique, .. } => {
				if *unique {
					buf.push_str("unique ");
				}
				buf.push_str("symbol");
			}
			Self::Extends { item, extends, .. } => {
				item.to_string_from_buffer(buf, options, local);
				buf.push_str(" extends ");
				extends.to_string_from_buffer(buf, options, local);
			}
			Self::Is { reference, is, .. } => {
				buf.push_str(match reference {
					IsItem::Reference(reference) => reference,
					IsItem::This => "this",
				});
				buf.push_str(" is ");
				is.to_string_from_buffer(buf, options, local);
			}
			Self::Asserts(predicate, _pos) => {
				buf.push_str("asserts ");
				predicate.to_string_from_buffer(buf, options, local);
			}
		}
	}
}

/// For parsing. Precedence level. WIP
#[derive(Clone, Copy)]
pub(crate) enum TypeOperatorKind {
	None,
	Intersection,
	// not an implication, not an implication, not an implication
	Function,
	Query,
	/// Break before `=>`
	ReturnType,
}

const COMMON_TYPE_NAMES: &[&str] =
	&["string", "number", "boolean", "any", "null", "undefined", "unknown", "never"];

impl TypeAnnotation {
	/// Also returns the local the generic arguments ran over
	/// TODO refactor and tidy a lot of this, precedence rather than config
	pub(crate) fn from_reader_with_precedence(
		reader: &mut crate::Lexer,
		parent_kind: TypeOperatorKind,
	) -> ParseResult<Self> {
		if reader.get_options().partial_syntax {
			let start = reader.get_start();
			reader.skip();
			let next_is_not_expression_like = reader.starts_with_expression_delimiter()
				|| reader.starts_with_statement_or_declaration_on_new_line();

			if next_is_not_expression_like {
				// take up the whole next part for checker suggestions
				let position = start.union(reader.get_end());
				return Ok(TypeAnnotation::Marker(
					reader.new_partial_point_marker(position),
					position,
				));
			}
		} else {
			reader.skip();
		}

		// while let Some(Token(TSXToken::Comment(_) | TSXToken::MultiLineComment(_), _)) =
		// 	reader.peek()
		// {
		// 	reader.next();
		// }

		// Yes leading syntax is allowed sometimes
		if let TypeOperatorKind::None = parent_kind {
			let _ = reader.is_operator_advance("|") || reader.is_operator_advance("&");
		}

		reader.skip();

		let start = reader.get_start();

		let mut reference = if reader.starts_with_number() {
			let (value, length) = reader.parse_number_literal()?;
			let position = start.with_length(length as usize);
			Self::NumberLiteral(value, position)
		} else if reader.is_keyword_advance("this") {
			TypeAnnotation::This(start.with_length(4))
		} else if reader.is_keyword_advance("true") {
			TypeAnnotation::BooleanLiteral(true, start.with_length(4))
		} else if reader.is_keyword_advance("false") {
			TypeAnnotation::BooleanLiteral(false, start.with_length(5))
		} else if reader.is_keyword_advance("infer") {
			let name = reader.parse_identifier("infer name", false)?;
			let (position, extends) = if reader.is_keyword_advance("extends") {
				let extends =
					TypeAnnotation::from_reader_with_precedence(reader, TypeOperatorKind::Query)?;
				(start.union(extends.get_position()), Some(Box::new(extends)))
			} else {
				let position = start.with_length(name.len());
				(position, None)
			};
			TypeAnnotation::Infer { name: name.to_owned(), extends, position }
		} else if reader.is_keyword_advance("asserts") {
			let predicate = TypeAnnotation::from_reader_with_precedence(reader, parent_kind)?;
			let position = start.union(predicate.get_position());
			TypeAnnotation::Asserts(Box::new(predicate), position)
		} else if reader.is_keyword_advance("typeof") {
			let reference = VariableOrPropertyAccess::from_reader(reader)?;
			let position = start.union(reference.get_position());
			Self::TypeOf(Box::new(reference), position)
		} else if reader.is_keyword_advance("readonly") {
			let readonly_type =
				TypeAnnotation::from_reader_with_precedence(reader, TypeOperatorKind::Query)?;
			let position = start.union(readonly_type.get_position());
			TypeAnnotation::Readonly(Box::new(readonly_type), position)
		} else if reader.is_keyword_advance("keyof") {
			let key_of_type =
				TypeAnnotation::from_reader_with_precedence(reader, TypeOperatorKind::Query)?;
			let position = start.union(key_of_type.get_position());
			TypeAnnotation::KeyOf(Box::new(key_of_type), position)
		} else if reader.is_keyword_advance("abstract") {
			let inner_type =
				TypeAnnotation::from_reader_with_precedence(reader, TypeOperatorKind::Query)?;
			let position = start.union(inner_type.get_position());
			TypeAnnotation::Abstract(Box::new(inner_type), position)
		} else if reader.is_keyword_advance("new") {
			let type_parameters = if reader.starts_with('<') {
				let (type_parameters, _) = bracketed_items_from_reader(reader, ">")?;
				Some(type_parameters)
			} else {
				None
			};
			let parameters = TypeAnnotationFunctionParameters::from_reader(reader)?;
			reader.expect_operator("=>")?;
			let return_type = Box::new(TypeAnnotation::from_reader(reader)?);
			let position = start.union(return_type.get_position());
			TypeAnnotation::ConstructorLiteral {
				position,
				parameters: Box::new(parameters),
				type_parameters,
				return_type,
			}
		} else if let Some(keyword) = reader.is_one_of_keywords_advance(COMMON_TYPE_NAMES) {
			let name = match keyword {
				"string" => CommonTypes::String,
				"number" => CommonTypes::Number,
				"boolean" => CommonTypes::Boolean,
				"any" => CommonTypes::Any,
				"null" => CommonTypes::Null,
				"undefined" => CommonTypes::Undefined,
				"unknown" => CommonTypes::Unknown,
				"never" => CommonTypes::Never,
				slice => unreachable!("{slice:?}"),
			};
			Self::CommonName(name, start.with_length(keyword.len()))
		} else if reader.is_keyword_advance("unique") {
			reader.expect_keyword("symbol")?;
			reader.skip();
			#[cfg(feature = "extras")]
			let name = if reader.get_options().extra_type_annotations
				&& reader.starts_with_string_delimeter()
			{
				Some(reader.parse_string_literal()?.0.into_owned())
			} else {
				None
			};
			Self::Symbol {
				unique: true,
				position: start.union(reader.get_end()),
				#[cfg(feature = "extras")]
				name,
			}
		} else if reader.is_operator_advance("-") {
			let (value, length) = reader.parse_number_literal()?;
			let position = start.with_length(length as usize);
			// important negation here
			Self::NumberLiteral(value.neg(), position)
		} else if reader.starts_with('"') || reader.starts_with('\'') {
			let (content, quoted) = reader.parse_string_literal()?;
			let position = start.with_length(content.len() + 2);
			Self::StringLiteral(content.into_owned(), quoted, position)
		} else if reader.starts_with('@') {
			let decorator = Decorator::from_reader(reader)?;
			// TODO ...
			let this_declaration =
				Self::from_reader_with_precedence(reader, TypeOperatorKind::Query)?;
			let position = start.union(this_declaration.get_position());
			Self::Decorated(Box::new(decorator), Box::new(this_declaration), position)
		} else if reader.is_operator("(") {
			// Function literal or group
			let after = reader.after_brackets();
			let is_arrow_function = after.starts_with("=>");

			if is_arrow_function {
				let parameters = TypeAnnotationFunctionParameters::from_reader(reader)?;
				reader.expect_operator("=>")?;
				let return_type = Self::from_reader(reader)?;
				Self::FunctionLiteral {
					position: start.union(return_type.get_position()),
					type_parameters: None,
					parameters: Box::new(parameters),
					return_type: Box::new(return_type),
				}
			} else {
				reader.advance(1);
				let type_annotation = Self::from_reader(reader)?;
				let position = start.union(reader.expect(')')?);
				Self::ParenthesizedReference(type_annotation.into(), position)
			}
		} else if reader.is_operator_advance("<") {
			let (type_parameters, _) = bracketed_items_from_reader(reader, ">")?;
			let parameters = TypeAnnotationFunctionParameters::from_reader(reader)?;
			reader.expect_operator("=>")?;
			let return_type = Self::from_reader(reader)?;
			Self::FunctionLiteral {
				position: start.union(return_type.get_position()),
				type_parameters: Some(type_parameters),
				parameters: Box::new(parameters),
				return_type: Box::new(return_type),
			}
		} else if reader.is_operator_advance("{") {
			let members = crate::types::interface::interface_members_from_reader(reader)?;
			let position = start.union(reader.expect('}')?);
			Self::ObjectLiteral(members, position)
		} else if reader.is_operator_advance("[") {
			let (members, _) = bracketed_items_from_reader(reader, "]")?;
			let position = start.union(reader.get_end());
			Self::TupleLiteral(members, position)
		} else if reader.is_operator_advance("`") {
			let start = reader.get_start();
			let mut parts = Vec::new();
			let result;
			loop {
				let (content, found) = reader.parse_until_one_of(&["${", "`"]).map_err(|()| {
					let (_found, position) = crate::lexer::utilities::next_item(reader);
					ParseError::new(ParseErrors::UnexpectedEnd, position)
				})?;
				reader.advance(found.len() as u32);
				if let "${" = found {
					let expression = AnnotationWithBinder::from_reader(reader)?;
					reader.expect('}')?;
					parts.push((content.to_owned(), expression));
				} else {
					result = Self::TemplateLiteral {
						parts,
						final_part: content.to_owned(),
						position: start.union(reader.get_end()),
					};
					break;
				}
			}
			result
		} else {
			let name = reader.parse_identifier("type name", false)?;
			let position = start.with_length(name.len());
			let name = name.to_owned();

			let name = TypeName::from_raw(name);

			// Generics arguments:
			if reader.is_operator_advance("<") {
				let (generic_arguments, _) = bracketed_items_from_reader(reader, ">")?;
				let end = reader.get_end();
				Self::NameWithGenericArguments(name, generic_arguments, start.union(end))
			} else {
				#[cfg(feature = "extras")]
				if reader.get_options().extra_type_annotations && reader.is_operator_advance("{") {
					let (binders, _) = bracketed_items_from_reader(reader, "}")?;
					let end = reader.get_end();
					Self::NameWithProperties(name, binders, start.union(end))
				} else {
					Self::Name(name, position)
				}

				#[cfg(not(feature = "extras"))]
				Self::Name(name, position)
			}
		};

		// Array shorthand & indexing type references. Loops as number[][]
		// unsure if index type can be looped
		while reader.is_operator_advance("[") {
			let start = reference.get_position();
			if reader.is_operator_advance("]") {
				let position = start.union(reader.get_end());
				reference = Self::ArrayLiteral(Box::new(reference), position);
			} else {
				// E.g type allTypes = Person[keyof Person];
				let indexer = TypeAnnotation::from_reader(reader)?;
				let end = reader.expect(']')?;
				let position = start.union(end);
				reference = Self::Index(Box::new(reference), Box::new(indexer), position);
			}
		}

		if reader.is_keyword_advance("is") {
			fn type_annotation_as_name(
				reference: TypeAnnotation,
			) -> Result<(IsItem, Span), TypeAnnotation> {
				match reference {
					TypeAnnotation::CommonName(name, span) => {
						Ok((IsItem::Reference(name.name().to_owned()), span))
					}
					TypeAnnotation::Name(name, span) if !name.is_namespace_reference() => {
						let name = name.parts().next().unwrap().to_owned();
						Ok((IsItem::Reference(name), span))
					}
					TypeAnnotation::This(span) => Ok((IsItem::This, span)),
					_ => Err(reference),
				}
			}

			match type_annotation_as_name(reference) {
				Ok((item, span)) => {
					let is_type = TypeAnnotation::from_reader_with_precedence(
						reader,
						TypeOperatorKind::Query,
					)?;
					// TODO local
					let position = span.union(is_type.get_position());

					reference =
						TypeAnnotation::Is { reference: item, is: Box::new(is_type), position };
				}
				Err(reference) => {
					return Err(ParseError::new(
						crate::ParseErrors::InvalidLHSOfIs,
						reference.get_position(),
					));
				}
			}
		}

		if reader.is_keyword("extends") {
			if let TypeOperatorKind::Query = parent_kind {
				return Ok(reference);
			}
			reader.advance("extends".len() as u32);
			let extends_type =
				TypeAnnotation::from_reader_with_precedence(reader, TypeOperatorKind::Query)?;
			let position = reference.get_position().union(extends_type.get_position());
			reference = TypeAnnotation::Extends {
				item: Box::new(reference),
				extends: Box::new(extends_type),
				position,
			};
		}

		// Fix for `as` and `satisfies` expressions
		if reader.is_one_of(&["||", "&&"]).is_some() {
			return Ok(reference);
		}

		// Intersections, unions, conditonals and (special) implicit function literals
		while let Some(operator) = reader.is_one_of_operators(&["|", "&"]) {
			if let TypeOperatorKind::Function = parent_kind {
				return Ok(reference);
			} else if let ("|", TypeOperatorKind::Intersection) = (operator, parent_kind) {
				return Ok(reference);
			}
			reader.advance(1);
			if let "&" = operator {
				let precedence = TypeOperatorKind::Intersection;
				let rhs = Self::from_reader_with_precedence(reader, precedence)?;
				let position = reference.get_position().union(rhs.get_position());
				if let TypeAnnotation::Intersection(members, existing) = &mut reference {
					*existing = position;
					members.push(rhs);
				} else {
					reference = TypeAnnotation::Intersection(vec![reference, rhs], position);
				}
			} else if let "|" = operator {
				let rhs = Self::from_reader_with_precedence(reader, parent_kind)?;
				let position = reference.get_position().union(rhs.get_position());
				if let TypeAnnotation::Union(members, existing) = &mut reference {
					*existing = position;
					members.push(rhs);
				} else {
					reference = TypeAnnotation::Union(vec![reference, rhs], position);
				}
			} else {
				unreachable!("{operator}")
			}
		}

		// TODO is this worthwhile syntax or a good feature?
		if reader.is_operator("=>") && reader.get_options().extra_type_annotations {
			if let TypeOperatorKind::Query
			| TypeOperatorKind::Function
			| TypeOperatorKind::ReturnType
			| TypeOperatorKind::Intersection = parent_kind
			{
				return Ok(reference);
			}
			reader.advance(2);
			let return_type =
				Self::from_reader_with_precedence(reader, TypeOperatorKind::Function)?;
			let parameters_position = reference.get_position();
			let position = parameters_position.union(return_type.get_position());
			Ok(Self::FunctionLiteral {
				position,
				type_parameters: None,
				parameters: Box::new(TypeAnnotationFunctionParameters {
					parameters: vec![TypeAnnotationFunctionParameter {
						position,
						name: None,
						type_annotation: reference,
						is_optional: false,
						decorators: Default::default(),
					}],
					rest_parameter: None,
					position: parameters_position,
				}),
				return_type: Box::new(return_type),
			})
		} else if reader.is_operator("?") {
			if let TypeOperatorKind::Query = parent_kind {
				return Ok(reference);
			}
			reader.advance(1);
			let lhs = TypeAnnotation::from_reader(reader)?;
			reader.expect(':')?;
			let rhs = TypeAnnotation::from_reader(reader)?;
			let position = reference.get_position().union(rhs.get_position());
			Ok(TypeAnnotation::Conditional {
				condition: Box::new(reference),
				resolve_true: Box::new(lhs),
				resolve_false: Box::new(rhs),
				position,
			})
		} else {
			Ok(reference)
		}
	}
}

/// Mirrors [`crate::FunctionParameters`]
#[derive(Debug, Clone)]
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

	fn from_reader(reader: &mut crate::Lexer) -> ParseResult<Self> {
		let start = reader.expect_start('(')?;
		let mut parameters = Vec::new();
		let mut rest_parameter = None;

		loop {
			reader.skip();
			if reader.is_operator(")") {
				break;
			}

			let start = reader.get_start();

			if reader.is_operator_advance("...") {
				let name = reader.parse_identifier("spread parameter name", true)?.to_owned();
				// // TODO is this a good feature
				// let name = if reader.after_identifier().starts_with(":") {
				// 	Some(WithComment::<VariableField>::from_reader(reader)?)
				// } else {
				// 	None
				// };

				let _ = reader.expect(':');
				let type_annotation = TypeAnnotation::from_reader(reader)?;

				let position = start.union(type_annotation.get_position());

				rest_parameter = Some(Box::new(TypeAnnotationSpreadFunctionParameter {
					// TODO
					decorators: Default::default(),
					name,
					type_annotation,
					position,
				}));
				break;
			}

			// TODO is this a good feature
			let after_identifier = reader.after_identifier();
			let name = if after_identifier.starts_with(':') || after_identifier.starts_with("?:") {
				Some(WithComment::<VariableField>::from_reader(reader)?)
			} else {
				None
			};

			let is_optional = if name.is_some() {
				if reader.is_operator_advance("?:") {
					true
				} else if reader.is_operator_advance(":") {
					false
				} else {
					return Err(crate::lexer::utilities::expected_one_of_items(
						reader,
						&["?:", ":"],
					));
				}
			} else {
				false
			};
			let type_annotation = TypeAnnotation::from_reader(reader)?;
			let position = start.union(type_annotation.get_position());

			parameters.push(TypeAnnotationFunctionParameter {
				// TODO
				decorators: Default::default(),
				name,
				type_annotation,
				is_optional,
				position,
			});

			if !reader.is_operator_advance(",") {
				break;
			}
		}
		let close = reader.expect(')')?;
		let position = start.union(close);
		Ok(TypeAnnotationFunctionParameters { parameters, rest_parameter, position })
	}

	fn to_string_from_buffer<T: source_map::ToString>(
		&self,
		buf: &mut T,
		options: &crate::ToStringOptions,
		local: crate::LocalToStringInformation,
	) {
		buf.push('(');
		for (at_end, parameter) in self.parameters.iter().endiate() {
			if let Some(ref name) = parameter.name {
				name.to_string_from_buffer(buf, options, local);
			}
			if parameter.is_optional {
				buf.push_str("?: ");
			} else {
				buf.push_str(": ");
			}
			parameter.type_annotation.to_string_from_buffer(buf, options, local);
			if !at_end || self.rest_parameter.is_some() {
				buf.push(',');
				options.push_gap_optionally(buf);
			}
		}
		if let Some(ref rest_parameter) = self.rest_parameter {
			buf.push_str("...");
			buf.push_str(&rest_parameter.name);
			rest_parameter.type_annotation.to_string_from_buffer(buf, options, local);
		}
		buf.push(')');
	}
}

#[derive(Debug, Clone)]
#[apply(derive_ASTNode)]
pub struct TypeAnnotationFunctionParameter {
	pub decorators: Vec<Decorator>,
	/// Ooh nice optional
	pub name: Option<WithComment<VariableField>>,
	pub type_annotation: TypeAnnotation,
	pub is_optional: bool,
	pub position: Span,
}

#[derive(Debug, Clone)]
#[apply(derive_ASTNode)]
pub struct TypeAnnotationSpreadFunctionParameter {
	pub decorators: Vec<Decorator>,
	pub name: String,
	pub type_annotation: TypeAnnotation,
	pub position: Span,
}

#[derive(Debug, Clone)]
#[apply(derive_ASTNode)]
pub struct TupleLiteralElement(pub TupleElementKind, pub AnnotationWithBinder, pub Span);

impl ASTNode for TupleLiteralElement {
	fn get_position(&self) -> Span {
		self.2
	}

	fn from_reader(reader: &mut crate::Lexer) -> ParseResult<Self> {
		let start = reader.get_start();
		let is_spread = reader.is_operator_advance("...");

		let annotation_with_binder = AnnotationWithBinder::from_reader(reader)?;

		let (kind, position) = if is_spread {
			(TupleElementKind::Spread, start.union(annotation_with_binder.get_position()))
		} else if reader.is_operator_advance("?") {
			(
				TupleElementKind::Optional,
				annotation_with_binder.get_position().union(reader.get_end()),
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
	type LAST = ();
}

impl ListItem for BinderWithAnnotation {
	type LAST = ();
}
