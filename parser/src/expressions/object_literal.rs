use crate::functions::{
	FunctionBased, FunctionKind, HeadingAndPosition, MethodHeader, ThisParameter,
};
use crate::{
	ASTNode, Block, Expression, FunctionBase, ParseResult, Span, derive_ASTNode,
	visiting::Visitable,
};

pub use crate::property_key::{AlwaysPublic, PropertyKey};

use std::fmt::Debug;
use visitable_derive::Visitable;

#[apply(derive_ASTNode)]
#[derive(Debug, Clone, Visitable, get_field_by_type::GetFieldByType)]
#[get_field_by_type_target(Span)]
pub struct ObjectLiteral {
	pub members: Vec<ObjectLiteralMember>,
	pub position: Span,
}

/// Only standard are supported but for simplicity allow all here
#[apply(derive_ASTNode)]
#[derive(Debug, Clone, Visitable, get_field_by_type::GetFieldByType)]
#[get_field_by_type_target(Span)]
pub struct ShorthandKey(pub PropertyKey<AlwaysPublic>);

#[apply(derive_ASTNode)]
#[derive(Debug, Clone, get_field_by_type::GetFieldByType, Visitable)]
#[get_field_by_type_target(Span)]
pub enum ObjectLiteralMember {
	Spread(Expression, Span),
	Shorthand(ShorthandKey),
	Property {
		key: PropertyKey<AlwaysPublic>,
		/// Makes object destructuring syntax a subset of object literal syntax
		assignment: bool,
		value: Expression,
		position: Span,
	},
	Method(Box<ObjectLiteralMethod>),
	Comment(String, bool, Span),
}

impl ObjectLiteralMember {
	pub fn get_key(&self) -> Option<&PropertyKey<AlwaysPublic>> {
		match self {
			Self::Property { key, .. } => Some(key),
			Self::Method(method) => Some(&method.name),
			Self::Shorthand(key) => Some(&key.0),
			_ => None,
		}
	}
}

#[derive(Debug, Clone, Hash)]
pub struct ObjectLiteralMethodBase;
pub type ObjectLiteralMethod = FunctionBase<ObjectLiteralMethodBase>;

#[cfg_attr(target_family = "wasm", wasm_bindgen::prelude::wasm_bindgen(typescript_custom_section))]
#[allow(dead_code)]
const OBJECT_LITERAL_METHOD_TYPE: &str = r"
	export interface ObjectLiteralMethod extends FunctionBase {
		header: MethodHeader,
		body: Block,
		name: PropertyKey<AlwaysPublic>,
		parameters: FunctionParameters<ThisParameter | null, null>
	}
";

impl FunctionBased for ObjectLiteralMethodBase {
	type Name = PropertyKey<AlwaysPublic>;
	type Header = MethodHeader;
	type Body = Block;
	type LeadingParameter = Option<ThisParameter>;
	type ParameterVisibility = ();

	fn kind() -> FunctionKind {
		FunctionKind::Method
	}

	fn header_and_name_from_reader(
		_reader: &mut crate::Lexer,
	) -> ParseResult<(HeadingAndPosition<Self>, Self::Name)> {
		todo!()
		// // TODO not great
		// let start = reader.peek().unwrap().1;
		// Ok((
		// 	(Some(start), MethodHeader::from_reader(reader)?),
		// ))
	}

	fn header_and_name_to_string_from_buffer<T: source_map::ToString>(
		buf: &mut T,
		header: &Self::Header,
		name: &Self::Name,
		options: &crate::ToStringOptions,
		local: crate::LocalToStringInformation,
	) {
		header.to_string_from_buffer(buf);
		name.to_string_from_buffer(buf, options, local);
	}

	fn visit_name<TData>(
		name: &Self::Name,
		visitors: &mut (impl crate::VisitorReceiver<TData> + ?Sized),
		data: &mut TData,
		options: &crate::visiting::VisitOptions,
		chain: &mut temporary_annex::Annex<crate::Chain>,
	) {
		name.visit(visitors, data, options, chain);
	}

	fn visit_name_mut<TData>(
		name: &mut Self::Name,
		visitors: &mut (impl crate::VisitorMutReceiver<TData> + ?Sized),
		data: &mut TData,
		options: &crate::visiting::VisitOptions,
		chain: &mut temporary_annex::Annex<crate::Chain>,
	) {
		name.visit_mut(visitors, data, options, chain);
	}

	fn get_name(name: &Self::Name) -> Option<&str> {
		if let PropertyKey::Identifier(name, ..) = name { Some(name.as_str()) } else { None }
	}
}

impl ASTNode for ObjectLiteral {
	fn get_position(&self) -> Span {
		self.position
	}

	fn from_reader(reader: &mut crate::Lexer) -> ParseResult<Self> {
		let start = reader.get_start();
		reader.expect_chr('{')?;
		let mut members: Vec<ObjectLiteralMember> = Vec::new();
		loop {
			if reader.is_operator("}") {
				break;
			}
			let member = ObjectLiteralMember::from_reader(reader)?;
			let is_comment = matches!(member, ObjectLiteralMember::Comment(..));

			// if let Some(key) = member.get_key()
			// 	&& reader.get_options().features.run_validation
			// {
			// 	for existing_member in &members {
			// 		if let Some(existing_key) = existing_member.get_key()
			// 			&& key.definitionally_equal(existing_key)
			// 		{
			// 			return Err(crate::ParseError::new(
			// 				crate::ParseErrors::TODO("duplicate object literal key"),
			// 				member.get_position(),
			// 			));
			// 		}
			// 	}
			// }

			members.push(member);

			if is_comment || reader.starts_with_slice("/*") || reader.starts_with_slice("//") {
				continue;
			}
			if !reader.is_operator_advance(",") {
				break;
			}
		}
		let end = reader.expect_chr('}')?;
		Ok(ObjectLiteral { members, position: start.union(end) })
	}

	fn to_string_from_buffer<T: source_map::ToString>(
		&self,
		buf: &mut T,
		options: &crate::ToStringOptions,
		local: crate::LocalToStringInformation,
	) {
		crate::bracketed_items_to_string(&self.members, ('{', '}'), buf, options, local);
	}
}

impl ASTNode for ObjectLiteralMember {
	fn get_position(&self) -> Span {
		*get_field_by_type::GetFieldByType::get(self)
	}

	#[allow(clippy::similar_names)]
	fn from_reader(reader: &mut crate::Lexer) -> ParseResult<Self> {
		let start = reader.get_start();

		if reader.starts_with_slice("//") || reader.starts_with_slice("/*") {
			let is_multiline = reader.starts_with_slice("/*");
			reader.advance(2);
			let content = reader.parse_comment_literal(is_multiline)?.to_owned();
			let position = if is_multiline {
				start.with_length(2 + content.len())
			} else {
				start.with_length(4 + content.len())
			};
			return Ok(Self::Comment(content.clone(), false, position));
		}

		if reader.is_operator_advance("...") {
			// TODO precedence okay?
			let expression = Expression::from_reader(reader)?;
			let position = start.union(expression.get_position());
			return Ok(Self::Spread(expression, position));
		}

		let mut header = MethodHeader::from_reader(reader)?;
		let key = if reader.get_current().starts_with(['<', '(', ':', '}', ',']) {
			if let Ok(name) = header.into_property_key() {
				let position = start.with_length(name.len());
				let privacy = crate::property_key::AlwaysPublic;
				crate::property_key::PropertyKey::Identifier(name.to_owned(), position, privacy)
			} else {
				todo!("error")
			}
		} else {
			PropertyKey::<crate::property_key::AlwaysPublic>::from_reader(reader)?
		};

		if reader.get_current().starts_with(['(', '<']) {
			let method: ObjectLiteralMethod =
				FunctionBase::from_reader_with_header_and_name(reader, header, key)?;

			if let MethodHeader::Get = method.header
				&& !method.parameters.is_empty()
			{
				return Err(crate::ParseError::new(
					crate::ParseErrors::TODO("get cannot have parameters"),
					method.parameters.get_position(),
				));
			}
			if let MethodHeader::Set = method.header
				&& !method.parameters.is_single()
			{
				return Err(crate::ParseError::new(
					crate::ParseErrors::TODO("set can only have 1 parameter"),
					method.parameters.get_position(),
				));
			}

			Ok(Self::Method(Box::new(method)))
		} else if header.is_no_modifiers() {
			if reader.get_current().starts_with([',', '}']) {
				// if let PropertyKey::Identifier(name, position, _) = key {
				// 	if crate::lexer::utilities::is_reserved_word(&name, reader.strict_mode()) {
				// 		return Err(crate::ParseError::new(
				// 			crate::ParseErrors::ReservedIdentifier,
				// 			position,
				// 		));
				// 	}

				// } else {
				// 	let found = reader.get_current().chars().next();
				// 	Err(crate::ParseError::new(
				// 		crate::ParseErrors::UnexpectedCharacter { expected: &[':'], found },
				// 		reader.get_start().with_length(1),
				// 	))
				// }
				Ok(Self::Shorthand(ShorthandKey(key)))
			} else {
				// FUTURE currently for `{ x = 2 } = {}` but means that `console.log({ x = 2 })`, is a false positive
				let assignment = if reader.is_operator_advance("=") {
					true
				} else {
					reader.expect_chr(':')?;
					false
				};
				let value = Expression::from_reader(reader)?;
				let position = key.get_position().union(value.get_position());
				Ok(Self::Property { assignment, key, value, position })
			}
		} else {
			let found = reader.get_current().chars().next();
			Err(crate::ParseError::new(
				crate::ParseErrors::UnexpectedCharacter { expected: &['}'], found },
				reader.get_start().with_length(1),
			))
		}
	}

	fn to_string_from_buffer<T: source_map::ToString>(
		&self,
		buf: &mut T,
		options: &crate::ToStringOptions,
		local: crate::LocalToStringInformation,
	) {
		match self {
			Self::Property { assignment: _, key, value, position: _ } => {
				key.to_string_from_buffer(buf, options, local);
				buf.push(':');
				options.push_gap_optionally(buf);
				value.to_string_from_buffer(buf, options, local);
			}
			Self::Shorthand(name) => {
				name.0.to_string_from_buffer(buf, options, local);
			}
			Self::Method(func) => {
				func.to_string_from_buffer(buf, options, local);
			}
			Self::Spread(spread_expr, _) => {
				buf.push_str("...");
				spread_expr.to_string_from_buffer(buf, options, local);
			}
			Self::Comment(content, is_multiline, _) => {
				if options.should_add_comment(content) {
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
}

// impl crate::Visitable for ObjectLiteralMember {
// 	fn visit<TData>(
// 		&self,
// 		visitors: &mut (impl crate::VisitorReceiver<TData> + ?Sized),
// 		data: &mut TData,
// 		options: &crate::VisitOptions,
// 		chain: &mut temporary_annex::Annex<crate::Chain>,
// 	) {
// 		match self {
// 			ObjectLiteralMember::Shorthand(..)
// 			| ObjectLiteralMember::Property { .. }
// 			| ObjectLiteralMember::Spread(value) => value.
// 			| ObjectLiteralMember::Comment(..) => {}
// 			ObjectLiteralMember::Method(method) => method.visit(visitors, data, options, chain),
// 		}
// 	}

// 	fn visit_mut<TData>(
// 		&mut self,
// 		visitors: &mut (impl crate::VisitorMutReceiver<TData> + ?Sized),
// 		data: &mut TData,
// 		options: &crate::VisitOptions,
// 		chain: &mut temporary_annex::Annex<crate::Chain>,
// 	) {
// 		match self {
// 			ObjectLiteralMember::Shorthand(..)
// 			| ObjectLiteralMember::Property { .. }
// 			| ObjectLiteralMember::Spread(..)
// 			| ObjectLiteralMember::Comment(..) => {}
// 			ObjectLiteralMember::Method(method) => method.visit_mut(visitors, data, options, chain),
// 		}
// 	}
// }
