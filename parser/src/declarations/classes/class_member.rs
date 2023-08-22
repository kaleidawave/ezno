use std::{borrow::Cow, fmt::Debug};

use crate::tsx_keywords;
use source_map::Span;
use tokenizer_lib::{Token, TokenReader};

use crate::{
	functions::FunctionBased, ASTNode, Block, Expression, FunctionBase, GetSetGeneratorOrNone,
	Keyword, ParseError, ParseErrors, ParseOptions, ParseResult, PropertyKey, TSXKeyword, TSXToken,
	TypeAnnotation, VisitSettings, WithComment,
};

/// The variable id's of these is handled by their [PropertyKey]
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ClassMember {
	Constructor(ClassConstructor),
	Function(Option<Keyword<tsx_keywords::Static>>, ClassFunction),
	Property(Option<Keyword<tsx_keywords::Static>>, ClassProperty),
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct ClassConstructorBase;
pub type ClassConstructor = FunctionBase<ClassConstructorBase>;

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct ClassFunctionBase;
pub type ClassFunction = FunctionBase<ClassFunctionBase>;

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ClassProperty {
	pub key: WithComment<PropertyKey>,
	pub type_annotation: Option<TypeAnnotation>,
	pub value: Option<Box<Expression>>,
}

impl ASTNode for ClassMember {
	fn get_position(&self) -> Cow<Span> {
		todo!()
		// match self {
		// 	Self::Constructor { position, .. }
		// 	| Self::Method { position, .. }
		// 	| Self::Property { position, .. } => position.as_ref(),
		// }
	}

	fn from_reader(
		reader: &mut impl TokenReader<TSXToken, Span>,
		state: &mut crate::ParsingState,
		settings: &ParseOptions,
	) -> ParseResult<Self> {
		if let Some(Token(TSXToken::Keyword(TSXKeyword::Constructor), _)) = reader.peek() {
			let constructor = ClassConstructor::from_reader(reader, state, settings)?;
			return Ok(ClassMember::Constructor(constructor));
		}

		let is_static = reader
			.conditional_next(|tok| *tok == TSXToken::Keyword(TSXKeyword::Static))
			.map(|Token(_, span)| Keyword::new(span));
		let is_async = reader
			.conditional_next(|tok| *tok == TSXToken::Keyword(TSXKeyword::Async))
			.map(|Token(_, span)| Keyword::new(span));
		let get_set_generator_or_none = GetSetGeneratorOrNone::from_reader(reader);

		let key = WithComment::<PropertyKey>::from_reader(reader, state, settings)?;

		match reader.peek().unwrap() {
			Token(TSXToken::OpenParentheses, _) => {
				let function = ClassFunction::from_reader_with_config(
					reader,
					state,
					settings,
					is_async,
					get_set_generator_or_none,
					key,
				)?;
				Ok(ClassMember::Function(is_static, function))
			}
			Token(token, _) => {
				if get_set_generator_or_none != GetSetGeneratorOrNone::None {
					let Token(token, position) = reader.next().unwrap();
					return Err(ParseError::new(
						ParseErrors::UnexpectedToken {
							expected: &[TSXToken::OpenParentheses],
							found: token,
						},
						position,
					));
				}
				let member_type: Option<TypeAnnotation> = match token {
					TSXToken::Colon => {
						reader.next();
						let type_annotation = TypeAnnotation::from_reader(reader, state, settings)?;
						Some(type_annotation)
					}
					_ => None,
				};
				let member_expression: Option<Expression> = match reader.peek() {
					Some(Token(TSXToken::Assign, _)) => {
						reader.next();
						let expression = Expression::from_reader(reader, state, settings)?;
						Some(expression)
					}
					_ => None,
				};
				Ok(Self::Property(
					is_static,
					ClassProperty {
						key,
						type_annotation: member_type,
						value: member_expression.map(Box::new),
					},
				))
			}
		}
	}

	fn to_string_from_buffer<T: source_map::ToString>(
		&self,
		buf: &mut T,
		settings: &crate::ToStringOptions,
		depth: u8,
	) {
		match self {
			Self::Property(is_static, ClassProperty { key, type_annotation, value }) => {
				if is_static.is_some() {
					buf.push_str("static ");
				}
				key.to_string_from_buffer(buf, settings, depth);
				if let (true, Some(type_annotation)) = (settings.include_types, type_annotation) {
					buf.push_str(": ");
					type_annotation.to_string_from_buffer(buf, settings, depth);
				}
				if let Some(value) = value {
					buf.push_str(if settings.pretty { " = " } else { "=" });
					value.to_string_from_buffer(buf, settings, depth);
				}
			}
			Self::Function(is_static, function) => {
				if is_static.is_some() {
					buf.push_str("static ");
				}
				function.to_string_from_buffer(buf, settings, depth + 1)
			}
			Self::Constructor(constructor) => {
				constructor.to_string_from_buffer(buf, settings, depth + 1)
			}
		}
	}
}

impl ClassMember {
	// pub fn get_property_id(&self) -> Option<PropertyId> {
	// 	match self {
	// 		ClassMember::Method(_, ClassMethod { key, .. })
	// 		| ClassMember::Property(_, ClassProperty { key, .. }) => Some(key.get_ast().get_property_id()),
	// 		ClassMember::Constructor { .. } => None,
	// 	}
	// }
}

#[allow(unused)]
impl ClassMember {
	pub fn visit<TData>(
		&self,
		visitors: &mut (impl crate::VisitorReceiver<TData> + ?Sized),
		data: &mut TData,
		settings: &VisitSettings,
		chain: &mut temporary_annex::Annex<crate::visiting::Chain>,
	) {
		match self {
			ClassMember::Constructor(..) => {
				todo!()
				// decorators.visit(visitors, data, settings, chain);
				// function.visit(
				// 	visitors,
				// 	data,
				// 	settings,
				// 	&mut chain.push_annex(ChainVariable::UnderClassMethod(
				// 		function.body.1,
				// 		class_variable_id,
				// 	)),
				// );
			}
			ClassMember::Function(..) => {
				todo!()
				//  { decorators, function, key, .. }
				// key.visit(visitors, data, chain, PropertyKeyLocation::Class(class_variable_id));
				// decorators.visit(visitors, data, settings, chain);
				// function.visit(
				// 	visitors,
				// 	data,
				// 	settings,
				// 	&mut chain.push_annex(ChainVariable::UnderClassMethod(
				// 		function.body.1,
				// 		class_variable_id,
				// 	)),
				// );
			}
			ClassMember::Property(_, ClassProperty { value, key, .. }) => {
				todo!()
				// key.visit(visitors, data, chain, PropertyKeyLocation::Class(class_variable_id));
				// value.visit(visitors, data, settings, chain);
			}
		}
	}

	pub fn visit_mut<TData>(
		&mut self,
		visitors: &mut (impl crate::VisitorMutReceiver<TData> + ?Sized),
		data: &mut TData,
		settings: &VisitSettings,
		chain: &mut temporary_annex::Annex<crate::visiting::Chain>,
	) {
		match self {
			ClassMember::Constructor(..) => {
				todo!()
				//  { decorators, function, .. }
				// decorators
				// 	.iter_mut()
				// 	.for_each(|decorator| decorator.visit_mut(visitors, data, settings, chain));
				// function.visit_mut(
				// 	visitors,
				// 	data,
				// 	settings,
				// 	&mut chain.push_annex(ChainVariable::UnderClassMethod(
				// 		function.body.1,
				// 		class_variable_id,
				// 	)),
				// );
			}
			ClassMember::Function(..) => {
				todo!()
				//  { decorators, function, key, .. }
				// key.visit_mut(visitors, data, chain, PropertyKeyLocation::Class(class_variable_id));
				// decorators
				// 	.iter_mut()
				// 	.for_each(|decorator| decorator.visit_mut(visitors, data, settings, chain));
				// function.visit_mut(
				// 	visitors,
				// 	data,
				// 	settings,
				// 	&mut chain.push_annex(ChainVariable::UnderClassMethod(
				// 		function.body.1,
				// 		class_variable_id,
				// 	)),
				// );
			}
			ClassMember::Property(_, ClassProperty { value, key, .. }) => {
				todo!()
				// key.visit_mut(visitors, data, chain, PropertyKeyLocation::Class(class_variable_id));
				// if let Some(value) = value {
				// 	value.visit_mut(visitors, data, settings, chain);
				// }
			}
		}
	}
}

impl ClassFunction {
	fn from_reader_with_config(
		reader: &mut impl TokenReader<TSXToken, Span>,
		state: &mut crate::ParsingState,
		settings: &ParseOptions,
		is_async: Option<Keyword<tsx_keywords::Async>>,
		get_set_generator_or_none: GetSetGeneratorOrNone,
		key: WithComment<PropertyKey>,
	) -> ParseResult<Self> {
		FunctionBase::from_reader_with_header_and_name(
			reader,
			state,
			settings,
			(is_async, get_set_generator_or_none),
			key,
		)
	}
}

impl FunctionBased for ClassFunctionBase {
	type Body = Block;
	type Header = (Option<Keyword<tsx_keywords::Async>>, GetSetGeneratorOrNone);
	type Name = WithComment<PropertyKey>;

	// fn get_chain_variable(this: &FunctionBase<Self>) -> ChainVariable {
	// 	ChainVariable::UnderClassMethod(this.body.1)
	// }

	fn header_and_name_from_reader(
		reader: &mut impl TokenReader<TSXToken, Span>,
		state: &mut crate::ParsingState,
		settings: &ParseOptions,
	) -> ParseResult<(Self::Header, Self::Name)> {
		let async_keyword = reader
			.conditional_next(|tok| matches!(tok, TSXToken::Keyword(TSXKeyword::Async)))
			.map(|Token(_, span)| Keyword::new(span));
		let header = GetSetGeneratorOrNone::from_reader(reader);
		let name = WithComment::<PropertyKey>::from_reader(reader, state, settings)?;
		Ok(((async_keyword, header), name))
	}

	fn header_and_name_to_string_from_buffer<T: source_map::ToString>(
		buf: &mut T,
		header: &Self::Header,
		name: &Self::Name,
		settings: &crate::ToStringOptions,
		depth: u8,
	) {
		if let Some(_header) = &header.0 {
			buf.push_str("async ");
		}
		header.1.to_string_from_buffer(buf);
		name.to_string_from_buffer(buf, settings, depth);
	}

	fn header_left(header: &Self::Header) -> Option<Cow<Span>> {
		header.0.as_ref().map(|async_kw| Cow::Borrowed(&async_kw.1)).xor(header.1.get_position())
	}
}

impl FunctionBased for ClassConstructorBase {
	type Body = Block;
	type Header = Keyword<tsx_keywords::Constructor>;
	type Name = ();

	// fn get_chain_variable(this: &FunctionBase<Self>) -> ChainVariable {
	// 	ChainVariable::UnderClassConstructor(this.body.1)
	// }

	fn header_and_name_from_reader(
		reader: &mut impl TokenReader<TSXToken, Span>,
		_state: &mut crate::ParsingState,
		_settings: &ParseOptions,
	) -> ParseResult<(Self::Header, Self::Name)> {
		let span = reader.expect_next(TSXToken::Keyword(TSXKeyword::Constructor))?;
		Ok((Keyword::new(span), ()))
	}

	fn header_and_name_to_string_from_buffer<T: source_map::ToString>(
		buf: &mut T,
		_header: &Self::Header,
		_name: &Self::Name,
		_settings: &crate::ToStringOptions,
		_depth: u8,
	) {
		buf.push_str("constructor")
	}

	fn header_left(header: &Self::Header) -> Option<Cow<Span>> {
		Some(Cow::Borrowed(&header.1))
	}
}
