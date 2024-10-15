use std::fmt::Debug;

use crate::{
	derive_ASTNode,
	functions::{
		FunctionBased, FunctionBody, HeadingAndPosition, MethodHeader, SuperParameter,
		ThisParameter,
	},
	property_key::PublicOrPrivate,
	visiting::Visitable,
	ASTNode, Block, Expression, FunctionBase, ParseOptions, ParseResult, PropertyKey,
	TypeAnnotation, WithComment,
};
use source_map::Span;
use visitable_derive::Visitable;

#[cfg_attr(target_family = "wasm", tsify::declare)]
pub type IsStatic = bool;

#[apply(derive_ASTNode)]
#[derive(Debug, Clone, PartialEq, Visitable)]
pub enum ClassMember {
	Constructor(ClassConstructor),
	Method(IsStatic, ClassFunction),
	Property(IsStatic, ClassProperty),
	StaticBlock(Block),
	/// Really for interfaces but here
	Indexer {
		name: String,
		indexer_type: TypeAnnotation,
		return_type: TypeAnnotation,
		is_readonly: bool,
		position: Span,
	},
	Comment(String, bool, Span),
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct ClassConstructorBase;
pub type ClassConstructor = FunctionBase<ClassConstructorBase>;

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct ClassFunctionBase;
pub type ClassFunction = FunctionBase<ClassFunctionBase>;

#[derive(Debug, Clone, PartialEq, Visitable)]
#[apply(derive_ASTNode)]
pub struct ClassProperty {
	pub is_readonly: bool,
	pub is_optional: bool,
	pub key: WithComment<PropertyKey<PublicOrPrivate>>,
	pub type_annotation: Option<TypeAnnotation>,
	pub value: Option<Box<Expression>>,
	pub position: Span,
}

impl ASTNode for ClassMember {
	fn get_position(&self) -> Span {
		match self {
			Self::Constructor(cst) => cst.get_position(),
			Self::Method(_, mtd) => mtd.get_position(),
			Self::Property(_, prop) => prop.position,
			Self::StaticBlock(blk) => blk.get_position(),
			Self::Indexer { position: pos, .. } | Self::Comment(.., pos) => *pos,
		}
	}

	#[allow(clippy::similar_names)]
	fn from_reader(reader: &mut crate::new::Lexer) -> ParseResult<Self> {
		if reader.starts_with_str("//") || reader.starts_with_str("/*") {
			todo!("comment; return Ok(Self::Comment(comment, is_multiline, span));");
		}

		// TODO temp fixes. Should be recorded
		let _ = reader.is_keyword_advance("declare");
		let _ = reader.is_keyword_advance("public");
		let _ = reader.is_keyword_advance("private");

		if reader.is_keyword("constructor") {
			let constructor = ClassConstructor::from_reader(reader)?;
			return Ok(ClassMember::Constructor(constructor));
		}

		let is_static = reader.is_keyword_advance("static");

		reader.skip();

		if is_static && reader.starts_with('{') {
			return Ok(ClassMember::StaticBlock(Block::from_reader(reader)?));
		}

		// Special index type annotation
		// TODO ts
		// if reader.starts_with('[')
		// 	&& reader
		// 		.get_current()
		// 		.chars()
		// 		.take_while(|c| c.is_whitespace() || c.is_alphabetic())
		// 		.after(c == ':')
		// {
		// 	// let Token(_, start) = reader.next().unwrap();
		// 	// let (name, _) = token_as_identifier(
		// 	// 	reader.next().ok_or_else(parse_lexing_error)?,
		// 	// 	"class indexer",
		// 	// )?;
		// 	// reader.expect(TSXToken::Colon)?;
		// 	// let indexer_type = TypeAnnotation::from_reader(reader)?;
		// 	// reader.expect(TSXToken::CloseBracket)?;
		// 	// reader.expect(TSXToken::Colon)?;
		// 	// let return_type = TypeAnnotation::from_reader(reader)?;
		// 	// return Ok(ClassMember::Indexer {
		// 	// 	name,
		// 	// 	is_readonly: readonly_position.is_some(),
		// 	// 	indexer_type,
		// 	// 	position: start.union(return_type.get_position()),
		// 	// 	return_type,
		// 	// });
		// 	todo!();
		// }

		let is_readonly = reader.is_keyword_advance("readonly");
		reader.skip();
		let start = reader.get_start();

		let (header, key) = crate::functions::get_method_name(reader)?;
		reader.skip();

		if reader.starts_with('(') || reader.starts_with('<') {
			let function = ClassFunction::from_reader_with_config(reader, header, key)?;
			Ok(ClassMember::Method(is_static, function))
		} else {
			if !header.is_no_modifiers() {
				// TODO ""
				return Err(crate::ParseError::new(
					crate::ParseErrors::ExpectedOperator { expected: "(", found: "" },
					reader.next_item_span(),
				));
			}
			let is_optional = reader.is_operator_advance("?:");
			let type_annotation = if is_optional || reader.is_operator_advance(":") {
				Some(TypeAnnotation::from_reader(reader)?)
			} else {
				None
			};

			let value: Option<Box<Expression>> = if reader.is_operator_advance("=") {
				Some(Box::new(Expression::from_reader(reader)?))
			} else {
				None
			};

			let position = start.union(reader.get_end());

			let property =
				ClassProperty { is_readonly, is_optional, position, key, type_annotation, value };

			Ok(Self::Property(is_static, property))
		}
	}

	fn to_string_from_buffer<T: source_map::ToString>(
		&self,
		buf: &mut T,
		options: &crate::ToStringOptions,
		local: crate::LocalToStringInformation,
	) {
		match self {
			Self::Property(
				is_static,
				ClassProperty {
					is_readonly,
					is_optional: _,
					key,
					type_annotation,
					value,
					position: _,
				},
			) => {
				if *is_static {
					buf.push_str("static ");
				}
				if *is_readonly {
					buf.push_str("readonly ");
				}
				key.to_string_from_buffer(buf, options, local);
				if let (true, Some(type_annotation)) =
					(options.include_type_annotations, type_annotation)
				{
					buf.push_str(": ");
					type_annotation.to_string_from_buffer(buf, options, local);
				}
				if let Some(value) = value {
					buf.push_str(if options.pretty { " = " } else { "=" });
					value.to_string_from_buffer(buf, options, local);
				}
			}
			Self::Method(is_static, function) => {
				if *is_static {
					buf.push_str("static ");
				}
				function.to_string_from_buffer(buf, options, local.next_level());
			}
			Self::Constructor(constructor) => {
				constructor.to_string_from_buffer(buf, options, local.next_level());
			}
			Self::StaticBlock(block) => {
				buf.push_str("static ");
				block.to_string_from_buffer(buf, options, local.next_level());
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
			Self::Indexer { name, indexer_type, return_type, is_readonly: _, position: _ } => {
				if options.include_type_annotations {
					buf.push('[');
					buf.push_str(name);
					buf.push_str(": ");
					indexer_type.to_string_from_buffer(buf, options, local);
					buf.push_str("]: ");
					return_type.to_string_from_buffer(buf, options, local);
				}
			}
		}
	}
}

impl ClassFunction {
	fn from_reader_with_config(
		reader: &mut crate::new::Lexer,
		header: MethodHeader,
		key: WithComment<PropertyKey<PublicOrPrivate>>,
	) -> ParseResult<Self> {
		FunctionBase::from_reader_with_header_and_name(reader, header, key)
	}
}

impl FunctionBased for ClassFunctionBase {
	type Header = MethodHeader;
	type Name = WithComment<PropertyKey<PublicOrPrivate>>;
	type LeadingParameter = (Option<ThisParameter>, Option<SuperParameter>);
	type ParameterVisibility = ();
	type Body = FunctionBody;

	fn has_body(body: &Self::Body) -> bool {
		body.0.is_some()
	}

	#[allow(clippy::similar_names)]
	fn header_and_name_from_reader(
		reader: &mut crate::new::Lexer,
	) -> ParseResult<(HeadingAndPosition<Self>, Self::Name)> {
		let header = MethodHeader::from_reader(reader);
		let name = WithComment::<PropertyKey<_>>::from_reader(reader)?;
		Ok((header, name))
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
		if let PropertyKey::Identifier(name, ..) = name.get_ast_ref() {
			Some(name.as_str())
		} else {
			None
		}
	}
}

impl FunctionBased for ClassConstructorBase {
	type Header = ();
	type Name = ();
	type Body = FunctionBody;
	type LeadingParameter = (Option<ThisParameter>, Option<SuperParameter>);
	type ParameterVisibility = Option<crate::types::Visibility>;

	// fn get_chain_variable(this: &FunctionBase<Self>) -> ChainVariable {
	// 	ChainVariable::UnderClassConstructor(this.body.1)
	// }

	fn has_body(body: &Self::Body) -> bool {
		body.0.is_some()
	}

	fn header_and_name_from_reader(
		reader: &mut crate::new::Lexer,
	) -> ParseResult<(HeadingAndPosition<Self>, Self::Name)> {
		let start = reader.expect_keyword("constructor")?;
		Ok(((), ()))
	}

	fn header_and_name_to_string_from_buffer<T: source_map::ToString>(
		buf: &mut T,
		_header: &Self::Header,
		_name: &Self::Name,
		_options: &crate::ToStringOptions,
		_local: crate::LocalToStringInformation,
	) {
		buf.push_str("constructor");
	}

	fn visit_name<TData>(
		(): &Self::Name,
		_: &mut (impl crate::VisitorReceiver<TData> + ?Sized),
		_: &mut TData,
		_: &crate::visiting::VisitOptions,
		_: &mut temporary_annex::Annex<crate::Chain>,
	) {
	}

	fn visit_name_mut<TData>(
		(): &mut Self::Name,
		_: &mut (impl crate::VisitorMutReceiver<TData> + ?Sized),
		_: &mut TData,
		_: &crate::visiting::VisitOptions,
		_: &mut temporary_annex::Annex<crate::Chain>,
	) {
	}

	fn get_name((): &Self::Name) -> Option<&str> {
		None
	}
}

#[cfg_attr(target_family = "wasm", wasm_bindgen::prelude::wasm_bindgen(typescript_custom_section))]
#[allow(dead_code)]
const CLASS_CONSTRUCTOR_AND_FUNCTION_TYPES: &str = r"
	export interface ClassConstructor extends FunctionBase {
		body: FunctionBody,
		parameters: FunctionParameters<[ThisParameter | null, SuperParameter | null], Visibility>,
	}

	export interface ClassFunction extends FunctionBase {
		header: MethodHeader,
		name: WithComment<PropertyKey<PublicOrPrivate>>
		parameters: FunctionParameters<ThisParameter | null, null>,
		body: FunctionBody,
	}
";
