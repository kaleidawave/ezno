use crate::{
	ast::FunctionArgument, derive_ASTNode, errors::parse_lexing_error, ASTNode, Expression,
	ParseError, ParseOptions, ParseResult, Span, TSXToken, Token, TokenReader,
};
use tokenizer_lib::sized_tokens::{TokenEnd, TokenReaderWithTokenEnds, TokenStart};
use visitable_derive::Visitable;

#[apply(derive_ASTNode)]
#[derive(Debug, Clone, PartialEq, Eq, Visitable, get_field_by_type::GetFieldByType)]
#[get_field_by_type_target(Span)]
pub enum JSXRoot {
	Element(JSXElement),
	Fragment(JSXFragment),
}

#[apply(derive_ASTNode)]
#[derive(Debug, Clone, PartialEq, Eq, Visitable, get_field_by_type::GetFieldByType)]
#[get_field_by_type_target(Span)]
pub struct JSXElement {
	/// Name of the element (TODO or reference to element)
	pub tag_name: String,
	pub attributes: Vec<JSXAttribute>,
	pub children: JSXElementChildren,
	pub position: Span,
}

#[derive(Debug, Clone, PartialEq, Eq, Visitable)]
#[apply(derive_ASTNode)]
pub enum JSXElementChildren {
	Children(Vec<JSXNode>),
	/// For img elements
	SelfClosing,
}

impl From<JSXElement> for JSXNode {
	fn from(value: JSXElement) -> JSXNode {
		JSXNode::Element(value)
	}
}

impl ASTNode for JSXElement {
	fn from_reader(
		reader: &mut impl TokenReader<TSXToken, crate::TokenStart>,
		state: &mut crate::ParsingState,
		options: &ParseOptions,
	) -> ParseResult<Self> {
		let start_position = reader.expect_next(TSXToken::JSXOpeningTagStart)?;
		Self::from_reader_sub_start(reader, state, options, start_position)
	}

	fn to_string_from_buffer<T: source_map::ToString>(
		&self,
		buf: &mut T,
		options: &crate::ToStringOptions,
		local: crate::LocalToStringInformation,
	) {
		buf.push('<');
		buf.push_str(&self.tag_name);
		for attribute in &self.attributes {
			buf.push(' ');
			attribute.to_string_from_buffer(buf, options, local);
		}

		match self.children {
			JSXElementChildren::Children(ref children) => {
				buf.push('>');
				jsx_children_to_string(children, buf, options, local);
				buf.push_str("</");
				buf.push_str(&self.tag_name);
				buf.push('>');
			}
			JSXElementChildren::SelfClosing => {
				buf.push_str(">");
			}
		}
	}

	fn get_position(&self) -> Span {
		self.position
	}
}

#[apply(derive_ASTNode)]
#[derive(Debug, Clone, PartialEq, Eq, Visitable, get_field_by_type::GetFieldByType)]
#[get_field_by_type_target(Span)]
pub struct JSXFragment {
	pub children: Vec<JSXNode>,
	pub position: Span,
}

impl ASTNode for JSXFragment {
	fn get_position(&self) -> Span {
		self.position
	}

	fn from_reader(
		reader: &mut impl TokenReader<TSXToken, crate::TokenStart>,
		state: &mut crate::ParsingState,
		options: &ParseOptions,
	) -> ParseResult<Self> {
		let start = reader.expect_next(TSXToken::JSXFragmentStart)?;
		Self::from_reader_sub_start(reader, state, options, start)
	}

	fn to_string_from_buffer<T: source_map::ToString>(
		&self,
		buf: &mut T,
		options: &crate::ToStringOptions,
		local: crate::LocalToStringInformation,
	) {
		buf.push_str("<>");
		jsx_children_to_string(&self.children, buf, options, local);
		buf.push_str("</>");
	}
}

impl JSXFragment {
	fn from_reader_sub_start(
		reader: &mut impl TokenReader<TSXToken, crate::TokenStart>,
		state: &mut crate::ParsingState,
		options: &ParseOptions,
		start: TokenStart,
	) -> ParseResult<Self> {
		let children = parse_jsx_children(reader, state, options)?;
		let end = reader.expect_next_get_end(TSXToken::JSXFragmentEnd)?;
		Ok(Self { children, position: start.union(end) })
	}
}

impl ASTNode for JSXRoot {
	fn from_reader(
		reader: &mut impl TokenReader<TSXToken, crate::TokenStart>,
		state: &mut crate::ParsingState,
		options: &ParseOptions,
	) -> ParseResult<Self> {
		let (is_fragment, span) = match reader.next().ok_or_else(parse_lexing_error)? {
			Token(TSXToken::JSXOpeningTagStart, span) => (false, span),
			Token(TSXToken::JSXFragmentStart, span) => (true, span),
			_ => panic!(),
		};
		Self::from_reader_sub_start(reader, state, options, is_fragment, span)
	}

	fn to_string_from_buffer<T: source_map::ToString>(
		&self,
		buf: &mut T,
		options: &crate::ToStringOptions,
		local: crate::LocalToStringInformation,
	) {
		match self {
			JSXRoot::Element(element) => element.to_string_from_buffer(buf, options, local),
			JSXRoot::Fragment(fragment) => fragment.to_string_from_buffer(buf, options, local),
		}
	}

	fn get_position(&self) -> Span {
		match self {
			JSXRoot::Element(element) => element.get_position(),
			JSXRoot::Fragment(fragment) => fragment.get_position(),
		}
	}
}

fn parse_jsx_children(
	reader: &mut impl TokenReader<TSXToken, crate::TokenStart>,
	state: &mut crate::ParsingState,
	options: &ParseOptions,
) -> ParseResult<Vec<JSXNode>> {
	let mut children = Vec::new();
	loop {
		if matches!(
			reader.peek(),
			Some(Token(TSXToken::JSXFragmentEnd | TSXToken::JSXClosingTagStart, _))
		) {
			return Ok(children);
		}
		children.push(JSXNode::from_reader(reader, state, options)?);
	}
}

fn jsx_children_to_string<T: source_map::ToString>(
	children: &[JSXNode],
	buf: &mut T,
	options: &crate::ToStringOptions,
	local: crate::LocalToStringInformation,
) {
	let element_of_line_break_in_children =
		children.iter().any(|node| matches!(node, JSXNode::Element(..) | JSXNode::LineBreak));

	let mut previous_was_break = true;

	for node in children {
		if element_of_line_break_in_children
			&& !matches!(node, JSXNode::LineBreak)
			&& previous_was_break
		{
			options.add_indent(local.depth + 1, buf);
		}
		node.to_string_from_buffer(buf, options, local);
		previous_was_break = matches!(node, JSXNode::Element(..) | JSXNode::LineBreak);
	}

	if options.pretty && local.depth > 0 && previous_was_break {
		options.add_indent(local.depth, buf);
	}
}

impl JSXRoot {
	pub(crate) fn from_reader_sub_start(
		reader: &mut impl TokenReader<TSXToken, crate::TokenStart>,
		state: &mut crate::ParsingState,
		options: &ParseOptions,
		is_fragment: bool,
		start: TokenStart,
	) -> ParseResult<Self> {
		if is_fragment {
			Ok(Self::Fragment(JSXFragment::from_reader_sub_start(reader, state, options, start)?))
		} else {
			Ok(Self::Element(JSXElement::from_reader_sub_start(reader, state, options, start)?))
		}
	}
}

// TODO can `JSXFragment` appear here?
#[derive(Debug, Clone, PartialEq, Eq, Visitable)]
#[apply(derive_ASTNode)]
pub enum JSXNode {
	Element(JSXElement),
	TextNode(String, Span),
	InterpolatedExpression(Box<FunctionArgument>, Span),
	Comment(String, Span),
	LineBreak,
}

impl ASTNode for JSXNode {
	fn get_position(&self) -> Span {
		match self {
			JSXNode::TextNode(_, pos)
			| JSXNode::InterpolatedExpression(_, pos)
			| JSXNode::Comment(_, pos) => *pos,
			JSXNode::Element(element) => element.get_position(),
			JSXNode::LineBreak => source_map::Nullable::NULL,
		}
	}

	fn from_reader(
		reader: &mut impl TokenReader<TSXToken, crate::TokenStart>,
		state: &mut crate::ParsingState,
		options: &ParseOptions,
	) -> ParseResult<Self> {
		let token = reader.next().ok_or_else(parse_lexing_error)?;
		match token {
			Token(TSXToken::JSXContent(content), start) => {
				let position = start.with_length(content.len());
				// TODO `trim` debatable
				Ok(JSXNode::TextNode(content.trim_start().into(), position))
			}
			Token(TSXToken::JSXExpressionStart, pos) => {
				let expression = FunctionArgument::from_reader(reader, state, options)?;
				let end_pos = reader.expect_next_get_end(TSXToken::JSXExpressionEnd)?;
				Ok(JSXNode::InterpolatedExpression(Box::new(expression), pos.union(end_pos)))
			}
			Token(TSXToken::JSXOpeningTagStart, pos) => {
				JSXElement::from_reader_sub_start(reader, state, options, pos).map(JSXNode::Element)
			}
			Token(TSXToken::JSXContentLineBreak, _) => Ok(JSXNode::LineBreak),
			Token(TSXToken::JSXComment(comment), start) => {
				let pos = start.with_length(comment.len() + 7);
				Ok(JSXNode::Comment(comment, pos))
			}
			_token => Err(parse_lexing_error()),
		}
	}

	fn to_string_from_buffer<T: source_map::ToString>(
		&self,
		buf: &mut T,
		options: &crate::ToStringOptions,
		local: crate::LocalToStringInformation,
	) {
		match self {
			JSXNode::Element(element) => {
				element.to_string_from_buffer(buf, options, local.next_level());
			}
			JSXNode::TextNode(text, _) => buf.push_str(text),
			JSXNode::InterpolatedExpression(expression, _) => {
				buf.push('{');
				expression.to_string_from_buffer(buf, options, local.next_level());
				buf.push('}');
			}
			JSXNode::LineBreak => {
				if options.pretty {
					buf.push_new_line();
				}
			}
			JSXNode::Comment(comment, _) => {
				if options.pretty {
					buf.push_str("<!--");
					buf.push_str(comment);
					buf.push_str("-->");
				}
			}
		}
	}
}

/// TODO spread attributes and boolean attributes
#[derive(Debug, Clone, PartialEq, Eq, Visitable)]
#[apply(derive_ASTNode)]
pub enum JSXAttribute {
	Static(String, String, Span),
	Dynamic(String, Box<Expression>, Span),
	BooleanAttribute(String, Span),
	Spread(Expression, Span),
	/// Preferably want a identifier here not an expr
	Shorthand(Expression),
}

impl ASTNode for JSXAttribute {
	fn get_position(&self) -> Span {
		match self {
			JSXAttribute::Static(_, _, pos)
			| JSXAttribute::Dynamic(_, _, pos)
			| JSXAttribute::BooleanAttribute(_, pos) => *pos,
			JSXAttribute::Spread(_, spread_pos) => *spread_pos,
			JSXAttribute::Shorthand(expr) => expr.get_position(),
		}
	}

	fn from_reader(
		_reader: &mut impl TokenReader<TSXToken, crate::TokenStart>,
		_state: &mut crate::ParsingState,
		_options: &ParseOptions,
	) -> ParseResult<Self> {
		todo!("this is currently done in `JSXElement::from_reader`")
	}

	fn to_string_from_buffer<T: source_map::ToString>(
		&self,
		buf: &mut T,
		options: &crate::ToStringOptions,
		local: crate::LocalToStringInformation,
	) {
		match self {
			JSXAttribute::Static(key, expression, _) => {
				buf.push_str(key.as_str());
				buf.push('=');
				buf.push('"');
				buf.push_str(expression.as_str());
				buf.push('"');
			}
			JSXAttribute::Dynamic(key, expression, _) => {
				buf.push_str(key.as_str());
				buf.push('=');
				buf.push('{');
				expression.to_string_from_buffer(buf, options, local);
				buf.push('}');
			}
			JSXAttribute::BooleanAttribute(key, _) => {
				buf.push_str(key.as_str());
			}
			JSXAttribute::Spread(expr, _) => {
				buf.push_str("...");
				expr.to_string_from_buffer(buf, options, local);
			}
			JSXAttribute::Shorthand(expr) => {
				expr.to_string_from_buffer(buf, options, local);
			}
		}
	}
}

impl JSXElement {
	pub(crate) fn from_reader_sub_start(
		reader: &mut impl TokenReader<TSXToken, crate::TokenStart>,
		state: &mut crate::ParsingState,
		options: &ParseOptions,
		start: TokenStart,
	) -> ParseResult<Self> {
		let Some(Token(TSXToken::JSXTagName(tag_name), _)) = reader.next() else {
			return Err(parse_lexing_error());
		};
		let mut attributes = Vec::new();
		// TODO spread attributes
		// Kind of weird / not clear conditions for breaking out of while loop
		while let Some(token) = reader.next() {
			let (span, key) = match token {
				// Break here
				Token(TSXToken::JSXOpeningTagEnd, _) => break,
				t @ Token(TSXToken::JSXSelfClosingTag, _) => {
					// Early return if self closing
					return Ok(JSXElement {
						tag_name,
						attributes,
						children: JSXElementChildren::SelfClosing,
						position: start.union(t.get_end()),
					});
				}
				Token(TSXToken::JSXExpressionStart, _pos) => {
					let attribute = if let Some(Token(TSXToken::Spread, _)) = reader.peek() {
						let spread_token = reader.next().unwrap();
						let expr = Expression::from_reader(reader, state, options)?;
						reader.expect_next(TSXToken::CloseBrace)?;
						JSXAttribute::Spread(expr, spread_token.get_span())
					} else {
						let expr = Expression::from_reader(reader, state, options)?;
						JSXAttribute::Shorthand(expr)
					};
					attributes.push(attribute);
					continue;
				}
				Token(TSXToken::JSXAttributeKey(key), start) => (start.with_length(key.len()), key),
				_ => return Err(parse_lexing_error()),
			};

			if let Some(Token(TSXToken::JSXAttributeAssign, _)) = reader.peek() {
				reader.next();
				let attribute = match reader.next().unwrap() {
					Token(TSXToken::JSXAttributeValue(expression), start) => {
						let position = start.with_length(expression.len());
						JSXAttribute::Static(key, expression, position)
					}
					Token(TSXToken::JSXExpressionStart, _) => {
						let expression = Expression::from_reader(reader, state, options)?;
						let close = reader.expect_next_get_end(TSXToken::JSXExpressionEnd)?;
						JSXAttribute::Dynamic(key, Box::new(expression), span.union(close))
					}
					_ => return Err(parse_lexing_error()),
				};
				attributes.push(attribute);
			} else {
				// Boolean attributes
				attributes.push(JSXAttribute::BooleanAttribute(key, span));
			}
		}

		let children = parse_jsx_children(reader, state, options)?;
		if let Token(TSXToken::JSXClosingTagStart, _) =
			reader.next().ok_or_else(parse_lexing_error)?
		{
			let end = if let Token(TSXToken::JSXClosingTagName(closing_tag_name), start) =
				reader.next().ok_or_else(parse_lexing_error)?
			{
				let end = start.0 + closing_tag_name.len() as u32 + 2;
				if closing_tag_name != tag_name {
					return Err(ParseError::new(
						crate::ParseErrors::ClosingTagDoesNotMatch {
							expected: &tag_name,
							found: &closing_tag_name,
						},
						start.with_length(closing_tag_name.len() + 2),
					));
				}
				TokenEnd::new(end)
			} else {
				return Err(parse_lexing_error());
			};
			Ok(JSXElement {
				tag_name,
				attributes,
				children: JSXElementChildren::Children(children),
				position: start.union(end),
			})
		} else {
			Err(parse_lexing_error())
		}
	}
}

/// Used for lexing
#[must_use]
pub fn html_tag_contains_literal_content(tag_name: &str) -> bool {
	matches!(tag_name, "script" | "style")
}

/// Used for lexing
#[must_use]
pub fn html_tag_is_self_closing(tag_name: &str) -> bool {
	matches!(
		tag_name,
		"area"
			| "base" | "br"
			| "col" | "embed"
			| "hr" | "img"
			| "input" | "link"
			| "meta" | "param"
			| "source" | "track"
			| "wbr"
	)
}
