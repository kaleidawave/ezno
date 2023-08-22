use std::borrow::Cow;

use crate::{
	errors::parse_lexing_error, ASTNode, Expression, ParseError, ParseOptions, ParseResult, Span,
	TSXToken, Token, TokenReader,
};
use visitable_derive::Visitable;

#[derive(Debug, Clone, PartialEq, Eq, Visitable)]
#[cfg_attr(feature = "self-rust-tokenize", derive(self_rust_tokenize::SelfRustTokenize))]
pub enum JSXRoot {
	Element(JSXElement),
	Fragment(JSXFragment),
}

#[derive(Debug, Clone, PartialEq, Eq, Visitable)]
#[cfg_attr(feature = "self-rust-tokenize", derive(self_rust_tokenize::SelfRustTokenize))]
pub struct JSXFragment {
	pub children: Vec<JSXNode>,
	pub position: Span,
}

impl ASTNode for JSXFragment {
	fn get_position(&self) -> Cow<Span> {
		Cow::Borrowed(&self.position)
	}

	fn from_reader(
		reader: &mut impl TokenReader<TSXToken, Span>,
		state: &mut crate::ParsingState,
		settings: &ParseOptions,
	) -> ParseResult<Self> {
		let start_pos = reader.expect_next(TSXToken::JSXFragmentStart)?;
		Self::from_reader_sub_start(reader, state, settings, start_pos)
	}

	fn to_string_from_buffer<T: source_map::ToString>(
		&self,
		buf: &mut T,
		settings: &crate::ToStringOptions,
		depth: u8,
	) {
		buf.push_str("<>");
		jsx_children_to_string(&self.children, buf, settings, depth);
		buf.push_str("</>");
	}
}

impl JSXFragment {
	fn from_reader_sub_start(
		reader: &mut impl TokenReader<TSXToken, Span>,
		state: &mut crate::ParsingState,
		settings: &ParseOptions,
		start_pos: Span,
	) -> ParseResult<Self> {
		let children = parse_jsx_children(reader, state, settings)?;
		let end_pos = reader.expect_next(TSXToken::JSXFragmentEnd)?;
		Ok(Self { children, position: start_pos.union(&end_pos) })
	}
}

impl ASTNode for JSXRoot {
	fn from_reader(
		reader: &mut impl TokenReader<TSXToken, Span>,
		state: &mut crate::ParsingState,
		settings: &ParseOptions,
	) -> ParseResult<Self> {
		let (is_fragment, span) = match reader.next().ok_or_else(parse_lexing_error)? {
			Token(TSXToken::JSXOpeningTagStart, span) => (false, span),
			Token(TSXToken::JSXFragmentStart, span) => (true, span),
			_ => panic!(),
		};
		Self::from_reader_sub_start(reader, state, settings, is_fragment, span)
	}

	fn to_string_from_buffer<T: source_map::ToString>(
		&self,
		buf: &mut T,
		settings: &crate::ToStringOptions,
		depth: u8,
	) {
		match self {
			JSXRoot::Element(element) => element.to_string_from_buffer(buf, settings, depth),
			JSXRoot::Fragment(fragment) => fragment.to_string_from_buffer(buf, settings, depth),
		}
	}

	fn get_position(&self) -> Cow<Span> {
		match self {
			JSXRoot::Element(element) => element.get_position(),
			JSXRoot::Fragment(fragment) => fragment.get_position(),
		}
	}
}

fn parse_jsx_children(
	reader: &mut impl TokenReader<TSXToken, Span>,
	state: &mut crate::ParsingState,
	settings: &ParseOptions,
) -> Result<Vec<JSXNode>, ParseError> {
	let mut children = Vec::new();
	loop {
		if matches!(
			reader.peek().unwrap().0,
			TSXToken::JSXFragmentEnd | TSXToken::JSXClosingTagStart
		) {
			return Ok(children);
		}
		let Token(token, pos) = reader.next().ok_or_else(parse_lexing_error)?;
		let child = match token {
			TSXToken::JSXContent(content) => JSXNode::TextNode(content, pos),
			TSXToken::JSXExpressionStart => {
				let expression = Expression::from_reader(reader, state, settings)?;
				let end_pos = reader.expect_next(TSXToken::JSXExpressionEnd)?;
				JSXNode::InterpolatedExpression(Box::new(expression), pos.union(&end_pos))
			}
			TSXToken::JSXOpeningTagStart => {
				JSXElement::from_reader_sub_start(reader, state, settings, pos)?.into()
			}
			TSXToken::JSXContentLineBreak => JSXNode::LineBreak,
			_token => {
				// unreachable!("Error in JSX lexing {:?}", token);
				return Err(parse_lexing_error());
			}
		};
		children.push(child);
	}
}

fn jsx_children_to_string<T: source_map::ToString>(
	children: &[JSXNode],
	buf: &mut T,
	settings: &crate::ToStringOptions,
	depth: u8,
) {
	for node in children.iter() {
		// if depth > 0 && settings.pretty {
		// 	settings.add_indent(depth + 1, buf);
		// }
		node.to_string_from_buffer(buf, settings, depth);
	}

	if settings.pretty && depth > 0 && matches!(children.last(), Some(JSXNode::LineBreak)) {
		settings.add_indent(depth, buf);
	}
}

impl JSXRoot {
	pub(crate) fn from_reader_sub_start(
		reader: &mut impl TokenReader<TSXToken, Span>,
		state: &mut crate::ParsingState,
		settings: &ParseOptions,
		is_fragment: bool,
		start_position: Span,
	) -> ParseResult<Self> {
		if is_fragment {
			Ok(Self::Fragment(JSXFragment::from_reader_sub_start(
				reader,
				state,
				settings,
				start_position,
			)?))
		} else {
			Ok(Self::Element(JSXElement::from_reader_sub_start(
				reader,
				state,
				settings,
				start_position,
			)?))
		}
	}
}

// TODO Fragment
#[derive(Debug, Clone, PartialEq, Eq, Visitable)]
#[cfg_attr(feature = "self-rust-tokenize", derive(self_rust_tokenize::SelfRustTokenize))]
pub enum JSXNode {
	TextNode(String, Span),
	InterpolatedExpression(Box<Expression>, Span),
	Element(JSXElement),
	LineBreak,
}

impl ASTNode for JSXNode {
	fn get_position(&self) -> Cow<Span> {
		match self {
			JSXNode::TextNode(_, pos) | JSXNode::InterpolatedExpression(_, pos) => {
				Cow::Borrowed(pos)
			}
			JSXNode::Element(element) => element.get_position(),
			JSXNode::LineBreak => todo!(),
		}
	}

	fn from_reader(
		_reader: &mut impl TokenReader<TSXToken, Span>,
		_state: &mut crate::ParsingState,
		_settings: &ParseOptions,
	) -> ParseResult<Self> {
		todo!()
	}

	fn to_string_from_buffer<T: source_map::ToString>(
		&self,
		buf: &mut T,
		settings: &crate::ToStringOptions,
		depth: u8,
	) {
		match self {
			JSXNode::Element(element) => element.to_string_from_buffer(buf, settings, depth + 1),
			JSXNode::InterpolatedExpression(expression, _) => {
				if !settings.should_add_comment()
					&& matches!(&**expression, Expression::Comment(..))
				{
					return;
				}
				buf.push('{');
				expression.to_string_from_buffer(buf, settings, depth + 1);
				buf.push('}');
			}
			JSXNode::TextNode(text, _) => buf.push_str(text),
			JSXNode::LineBreak => {
				if settings.pretty {
					buf.push_new_line();
				}
			}
		}
	}
}

/// TODO spread attributes and boolean attributes
#[derive(Debug, Clone, PartialEq, Eq, Visitable)]
#[cfg_attr(feature = "self-rust-tokenize", derive(self_rust_tokenize::SelfRustTokenize))]
pub enum JSXAttribute {
	Static(String, String, Span),
	Dynamic(String, Box<Expression>, Span),
	BooleanAttribute(String, Span),
	// TODO could combine these two
	Spread(Expression, Span),
	/// Preferably want a identifier here not an expr
	Shorthand(Expression),
}

#[derive(Debug, Clone, PartialEq, Eq, Visitable)]
#[cfg_attr(feature = "self-rust-tokenize", derive(self_rust_tokenize::SelfRustTokenize))]
pub enum JSXElementChildren {
	Children(Vec<JSXNode>),
	/// For img elements
	SelfClosing,
}

#[derive(Debug, Clone, PartialEq, Eq, Visitable)]
#[cfg_attr(feature = "self-rust-tokenize", derive(self_rust_tokenize::SelfRustTokenize))]
#[visit_self]
pub struct JSXElement {
	/// Name of the element (TODO or reference to element)
	pub tag_name: String,
	pub attributes: Vec<JSXAttribute>,
	pub children: JSXElementChildren,
	pub position: Span,
}

impl From<JSXElement> for JSXNode {
	fn from(value: JSXElement) -> JSXNode {
		JSXNode::Element(value)
	}
}

impl ASTNode for JSXElement {
	fn from_reader(
		reader: &mut impl TokenReader<TSXToken, Span>,
		state: &mut crate::ParsingState,
		settings: &ParseOptions,
	) -> ParseResult<Self> {
		let start_position = reader.expect_next(TSXToken::JSXOpeningTagStart)?;
		Self::from_reader_sub_start(reader, state, settings, start_position)
	}

	fn to_string_from_buffer<T: source_map::ToString>(
		&self,
		buf: &mut T,
		settings: &crate::ToStringOptions,
		depth: u8,
	) {
		buf.push('<');
		buf.push_str(&self.tag_name);
		for attribute in self.attributes.iter() {
			buf.push(' ');
			attribute.to_string_from_buffer(buf, settings, depth);
		}

		match self.children {
			JSXElementChildren::Children(ref children) => {
				buf.push('>');

				jsx_children_to_string(children, buf, settings, depth);

				buf.push_str("</");
				buf.push_str(&self.tag_name);
				buf.push('>');
			}
			JSXElementChildren::SelfClosing => {
				buf.push_str("/>");
			}
		}
	}

	fn get_position(&self) -> Cow<Span> {
		Cow::Borrowed(&self.position)
	}
}

impl ASTNode for JSXAttribute {
	fn get_position(&self) -> Cow<Span> {
		match self {
			JSXAttribute::Static(_, _, span)
			| JSXAttribute::Dynamic(_, _, span)
			| JSXAttribute::BooleanAttribute(_, span) => Cow::Borrowed(span),
			JSXAttribute::Spread(expr, spread_pos) => {
				Cow::Owned(spread_pos.union(&expr.get_position()))
			}
			JSXAttribute::Shorthand(expr) => expr.get_position(),
		}
	}

	fn from_reader(
		_reader: &mut impl TokenReader<TSXToken, Span>,
		_state: &mut crate::ParsingState,
		_settings: &ParseOptions,
	) -> ParseResult<Self> {
		todo!()
	}

	fn to_string_from_buffer<T: source_map::ToString>(
		&self,
		buf: &mut T,
		settings: &crate::ToStringOptions,
		depth: u8,
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
				expression.to_string_from_buffer(buf, settings, depth);
				buf.push('}');
			}
			JSXAttribute::BooleanAttribute(key, _) => {
				buf.push_str(key.as_str());
			}
			JSXAttribute::Spread(expr, _) => {
				buf.push_str("...");
				expr.to_string_from_buffer(buf, settings, depth);
			}
			JSXAttribute::Shorthand(expr) => {
				expr.to_string_from_buffer(buf, settings, depth);
			}
		}
	}
}

impl JSXElement {
	pub(crate) fn from_reader_sub_start(
		reader: &mut impl TokenReader<TSXToken, Span>,
		state: &mut crate::ParsingState,
		settings: &ParseOptions,
		mut start_position: Span,
	) -> ParseResult<Self> {
		let tag_name = if let Some(Token(TSXToken::JSXTagName(tag_name), _)) = reader.next() {
			tag_name
		} else {
			return Err(parse_lexing_error());
		};
		let mut attributes = Vec::new();
		// TODO spread attributes
		// Kind of weird / not clear conditions for breaking out of while loop
		while let Some(token) = reader.next() {
			let (key, span) = match token {
				// Break here
				Token(TSXToken::JSXOpeningTagEnd, _) => break,
				Token(TSXToken::JSXSelfClosingTag, position) => {
					// Early return if self closing
					return Ok(JSXElement {
						tag_name,
						attributes,
						children: JSXElementChildren::SelfClosing,
						position: start_position.union(&position),
					});
				}
				Token(TSXToken::JSXAttributeKey(key), pos) => (key, pos),
				Token(TSXToken::JSXExpressionStart, _pos) => {
					let attribute = if let Some(Token(TSXToken::Spread, _)) = reader.peek() {
						let Token(_, spread_pos) = reader.next().unwrap();
						let expr = Expression::from_reader(reader, state, settings)?;
						reader.expect_next(TSXToken::CloseBrace)?;
						JSXAttribute::Spread(expr, spread_pos)
					} else {
						let expr = Expression::from_reader(reader, state, settings)?;
						JSXAttribute::Shorthand(expr)
					};
					attributes.push(attribute);
					continue;
				}
				tok => panic!("Unexpected token {:?}", tok.0),
			};
			if let Some(Token(TSXToken::JSXAttributeAssign, _)) = reader.peek() {
				reader.next();
				let attribute = match reader.next().unwrap() {
					Token(TSXToken::JSXAttributeValue(expression), lit_pos) => {
						JSXAttribute::Static(key, expression, span.union(&lit_pos))
					}
					Token(TSXToken::JSXExpressionStart, _) => {
						let expression = Expression::from_reader(reader, state, settings)?;
						let close_brace = reader.expect_next(TSXToken::JSXExpressionEnd)?;
						JSXAttribute::Dynamic(key, Box::new(expression), span.union(&close_brace))
					}
					Token(_, position) => unreachable!("Invalid token in attribute {:?}", position),
				};
				attributes.push(attribute);
			} else {
				// Boolean attributes
				attributes.push(JSXAttribute::BooleanAttribute(key, span));
			}
		}

		let children = parse_jsx_children(reader, state, settings)?;
		if let Token(TSXToken::JSXClosingTagStart, _) =
			reader.next().ok_or_else(parse_lexing_error)?
		{
			let end_pos = if let Token(TSXToken::JSXClosingTagName(closing_tag_name), position) =
				reader.next().ok_or_else(parse_lexing_error)?
			{
				if closing_tag_name != tag_name {
					return Err(ParseError::new(
						crate::ParseErrors::ClosingTagDoesNotMatch {
							expected: &tag_name,
							found: &closing_tag_name,
						},
						position,
					));
				}
				position
			} else {
				return Err(parse_lexing_error());
			};
			start_position = start_position.union(&end_pos);
		} else {
			return Err(parse_lexing_error());
		};

		Ok(JSXElement {
			tag_name,
			attributes,
			children: JSXElementChildren::Children(children),
			position: start_position,
		})
	}
}

/// Used for lexing
pub fn html_tag_contains_literal_content(tag_name: &str) -> bool {
	matches!(tag_name, "script" | "style")
}

/// Used for lexing
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
