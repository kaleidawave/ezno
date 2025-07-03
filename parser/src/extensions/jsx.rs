use crate::{
	ast::FunctionArgument, derive_ASTNode, ASTNode, Expression, ParseError, ParseErrors,
	ParseResult, Span,
};
use visitable_derive::Visitable;

#[apply(derive_ASTNode)]
#[derive(Debug, Clone, PartialEq, Visitable, get_field_by_type::GetFieldByType)]
#[get_field_by_type_target(Span)]
pub enum JSXRoot {
	Element(JSXElement),
	Fragment(JSXFragment),
}

#[apply(derive_ASTNode)]
#[derive(Debug, Clone, PartialEq, Visitable, get_field_by_type::GetFieldByType)]
#[get_field_by_type_target(Span)]
pub struct JSXElement {
	/// Name of the element (TODO or reference to element)
	pub tag_name: String,
	pub attributes: Vec<JSXAttribute>,
	pub children: JSXElementChildren,
	pub position: Span,
}

pub type JSXChildren = Vec<JSXNode>;

#[derive(Debug, Clone, PartialEq, Visitable)]
#[apply(derive_ASTNode)]
pub enum JSXElementChildren {
	Children(JSXChildren),
	/// For img elements
	SelfClosing,
	/// For script + style elements
	Literal(String),
}

impl From<JSXElement> for JSXNode {
	fn from(value: JSXElement) -> JSXNode {
		JSXNode::Element(value)
	}
}

impl ASTNode for JSXElement {
	fn get_position(&self) -> Span {
		self.position
	}

	fn from_reader(reader: &mut crate::Lexer) -> ParseResult<Self> {
		let start = reader.expect_start('<')?;
		let tag_name = reader.parse_identifier("JSX element name", false)?.to_owned();
		let mut attributes = Vec::new();
		// TODO spread attributes
		// Kind of weird / not clear conditions for breaking out of while loop
		loop {
			if reader.is_operator_advance(">") {
				break;
			} else if reader.is_operator_advance("/>") {
				// TODO check set closing
				// Early return if self closing
				let end = reader.get_end();
				return Ok(JSXElement {
					tag_name,
					attributes,
					children: JSXElementChildren::SelfClosing,
					position: start.union(end),
				});
			} else if reader.is_operator_advance("{") {
				let start = reader.get_start();
				let attribute = if reader.is_operator_advance("...") {
					let expression = Expression::from_reader(reader)?;
					let end = reader.expect('}')?;
					JSXAttribute::Spread(expression, start.union(end))
				} else {
					let expression = Expression::from_reader(reader)?;
					JSXAttribute::Shorthand(expression)
				};
				attributes.push(attribute);
			} else {
				let start = reader.get_start();
				// Using this because parse_identifier breaks on things that we want to include here
				let result = reader.parse_until_one_of(&["=", ">", " ", "\n"]);
				let (key, delimiter) = match result {
					Ok((key, delimiter)) => (key.to_owned(), delimiter),
					Err(()) => {
						return Err(ParseError::new(
							ParseErrors::ExpectedIdentifier { location: "JSX Attribute" },
							start.with_length(0),
						));
					}
				};
				if !reader.get_options().special_jsx_attributes {
					let idx_of_invalid_character = key.char_indices().find_map(|(idx, c)| {
						(!(c.is_alphanumeric() || matches!(c, '_' | '-')))
							.then_some(idx + c.len_utf8())
					});
					if let Some(idx) = idx_of_invalid_character {
						return Err(ParseError::new(
							ParseErrors::ExpectedIdentifier { location: "JSX Attribute" },
							start.with_length(idx),
						));
					}
				}
				reader.advance(delimiter.len() as u32);
				match delimiter {
					"=" => {
						let start = reader.get_start();
						let attribute = if reader.is_operator_advance("{") {
							let expression = Expression::from_reader(reader)?;
							let end = reader.expect('}')?;
							JSXAttribute::Dynamic(key, Box::new(expression), start.union(end))
						} else if reader.starts_with_string_delimeter() {
							// TODO _quoted
							let (content, _quoted) = reader.parse_string_literal()?;
							let position = start.with_length(content.len() + 2);
							JSXAttribute::Static(key, content.to_owned(), position)
						} else {
							let (_found, position) = crate::lexer::utilities::next_item(reader);
							return Err(ParseError::new(
								ParseErrors::ExpectedJSXAttribute,
								position,
							));
						};
						attributes.push(attribute);
					}
					delimiter => {
						// Boolean attributes
						let position = start.with_length(key.len());
						let attribute = JSXAttribute::Boolean(key, position);
						attributes.push(attribute);
						if delimiter == ">" {
							break;
						}
					}
				}
			}
		}

		if html_tag_is_self_closing(&tag_name) {
			return Ok(JSXElement {
				tag_name,
				attributes,
				children: JSXElementChildren::SelfClosing,
				position: start.union(reader.get_end()),
			});
		} else if html_tag_contains_literal_content(&tag_name) {
			// TODO could embedded parser?
			let content = reader
				.parse_until("</")
				.map_err(|()| {
					let (_found, position) = crate::lexer::utilities::next_item(reader);
					ParseError::new(crate::ParseErrors::UnexpectedEnd, position)
				})?
				.to_owned();

			let closing_tag_name = reader.parse_identifier("JSX closing tag", false)?;
			if tag_name != closing_tag_name {
				return Err(ParseError::new(
					crate::ParseErrors::ClosingTagDoesNotMatch {
						tag_name: &tag_name,
						closing_tag_name,
					},
					start.with_length(closing_tag_name.len() + 2),
				));
			}
			let end = reader.expect('>')?;
			return Ok(JSXElement {
				tag_name,
				attributes,
				children: JSXElementChildren::Literal(content),
				position: start.union(end),
			});
		}

		let children = jsx_children_from_reader(reader)?;
		if reader.is_operator_advance("</") {
			let closing_tag_name = reader.parse_identifier("JSX closing tag", false)?;
			let end = reader.expect('>')?;
			if closing_tag_name != tag_name {
				return Err(ParseError::new(
					crate::ParseErrors::ClosingTagDoesNotMatch {
						tag_name: &tag_name,
						closing_tag_name,
					},
					start.with_length(closing_tag_name.len() + 2),
				));
			}
			Ok(JSXElement {
				tag_name,
				attributes,
				children: JSXElementChildren::Children(children),
				position: start.union(end),
			})
		} else {
			todo!()
			// Err(parse_lexing_error())
		}
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
		buf.push('>');

		match self.children {
			JSXElementChildren::Children(ref children) => {
				jsx_children_to_string(children, buf, options, local);
				buf.push_str("</");
				buf.push_str(&self.tag_name);
				buf.push('>');
			}
			JSXElementChildren::SelfClosing => {}
			JSXElementChildren::Literal(ref content) => {
				// if options.pretty {
				// 	// Perform indent correction
				// 	// Have to use '\n' as `.lines` with it's handling of '\r'
				// 	for (idx, line) in content.split('\n').enumerate() {
				// 		if idx > 0 {
				// 			buf.push_new_line();
				// 		}
				// 		options.add_indent(local.depth, buf);
				// 		buf.push_str(line);
				// 	}
				// } else {
				buf.push_str_contains_new_line(content.as_str());
				// }
				buf.push_str("</");
				buf.push_str(&self.tag_name);
				buf.push('>');
			}
		}
	}
}

/// TODO spread attributes and boolean attributes
#[derive(Debug, Clone, PartialEq, Visitable)]
#[apply(derive_ASTNode)]
pub enum JSXAttribute {
	Static(String, String, Span),
	Dynamic(String, Box<Expression>, Span),
	Boolean(String, Span),
	Spread(Expression, Span),
	/// Preferably want a identifier here not an expr
	Shorthand(Expression),
}

impl ASTNode for JSXAttribute {
	fn get_position(&self) -> Span {
		match self {
			JSXAttribute::Static(_, _, pos)
			| JSXAttribute::Dynamic(_, _, pos)
			| JSXAttribute::Boolean(_, pos) => *pos,
			JSXAttribute::Spread(_, spread_pos) => *spread_pos,
			JSXAttribute::Shorthand(expr) => expr.get_position(),
		}
	}

	fn from_reader(reader: &mut crate::Lexer) -> ParseResult<Self> {
		let start = reader.get_start();
		let key = reader.parse_identifier("JSX element attribute", false)?.to_owned();
		if reader.is_operator_advance("=") {
			let start = reader.get_start();
			if reader.is_operator_advance("{") {
				let expression = Expression::from_reader(reader)?;
				let end = reader.expect('}')?;
				Ok(JSXAttribute::Dynamic(key, Box::new(expression), start.union(end)))
			} else if reader.starts_with_string_delimeter() {
				let (content, _quoted) = reader.parse_string_literal()?;
				let position = start.with_length(content.len() + 2);
				Ok(JSXAttribute::Static(key, content.to_owned(), position))
			} else {
				let (_found, position) = crate::lexer::utilities::next_item(reader);
				Err(ParseError::new(ParseErrors::ExpectedJSXAttribute, position))
			}
		} else {
			// Boolean attributes
			let position = start.with_length(key.len());
			Ok(JSXAttribute::Boolean(key, position))
		}
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
			JSXAttribute::Boolean(key, _) => {
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

#[apply(derive_ASTNode)]
#[derive(Debug, Clone, PartialEq, Visitable, get_field_by_type::GetFieldByType)]
#[get_field_by_type_target(Span)]
pub struct JSXFragment {
	pub children: JSXChildren,
	pub position: Span,
}

impl ASTNode for JSXFragment {
	fn get_position(&self) -> Span {
		self.position
	}

	fn from_reader(reader: &mut crate::Lexer) -> ParseResult<Self> {
		let start = reader.get_start();
		reader.expect_operator("<>")?;
		let children = jsx_children_from_reader(reader)?;
		reader.expect_operator("</>")?;
		let end = reader.get_end();
		Ok(Self { children, position: start.union(end) })
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

impl ASTNode for JSXRoot {
	fn get_position(&self) -> Span {
		match self {
			JSXRoot::Element(element) => element.get_position(),
			JSXRoot::Fragment(fragment) => fragment.get_position(),
		}
	}

	fn from_reader(reader: &mut crate::Lexer) -> ParseResult<Self> {
		if reader.starts_with_slice("<>") {
			JSXFragment::from_reader(reader).map(JSXRoot::Fragment)
		} else {
			JSXElement::from_reader(reader).map(JSXRoot::Element)
		}
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
}

fn jsx_children_from_reader(reader: &mut crate::Lexer) -> ParseResult<Vec<JSXNode>> {
	let mut children = Vec::new();
	// TODO count new lines etc
	loop {
		reader.skip();
		for _ in 0..reader.last_was_from_new_line_consume() {
			children.push(JSXNode::LineBreak);
		}
		if reader.starts_with_slice("</") {
			return Ok(children);
		}
		children.push(JSXNode::from_reader(reader)?);
	}
}

fn jsx_children_to_string<T: source_map::ToString>(
	children: &[JSXNode],
	buf: &mut T,
	options: &crate::ToStringOptions,
	local: crate::LocalToStringInformation,
) {
	let element_or_line_break_in_children =
		children.iter().any(|node| matches!(node, JSXNode::Element(..) | JSXNode::LineBreak));

	let mut previous_was_element_or_line_break = true;

	for node in children {
		if element_or_line_break_in_children
			&& !matches!(node, JSXNode::LineBreak)
			&& previous_was_element_or_line_break
		{
			options.add_indent(local.depth + 1, buf);
		}
		node.to_string_from_buffer(buf, options, local);
		previous_was_element_or_line_break =
			matches!(node, JSXNode::Element(..) | JSXNode::LineBreak);
	}

	if options.pretty && local.depth > 0 && previous_was_element_or_line_break {
		options.add_indent(local.depth, buf);
	}
}

// TODO can `JSXFragment` appear here?
#[derive(Debug, Clone, PartialEq, Visitable)]
#[apply(derive_ASTNode)]
pub enum JSXNode {
	Element(JSXElement),
	TextNode(String, Span),
	/// Function argument as single comments and `...` is allowed
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

	fn from_reader(reader: &mut crate::Lexer) -> ParseResult<Self> {
		reader.skip();
		let start = reader.get_start();
		if reader.is_operator_advance("{") {
			let expression = FunctionArgument::from_reader(reader)?;
			let end = reader.expect('}')?;
			let position = start.union(end);
			Ok(JSXNode::InterpolatedExpression(Box::new(expression), position))
		} else if reader.starts_with_slice("<!--") {
			reader.advance("<!--".len() as u32);
			let content = reader
				.parse_until("-->")
				.map_err(|()| {
					let (_found, position) = crate::lexer::utilities::next_item(reader);
					ParseError::new(crate::ParseErrors::UnexpectedEnd, position)
				})?
				.to_owned();

			let position = start.with_length(content.len());
			Ok(JSXNode::Comment(content, position))
		} else if reader.starts_with_slice("<") {
			let element = JSXElement::from_reader(reader)?;
			Ok(JSXNode::Element(element))
		} else {
			let (content, _) = reader.parse_until_one_of(&["<", "{"]).map_err(|()| {
				let (_found, position) = crate::lexer::utilities::next_item(reader);
				ParseError::new(crate::ParseErrors::UnexpectedEnd, position)
			})?;
			let position = start.with_length(content.len());
			Ok(JSXNode::TextNode(content.trim_start().into(), position))
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
			| "source"
			| "track" | "wbr"
	)
}
