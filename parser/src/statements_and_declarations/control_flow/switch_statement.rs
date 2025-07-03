use source_map::Span;
use visitable_derive::Visitable;

use crate::{ast::MultipleExpression, derive_ASTNode, ASTNode, Expression, StatementOrDeclaration};

#[apply(derive_ASTNode)]
#[derive(Debug, PartialEq, Clone, Visitable, get_field_by_type::GetFieldByType)]
#[get_field_by_type_target(Span)]
pub struct SwitchStatement {
	pub case: Box<MultipleExpression>,
	pub branches: Vec<SwitchBranch>,
	pub position: Span,
}

#[derive(Debug, PartialEq, Clone, Visitable)]
#[apply(derive_ASTNode)]
pub enum SwitchBranch {
	Default(Vec<StatementOrDeclaration>),
	Case(Box<Expression>, Vec<StatementOrDeclaration>),
}

impl ASTNode for SwitchStatement {
	fn get_position(&self) -> Span {
		self.position
	}

	fn from_reader(reader: &mut crate::Lexer) -> Result<Self, crate::ParseError> {
		let start = reader.expect_keyword("switch")?;

		reader.expect('(')?;
		let case = MultipleExpression::from_reader(reader).map(Box::new)?;
		reader.expect(')')?;
		reader.expect('{')?;

		let mut branches = Vec::new();
		loop {
			let case: Option<Box<Expression>> = if reader.is_operator_advance("}") {
				break;
			} else if reader.is_operator_advance("case") {
				let case = Expression::from_reader(reader).map(Box::new)?;
				reader.expect(':')?;
				Some(case)
			} else if reader.is_operator_advance("default") {
				reader.expect(':')?;
				None
			} else if reader.is_one_of(&["//", "/*"]).is_some() {
				let is_multiline = reader.starts_with_slice("/*");
				reader.advance(2);
				let _content = reader.parse_comment_literal(is_multiline)?;
				// Skip for now
				continue;
			} else {
				return Err(crate::lexer::utilities::expected_one_of_items(
					reader,
					&["default", "case", "}"],
				));
			};

			// This is a modified form of Block::from_reader where `TSXKeyword::Case` and
			// `TSXKeyword::Default` are delimiters
			let mut items = Vec::new();
			loop {
				if reader.is_operator("}")
					|| reader.is_one_of_keywords(&["case", "default"]).is_some()
				{
					break;
				}
				let item = StatementOrDeclaration::from_reader(reader)?;
				// TODO temp
				let retain_blank_lines = false;

				if let (
					false,
					StatementOrDeclaration::AestheticSemiColon(..)
					| StatementOrDeclaration::Empty(..),
				) = (retain_blank_lines, &item)
				{
					continue;
				}
				if item.requires_semi_colon() {
					reader.expect_semi_colon()?;
				}
				// Could skip over semi colons regardless. But they are technically empty statements 🤷‍♂️
				items.push(item);
			}

			if let Some(case) = case {
				branches.push(SwitchBranch::Case(case, items));
			} else {
				branches.push(SwitchBranch::Default(items));
			}
		}
		Ok(Self { case, branches, position: start.union(reader.get_end()) })
	}

	fn to_string_from_buffer<T: source_map::ToString>(
		&self,
		buf: &mut T,
		options: &crate::ToStringOptions,
		local: crate::LocalToStringInformation,
	) {
		buf.push_str("switch");
		options.push_gap_optionally(buf);
		buf.push('(');
		self.case.to_string_from_buffer(buf, options, local);
		buf.push(')');
		options.push_gap_optionally(buf);
		buf.push('{');
		for branch in &self.branches {
			if options.pretty {
				buf.push_new_line();
				options.add_indent(local.depth + 1, buf);
			}
			let local = local.next_level();
			match branch {
				SwitchBranch::Default(statements) => {
					buf.push_str("default:");
					if options.pretty {
						buf.push_new_line();
					}
					crate::block::statements_and_declarations_to_string(
						statements,
						buf,
						options,
						local.next_level(),
					);
				}
				SwitchBranch::Case(case, statements) => {
					buf.push_str("case ");
					case.to_string_from_buffer(buf, options, local);
					buf.push(':');
					if options.pretty {
						buf.push_new_line();
					}
					crate::block::statements_and_declarations_to_string(
						statements,
						buf,
						options,
						local.next_level(),
					);
				}
			}
		}
		if options.pretty {
			buf.push_new_line();
			options.add_indent(local.depth, buf);
		}
		buf.push('}');
	}
}
