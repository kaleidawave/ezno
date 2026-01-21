use crate::expressions::{LHSOfAssignment, MultipleExpression};
use crate::statements_and_declarations::using::{UsingBinding, UsingDeclaration};
use crate::statements_and_declarations::variables::{
	VarVariableStatement, VariableDeclaration, VariableField, VariableKeyword,
};
use crate::{
	ASTNode, Expression, ParseError, ParseErrors, ParseResult, Span, block::BlockOrSingleStatement,
	derive_ASTNode,
};

use visitable_derive::Visitable;

#[apply(derive_ASTNode)]
#[derive(Debug, Clone, Visitable, get_field_by_type::GetFieldByType)]
#[get_field_by_type_target(Span)]
pub struct ForLoopStatement {
	pub condition: ForLoopCondition,
	pub inner: BlockOrSingleStatement,
	pub position: Span,
}

impl ASTNode for ForLoopStatement {
	fn get_position(&self) -> Span {
		self.position
	}

	fn from_reader(reader: &mut crate::Lexer) -> ParseResult<Self> {
		let start = reader.expect_keyword("for")?;
		let is_await = reader.is_keyword_advance("await");
		let mut condition = ForLoopCondition::from_reader(reader)?;
		if is_await {
			if let ForLoopCondition::ForOf { is_await: ref mut a, .. } = condition {
				*a = is_await;
			} else {
				return Err(ParseError::new(
					ParseErrors::AwaitRequiresForOf,
					condition.get_position(),
				));
			}
		}
		let inner = BlockOrSingleStatement::from_reader(reader)?;
		let position = start.union(inner.get_position());
		Ok(ForLoopStatement { condition, inner, position })
	}

	fn to_string_from_buffer<T: source_map::ToString>(
		&self,
		buf: &mut T,
		options: &crate::ToStringOptions,
		local: crate::LocalToStringInformation,
	) {
		buf.push_str("for");
		if let ForLoopCondition::ForOf { is_await: true, .. } = self.condition {
			buf.push_str(" await");
		}
		options.push_gap_optionally(buf);
		self.condition.to_string_from_buffer(buf, options, local);
		options.push_gap_optionally(buf);
		self.inner.to_string_from_buffer(buf, options, local.next_level());
	}
}

#[derive(Debug, Clone, Copy)]
#[apply(derive_ASTNode)]
pub enum VariableKeywordOrUsing {
	Const,
	Let,
	Var,
	Using,
}

#[derive(Debug, Clone, Visitable)]
#[apply(derive_ASTNode)]
pub enum ForLoopStatementInitialiser {
	VariableDeclaration(VariableDeclaration),
	UsingDeclaration(UsingDeclaration),
	VarVariableStatement(VarVariableStatement),
	Expression(Box<MultipleExpression>),
}

#[derive(Debug, Clone, Visitable)]
#[apply(derive_ASTNode)]
pub enum VariableOrAssignable {
	Variable(VariableKeyword, VariableField, Option<crate::TypeAnnotation>),
	Assignable(LHSOfAssignment),
}

#[derive(Debug, Clone, Visitable)]
#[apply(derive_ASTNode)]
pub enum VariableUsingOrAssignable {
	Variable(VariableKeyword, VariableField, Option<crate::TypeAnnotation>),
	Using { is_await: bool, annotation: Option<crate::TypeAnnotation>, name: String },
	Assignable(LHSOfAssignment),
}

#[derive(Debug, Clone, Visitable)]
#[apply(derive_ASTNode)]
pub enum ForLoopCondition {
	ForOf {
		is_await: bool,
		lhs: VariableUsingOrAssignable,
		of: Box<Expression>,
		position: Span,
	},
	ForIn {
		lhs: VariableOrAssignable,
		/// Yes `of` is single expression, `in` is multiple
		r#in: Box<MultipleExpression>,
		position: Span,
	},
	Statements {
		initialiser: Option<ForLoopStatementInitialiser>,
		condition: Option<Box<MultipleExpression>>,
		afterthought: Option<Box<MultipleExpression>>,
		position: Span,
	},
}

impl ASTNode for ForLoopCondition {
	fn get_position(&self) -> Span {
		match self {
			ForLoopCondition::ForOf { position, .. }
			| ForLoopCondition::ForIn { position, .. }
			| ForLoopCondition::Statements { position, .. } => *position,
		}
	}

	fn from_reader(reader: &mut crate::Lexer) -> ParseResult<Self> {
		fn parse_statements(
			reader: &mut crate::Lexer,
			initialiser: Option<ForLoopStatementInitialiser>,
			start: source_map::Start,
		) -> ParseResult<ForLoopCondition> {
			let _semi_colon_one = reader.expect(';')?;
			let condition = if reader.is_operator(";") {
				None
			} else {
				Some(MultipleExpression::from_reader(reader).map(Box::new)?)
			};
			let _semi_colon_two = reader.expect(';')?;
			let afterthought = if reader.is_operator(")") {
				None
			} else {
				Some(MultipleExpression::from_reader(reader).map(Box::new)?)
			};

			let position = start.union(reader.get_end());
			Ok(ForLoopCondition::Statements { initialiser, condition, afterthought, position })
		}

		fn parse_using(
			reader: &mut crate::Lexer,
			is_await: bool,
			start: source_map::Start,
		) -> ParseResult<ForLoopCondition> {
			let name = reader.parse_identifier("using name", true)?.into_owned();
			let annotation = if reader.is_operator_advance(":") {
				Some(crate::TypeAnnotation::from_reader(reader)?)
			} else {
				None
			};
			if reader.is_keyword_advance("of") {
				let lhs = VariableUsingOrAssignable::Using { name, is_await, annotation };
				let of = Box::new(Expression::from_reader(reader)?);
				let position = start.union(reader.get_end());
				Ok(ForLoopCondition::ForOf { is_await: false, lhs, of, position })
			} else {
				reader.expect_operator("=")?;
				let value = Expression::from_reader(reader)?;
				let binding = UsingBinding { name, annotation, value };
				let mut bindings = vec![binding];
				while reader.is_operator_advance(",") {
					let name = reader.parse_identifier("using name", false)?.into_owned();
					let annotation = if reader.is_operator_advance(":") {
						Some(crate::TypeAnnotation::from_reader(reader)?)
					} else {
						None
					};
					reader.expect_operator("=")?;
					let value = Expression::from_reader(reader)?;
					let binding = UsingBinding { name, annotation, value };
					bindings.push(binding);
				}
				let position = start.union(reader.get_end());
				let declaration = UsingDeclaration { is_await, bindings, position };
				let initialiser = Some(ForLoopStatementInitialiser::UsingDeclaration(declaration));
				parse_statements(reader, initialiser, start)
			}
		}

		fn parse_let_const_var(
			reader: &mut crate::Lexer,
			kind: VariableKeyword,
			start: source_map::Start,
		) -> ParseResult<ForLoopCondition> {
			let name = VariableField::from_reader(reader)?;
			let type_annotation = if reader.is_operator_advance(":") {
				let annotation = crate::TypeAnnotation::from_reader(reader)?;
				crate::lexer::utilities::assert_type_annotations(
					reader,
					annotation.get_position(),
				)?;
				Some(annotation)
			} else {
				None
			};
			if reader.is_keyword_advance("of") {
				let lhs = VariableUsingOrAssignable::Variable(kind, name, type_annotation);
				let of = Box::new(Expression::from_reader(reader)?);
				let position = start.union(reader.get_end());
				Ok(ForLoopCondition::ForOf { is_await: false, lhs, of, position })
			} else if reader.is_keyword_advance("in") {
				let lhs = VariableOrAssignable::Variable(kind, name, type_annotation);
				let r#in = Box::new(MultipleExpression::from_reader(reader)?);
				let position = start.union(reader.get_end());
				Ok(ForLoopCondition::ForIn { lhs, r#in, position })
			} else {
				let expression = if reader.is_operator_advance("=") {
					Some(Expression::from_reader(reader)?)
				} else {
					None
				};
				let position = start.union(reader.get_end());
				let mut declarations = vec![crate::variables::VariableDeclarationItem {
					name: crate::WithComment::None(name),
					type_annotation,
					expression,
					position,
				}];

				while reader.is_operator_advance(",") {
					let value = crate::variables::VariableDeclarationItem::from_reader(reader)?;

					if value.expression.is_none() {
						if let VariableKeyword::Const = kind {
							return Err(crate::ParseError::new(
								crate::ParseErrors::ConstDeclarationRequiresValue,
								value.name.get_ast_ref().get_position(),
							));
						}
						if !matches!(value.name.get_ast_ref(), VariableField::Name(_)) {
							return Err(crate::ParseError::new(
								crate::ParseErrors::DestructuringRequiresValue,
								value.name.get_ast_ref().get_position(),
							));
						}
					}

					declarations.push(value);
					if !reader.is_operator_advance(",") {
						break;
					}
				}
				match kind {
					VariableKeyword::Let => {
						let variable_declaration = VariableDeclaration {
							kind: crate::variables::VariableDeclarationKeyword::Let,
							declarations,
							position,
						};
						let initialiser = Some(ForLoopStatementInitialiser::VariableDeclaration(
							variable_declaration,
						));
						parse_statements(reader, initialiser, start)
					}
					VariableKeyword::Const => {
						let variable_declaration = VariableDeclaration {
							kind: crate::variables::VariableDeclarationKeyword::Const,
							declarations,
							position,
						};
						let initialiser = Some(ForLoopStatementInitialiser::VariableDeclaration(
							variable_declaration,
						));
						parse_statements(reader, initialiser, start)
					}
					VariableKeyword::Var => {
						let variable_statement = VarVariableStatement { declarations, position };
						let initialiser = Some(ForLoopStatementInitialiser::VarVariableStatement(
							variable_statement,
						));
						parse_statements(reader, initialiser, start)
					}
				}
			}
		}

		reader.expect('(')?;
		reader.skip();

		let start = reader.get_start();

		// TODO copy using+await to statment parsing
		let condition = if reader.is_keyword_advance("using") {
			parse_using(reader, false, start)?
		} else if reader.is_keyword_advance("await") {
			reader.skip();
			if reader.is_keyword_advance("using") {
				parse_using(reader, true, start)?
			} else {
				let expression = crate::expressions::parse_after_await(reader, start)?;
				let expression = MultipleExpression::from_first_expression(reader, expression)?;
				let initialiser =
					Some(ForLoopStatementInitialiser::Expression(Box::new(expression)));
				parse_statements(reader, initialiser, start)?
			}
		} else if reader.is_keyword_advance("let") {
			parse_let_const_var(reader, VariableKeyword::Let, start)?
		} else if reader.is_keyword_advance("var") {
			parse_let_const_var(reader, VariableKeyword::Var, start)?
		} else if reader.is_keyword_advance("const") {
			parse_let_const_var(reader, VariableKeyword::Const, start)?
		} else if reader.is_operator(";") {
			parse_statements(reader, None, start)?
		} else {
			let expression = Expression::from_reader(reader)?;
			// if let Expression::SpecialOperators(
			// 	crate::expressions::SpecialOperators::In { lhs, rhs },
			// 	_,
			// ) = expression {
			// 	todo!("parse other items");
			// 	let lhs = match lhs {
			// 		crate::expressions::InExpressionLHS::PrivateProperty(_) => {
			// 			return Err(crate::ParseError::new(
			// 				crate::ParseErrors::CannotUsePrivatePropertyHere,
			// 				start.with_length(1),
			// 			));
			// 		}
			// 		crate::expressions::InExpressionLHS::Expression(expression) => *expression,
			// 	};
			// 	let lhs = LHSOfAssignment::try_from(lhs)?;
			// 	let lhs = VariableOrAssignable::Assignable(lhs);
			// 	let r#in = Box::new(MultipleExpression::from_first_expression(reader, *rhs)?);
			// 	let position = start.union(reader.get_end());
			// 	Self::ForIn { lhs, r#in, position }
			// } else {
			// }
			let expression = MultipleExpression::from_first_expression(reader, expression)?;
			let initialiser = Some(ForLoopStatementInitialiser::Expression(Box::new(expression)));
			parse_statements(reader, initialiser, start)?
		};
		reader.expect(')')?;
		Ok(condition)
	}

	fn to_string_from_buffer<T: source_map::ToString>(
		&self,
		buf: &mut T,
		options: &crate::ToStringOptions,
		local: crate::LocalToStringInformation,
	) {
		buf.push('(');
		match self {
			// `is_await` is printed in the parent of this
			Self::ForOf { lhs, of, is_await: _, position: _ } => {
				match lhs {
					VariableUsingOrAssignable::Variable(kw, field, type_annotation) => {
						buf.push_str(kw.as_str());
						field.to_string_from_buffer(buf, options, local);
						if let Some(type_annotation) = type_annotation
							&& options.include_type_annotations
						{
							buf.push_str(": ");
							type_annotation.to_string_from_buffer(buf, options, local);
						}
					}
					VariableUsingOrAssignable::Using { is_await, annotation, name } => {
						if *is_await {
							buf.push_str("await ");
						}
						buf.push_str(name);
						if let Some(type_annotation) = annotation
							&& options.include_type_annotations
						{
							buf.push_str(": ");
							type_annotation.to_string_from_buffer(buf, options, local);
						}
					}
					VariableUsingOrAssignable::Assignable(lhs) => {
						lhs.to_string_from_buffer(buf, options, local);
					}
				}
				// TODO whitespace here if variable is array of object destructuring
				buf.push_str(" of ");
				of.to_string_from_buffer(buf, options, local);
			}
			Self::ForIn { lhs, r#in, position: _ } => {
				match lhs {
					VariableOrAssignable::Variable(kw, field, type_annotation) => {
						buf.push_str(kw.as_str());
						field.to_string_from_buffer(buf, options, local);
						if let (true, Some(type_annotation)) =
							(options.include_type_annotations, &type_annotation)
						{
							buf.push_str(": ");
							type_annotation.to_string_from_buffer(buf, options, local);
						}
					}
					VariableOrAssignable::Assignable(lhs) => {
						lhs.to_string_from_buffer(buf, options, local);
					}
				}
				// TODO whitespace here if variable is array of object destructuring
				buf.push_str(" in ");
				r#in.to_string_from_buffer(buf, options, local);
			}
			Self::Statements { initialiser, condition, afterthought, position: _ } => {
				let mut large = false;
				if options.enforce_limit_length_limit() && local.should_try_pretty_print {
					let room = options.max_line_length as usize;
					let mut buf = source_map::StringWithOptionalSourceMap {
						source: String::new(),
						source_map: None,
						quit_after: Some(room),
						since_new_line: 0,
					};

					if let Some(initialiser) = initialiser {
						initialiser_to_string(initialiser, &mut buf, options, local);
					}
					large = buf.source.len() > room;
					if !large {
						if let Some(condition) = condition {
							condition.to_string_from_buffer(&mut buf, options, local);
						}
						large = buf.source.len() > room;
						if !large {
							if let Some(afterthought) = afterthought {
								afterthought.to_string_from_buffer(&mut buf, options, local);
							}
							large = buf.source.len() > room;
						}
					}
				}
				let inner_local = if large { local.next_level() } else { local };

				if let Some(initialiser) = initialiser {
					if large {
						buf.push_new_line();
						options.add_indent(inner_local.depth, buf);
					}
					initialiser_to_string(initialiser, buf, options, inner_local);
				}
				buf.push(';');
				if let Some(condition) = condition {
					if large {
						buf.push_new_line();
						options.add_indent(inner_local.depth, buf);
					} else {
						options.push_gap_optionally(buf);
					}
					condition.to_string_from_buffer(buf, options, inner_local);
				}
				buf.push(';');
				if let Some(afterthought) = afterthought {
					if large {
						buf.push_new_line();
						options.add_indent(inner_local.depth, buf);
					} else {
						options.push_gap_optionally(buf);
					}
					afterthought.to_string_from_buffer(buf, options, inner_local);
				}
				if large {
					buf.push_new_line();
					options.add_indent(local.depth, buf);
				}
			}
		}
		buf.push(')');
	}
}

fn initialiser_to_string<T: source_map::ToString>(
	initialiser: &ForLoopStatementInitialiser,
	buf: &mut T,
	options: &crate::ToStringOptions,
	local: crate::LocalToStringInformation,
) {
	match initialiser {
		ForLoopStatementInitialiser::VariableDeclaration(stmt) => {
			stmt.to_string_from_buffer(buf, options, local);
		}
		ForLoopStatementInitialiser::UsingDeclaration(stmt) => {
			stmt.to_string_from_buffer(buf, options, local);
		}
		ForLoopStatementInitialiser::Expression(expr) => {
			expr.to_string_from_buffer(buf, options, local);
		}
		ForLoopStatementInitialiser::VarVariableStatement(stmt) => {
			stmt.to_string_from_buffer(buf, options, local);
		}
	}
}
