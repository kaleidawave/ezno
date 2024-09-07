#![allow(unused)]

use crate::{
	are_nodes_over_length, declarations::ClassDeclaration, derive_ASTNode,
	errors::parse_lexing_error, functions, number::NumberRepresentation, parse_bracketed,
	throw_unexpected_token_with_token, to_string_bracketed,
	type_annotations::generic_arguments_from_reader_sub_open_angle, ASTNode, ExpressionPosition,
	FunctionHeader, ListItem, Marker, ParseErrors, ParseResult, Quoted, TSXKeyword,
};

use super::{
	assignments::{LHSOfAssignment, VariableOrPropertyAccess},
	object_literal::ObjectLiteral,
	operators::{
		BinaryOperator, UnaryPrefixAssignmentOperator, UnaryOperator, IncrementOrDecrement, Operator, ARROW_FUNCTION_PRECEDENCE,
		COMMA_PRECEDENCE, CONDITIONAL_TERNARY_PRECEDENCE, CONSTRUCTOR_PRECEDENCE,
		CONSTRUCTOR_WITHOUT_PARENTHESIS_PRECEDENCE, INDEX_PRECEDENCE, MEMBER_ACCESS_PRECEDENCE,
		PARENTHESIZED_EXPRESSION_AND_LITERAL_PRECEDENCE,
	},
	Expression, MultipleExpression, TemplateLiteral,
};

impl Expression {
	pub fn from_reader2(reader: &mut crate::new::Lexer) -> ParseResult<Self> {
		Self::from_reader_with_precedence2(reader, COMMA_PRECEDENCE)
	}

	pub fn from_reader_with_precedence2(
		reader: &mut crate::new::Lexer,
		return_precedence: u8,
	) -> ParseResult<Self> {
		// if let (true, Some(Token(peek, at))) = (options.partial_syntax, reader.peek()) {
		// 	let next_is_not_expression_like = peek.is_expression_delimiter()
		// 		|| start.map_or(false, |start| {
		// 			peek.is_statement_or_declaration_start()
		// 				&& state
		// 					.line_starts
		// 					.byte_indexes_on_different_lines(start.0 as usize, at.0 as usize)
		// 		});

		// 	if next_is_not_expression_like {
		// 		let point = start.unwrap_or(*at);
		// 		// take up the whole next part for checker suggestions
		// 		let position = point.union(source_map::End(at.0));
		// 		return Ok(Expression::Marker {
		// 			marker_id: state.new_partial_point_marker(point),
		// 			position,
		// 		});
		// 	}
		// }

		reader.skip();

		let start = reader.get_start();
		let first_expression = {
			if reader.starts_with('"') || reader.starts_with('\'') {
				let (content, quoted) = reader.parse_string_literal().unwrap();
				let position = start.with_length(content.len() + 2);
				Expression::StringLiteral(content, quoted, position)
			} else if reader.starts_with_number() {
				let (value, length) = reader.parse_number_literal().unwrap();
				let position = start.with_length(length as usize);
				Expression::NumberLiteral(value, position)
			} else if reader.starts_with('/') {
				let (pattern, flags, length) = reader.parse_regex_literal().unwrap();
				let position = start.with_length(length as usize);
				Expression::RegexLiteral { pattern, flags, position }
			} else if reader.starts_with('(') {
				reader.advance(1);
				// TODO scan for =>
				let parenthesize_expression = MultipleExpression::from_reader2(reader)?;

				reader.skip();
				let end = reader.expect_and_get_after(')').unwrap();
				Expression::ParenthesizedExpression(
					Box::new(parenthesize_expression),
					start.union(end),
				)
			}
			
			else if reader.starts_with('[') {
				todo!()
				// let (items, _, end) = parse_bracketed::<ArrayElement>(
				// 	reader,
				// 	state,
				// 	options,
				// 	None,
				// 	TSXToken::CloseBracket,
				// )?;

				// Expression::ArrayLiteral(items, start.union(end))
			} else if reader.starts_with('{') {
				todo!()
				// ObjectLiteral::from_reader(reader, state, options, start)
				// 	.map(Expression::ObjectLiteral)?
			} else if reader.starts_with('(') {
				todo!()
				// let mut parentheses_depth = 1;
				// let is_arrow_function = if let Some(Token(
				// 	TSXToken::Keyword(..)
				// 	| TSXToken::Identifier(..)
				// 	| TSXToken::OpenBrace
				// 	| TSXToken::OpenBracket
				// 	| TSXToken::CloseParentheses
				// 	| TSXToken::Spread,
				// 	_,
				// )) = reader.peek()
				// {
				// 	let next = reader.scan(|token, _| match token {
				// 		TSXToken::OpenParentheses => {
				// 			parentheses_depth += 1;
				// 			false
				// 		}
				// 		TSXToken::CloseParentheses => {
				// 			parentheses_depth -= 1;
				// 			parentheses_depth == 0
				// 		}
				// 		_ => false,
				// 	});

				// 	if let Some(Token(token_type, _)) = next {
				// 		matches!(token_type, TSXToken::Arrow)
				// 	} else {
				// 		return Err(ParseError::new(
				// 			crate::ParseErrors::UnmatchedBrackets,
				// 			t.get_span(),
				// 		));
				// 	}
				// } else {
				// 	false
				// };

				// if is_arrow_function {
				// 	let arrow_function = ArrowFunction::from_reader_sub_open_paren(
				// 		reader, state, options, false, start,
				// 	)?;
				// 	Expression::ArrowFunction(arrow_function)
				// } else {
				// 	let parenthesize_expression =
				// 		MultipleExpression::from_reader(reader, state, options)?;
				// 	let end = reader.expect_next_get_end(TSXToken::CloseParentheses)?;
				// 	Expression::ParenthesizedExpression(
				// 		Box::new(parenthesize_expression),
				// 		start.union(end),
				// 	)
				// }
			}
			// Yes single line comment can occur here if the line splits
			// ```
			// [
			//     // Hi
			//     2
			// ]
			// ```
			else if reader.starts_with_str("//") || reader.starts_with_str("/*") {
				let is_multiline = reader.starts_with_str("/*");
				reader.advance(2);
				let (content, length) = if is_multiline {
					todo!()
				} else {
					todo!()
				};

				reader.advance(length);

				let expression = Self::from_reader_with_precedence2(
					reader,
					return_precedence,
				)?;
				let position = start.union(expression.get_position());
				Expression::Comment {
					is_multiline,
					content,
					position,
					on: Box::new(expression),
					prefix: true,
				}
			} else if reader.starts_with('<') {
				// Token(tok @ (TSXToken::JSXOpeningTagStart | TSXToken::JSXFragmentStart), span) => {
				// 	let var_name = matches!(tok, TSXToken::JSXFragmentStart);
				// 	JSXRoot::from_reader_sub_start(reader, state, options, var_name, span)
				// 		.map(Expression::JSXRoot)?
				// }
				todo!()
			} else if reader.starts_with('`') {
				todo!()
				// TemplateLiteral::from_reader(reader, state, options, None, start)
				// 	.map(Expression::TemplateLiteral)?
			}
			// Token(TSXToken::Keyword(kw), start) if function_header_ish(kw, reader) => {
			// 	// TODO not great to recreate token, but that is how Rust works :)
			// 	let token = Token(TSXToken::Keyword(kw), start);
			// 	let (is_async, start, token) =
			// 		if let Token(TSXToken::Keyword(TSXKeyword::Async), start) = token {
			// 			(true, start, reader.next().unwrap())
			// 		} else {
			// 			(false, start, token)
			// 		};

			// 	if is_async
			// 		&& !matches!(token, Token(TSXToken::Keyword(ref kw), _) if kw.is_in_function_header())
			// 	{
			// 		if let Token(TSXToken::OpenParentheses, start) = token {
			// 			let function = ArrowFunction::from_reader_sub_open_paren(
			// 				reader, state, options, is_async, start,
			// 			)?;
			// 			return Ok(Expression::ArrowFunction(function));
			// 		}

			// 		let (name, position) = token_as_identifier(token, "function parameter")?;
			// 		ArrowFunction::from_reader_with_first_parameter(
			// 			reader,
			// 			state,
			// 			options,
			// 			(name, position),
			// 			is_async,
			// 		)
			// 		.map(Expression::ArrowFunction)?
			// 	} else {
			// 		#[cfg(feature = "extras")]
			// 		{
			// 			use crate::functions::FunctionLocationModifier;
			// 			let (generator_keyword, token) =
			// 				if let Token(TSXToken::Keyword(TSXKeyword::Generator), _) = token {
			// 					(Some(token.get_span()), reader.next().unwrap())
			// 				} else {
			// 					(None, token)
			// 				};

			// 			let (location, token) = match token {
			// 				Token(TSXToken::Keyword(TSXKeyword::Server), _) => {
			// 					(Some(FunctionLocationModifier::Server), reader.next().unwrap())
			// 				}
			// 				Token(TSXToken::Keyword(TSXKeyword::Worker), _) => {
			// 					(Some(FunctionLocationModifier::Worker), reader.next().unwrap())
			// 				}
			// 				token => (None, token),
			// 			};

			// 			// Here because `token` (can't use `.expect_next()`)
			// 			let Token(TSXToken::Keyword(TSXKeyword::Function), function_start) = token
			// 			else {
			// 				return throw_unexpected_token_with_token(
			// 					token,
			// 					&[TSXToken::Keyword(TSXKeyword::Function)],
			// 				);
			// 			};

			// 			let function_end =
			// 				function_start.get_end_after(TSXKeyword::Function.length() as usize);

			// 			if generator_keyword.is_some() {
			// 				let position = start.union(function_end);

			// 				let header = FunctionHeader::ChadFunctionHeader {
			// 					is_async,
			// 					is_generator: true,
			// 					location,
			// 					position,
			// 				};

			// 				let name = if let Some(Token(TSXToken::OpenParentheses, _)) =
			// 					reader.peek()
			// 				{
			// 					None
			// 				} else {
			// 					let (token, span) =
			// 						token_as_identifier(reader.next().unwrap(), "function name")?;
			// 					Some(crate::VariableIdentifier::Standard(token, span))
			// 				};

			// 				FunctionBase::from_reader_with_header_and_name(
			// 					reader,
			// 					state,
			// 					options,
			// 					(Some(header.get_position().get_start()), header),
			// 					ExpressionPosition(name),
			// 				)
			// 				.map(Expression::ExpressionFunction)?
			// 			} else {
			// 				let generator_star_token_position = reader
			// 					.conditional_next(|tok| matches!(tok, TSXToken::Multiply))
			// 					.map(|token| token.get_span());

			// 				let end = generator_star_token_position
			// 					.as_ref()
			// 					.map_or(function_end, Span::get_end);

			// 				let header = FunctionHeader::VirginFunctionHeader {
			// 					position: start.union(end),
			// 					is_async,
			// 					location,
			// 					generator_star_token_position,
			// 				};

			// 				let name = if let Some(Token(TSXToken::OpenParentheses, _)) =
			// 					reader.peek()
			// 				{
			// 					None
			// 				} else {
			// 					let (token, span) =
			// 						token_as_identifier(reader.next().unwrap(), "function name")?;
			// 					Some(crate::VariableIdentifier::Standard(token, span))
			// 				};

			// 				FunctionBase::from_reader_with_header_and_name(
			// 					reader,
			// 					state,
			// 					options,
			// 					(Some(header.get_position().get_start()), header),
			// 					ExpressionPosition(name),
			// 				)
			// 				.map(Expression::ExpressionFunction)?
			// 			}
			// 		}

			// 		#[cfg(not(feature = "extras"))]
			// 		{
			// 			let Token(TSXToken::Keyword(TSXKeyword::Function), _) = token else {
			// 				return throw_unexpected_token_with_token(
			// 					token,
			// 					&[TSXToken::Keyword(TSXKeyword::Function)],
			// 				);
			// 			};

			// 			let generator_star_token_position = reader
			// 				.conditional_next(|tok| matches!(tok, TSXToken::Multiply))
			// 				.map(|token| token.get_span());

			// 			let position =
			// 				start.union(generator_star_token_position.as_ref().unwrap_or(
			// 					&start.with_length(TSXKeyword::Function.length() as usize),
			// 				));

			// 			let header = FunctionHeader::VirginFunctionHeader {
			// 				position,
			// 				is_async,
			// 				generator_star_token_position,
			// 			};

			// 			let name = if let Some(Token(TSXToken::OpenParentheses, _)) = reader.peek()
			// 			{
			// 				None
			// 			} else {
			// 				let (token, span) =
			// 					token_as_identifier(reader.next().unwrap(), "function name")?;

			// 				Some(crate::VariableIdentifier::Standard(token, span))
			// 			};
			// 			FunctionBase::from_reader_with_header_and_name(
			// 				reader,
			// 				state,
			// 				options,
			// 				(Some(start), header),
			// 				ExpressionPosition(name),
			// 			)
			// 			.map(Expression::ExpressionFunction)?
			// 		}
			// 	}
			// }

			// #[cfg(feature = "extras")]
			// t @ Token(TSXToken::Keyword(TSXKeyword::Is), start) if options.is_expressions => {
			// 	// Maintains compatibility here
			// 	let mut parentheses_depth = 0;
			// 	let next = reader.scan(|token, _| match token {
			// 		TSXToken::OpenParentheses => {
			// 			parentheses_depth += 1;
			// 			false
			// 		}
			// 		TSXToken::CloseParentheses => {
			// 			parentheses_depth -= 1;
			// 			parentheses_depth == 0
			// 		}
			// 		_ => false,
			// 	});
			// 	if let Some(Token(token_type, _)) = next {
			// 		if let TSXToken::OpenBrace = token_type {
			// 			let is_expression_from_reader_sub_is_keyword =
			// 				is_expression_from_reader_sub_is_keyword(reader, state, options, start);

			// 			is_expression_from_reader_sub_is_keyword.map(Expression::IsExpression)?
			// 		} else {
			// 			Expression::VariableReference("is".to_owned(), start.with_length(2))
			// 		}
			// 	} else {
			// 		return Err(ParseError::new(
			// 			crate::ParseErrors::UnmatchedBrackets,
			// 			t.get_span(),
			// 		));
			// 	}
			// }
			else if reader.starts_with('#') {
				// let identifier = reader.parse_identifier()?;
				todo!();
			// let (property_name, _) = token_as_identifier(
			// 	reader.next().ok_or_else(parse_lexing_error)?,
			// 	"private in expression",
			// )?;
			// let in_pos = state.expect_keyword(reader, TSXKeyword::In)?;
			// let rhs = Expression::from_reader_with_precedence(
			// 	reader,
			// 	state,
			// 	options,
			// 	RELATION_PRECEDENCE,
			// 	Some(in_pos),
			// )?;
			// let position = start.union(rhs.get_position());
			// Self::SpecialOperators(
			// 	SpecialOperators::In {
			// 		lhs: InExpressionLHS::PrivateProperty(property_name),
			// 		rhs: Box::new(rhs),
			// 	},
			// 	position,
			// )
			}
			else if let Some(op) = reader.is_one_of_operators(&["++", "--"]) {
				let operator = match op {
					"++" => UnaryPrefixAssignmentOperator::IncrementOrDecrement(IncrementOrDecrement::Increment),
					"--" => UnaryPrefixAssignmentOperator::IncrementOrDecrement(IncrementOrDecrement::Decrement),
					op => unreachable!("{op:?}")
				};
				let precedence = operator.precedence();
				todo!();
				// let operand = VariableOrPropertyAccess::from_reader_with_precedence2(
				// 	lexer,
				// 	precedence,
				// )?;
				// let position = start.union(operand.get_position());
				// Expression::UnaryPrefixAssignmentOperation {
				// 	operand,
				// 	operator,
				// 	position,
				// }
			} else if reader.starts_with_str("async ") || reader.is_keyword("function") {
				// TODO generator keyword as well
				// TODO arrow functions
				todo!()
			} else if reader.is_keyword("class") {
				todo!()
				// Token(TSXToken::Keyword(kw @ TSXKeyword::Class), start) => {
				// 	state.append_keyword_at_pos(start.0, kw);
				// 	ClassDeclaration::from_reader_sub_class_keyword(reader, state, options, start)
				// 		.map(Expression::ClassExpression)?
				// }
			} else if let Some(op) = reader.is_one_of_operators(&["+", "-", "~", "!"]) {
				let operator = match op {
					"+" => UnaryOperator::Plus,
					"-" => UnaryOperator::Negation,
					"~" => UnaryOperator::BitwiseNot,
					"!" => UnaryOperator::LogicalNot,
					op => unreachable!("{op:?}")
				};
				let precedence = operator.precedence();
				todo!();
				// let operand = VariableOrPropertyAccess::from_reader_with_precedence2(
				// 	lexer,
				// 	precedence,
				// )?;
				// let position = start.union(operand.get_position());
				// Expression::UnaryOperation {
				// 	operand: Box::new(operand),
				// 	operator,
				// 	position,
				// }
			} else if let Some(keyword) = reader.is_one_of_keyword_advance(&["yield", "typeof", "await", "delete", "void"]) {
				let operator = match keyword {
					"yield" => {
						let is_delegated = reader.is_and_move('*');
						if is_delegated { UnaryOperator::DelegatedYield } else { UnaryOperator::Yield }
					}
					"typeof" => UnaryOperator::TypeOf,
					"await" => UnaryOperator::Await,
					"delete" => UnaryOperator::Delete,
					"void" => UnaryOperator::Void,
					slice => unreachable!("{slice:?}")
				};

				let operand = Expression::from_reader_with_precedence2(
					reader,
					operator.precedence(),
				)?;
				let position = start.union(operand.get_position());
				Expression::UnaryOperation { operator, operand: Box::new(operand), position }
			} else if let Some(keyword) = reader.is_one_of_keyword_advance(&["true", "false"]) {
				Expression::BooleanLiteral(keyword == "true", start.with_length(keyword.len()))
			} else if reader.is_keyword("this") {
				Expression::ThisReference(start.with_length(4))
			} else if reader.is_keyword("null") {
				Expression::Null(start.with_length(4))
			} else if reader.is_keyword("new") {
				todo!()
			} else if reader.is_keyword("super") {
				todo!()
			} else if reader.is_keyword("import") {
				todo!()
			} else {
				let name = reader.parse_identifier().unwrap();
				let position = start.with_length(name.len());
				// if keyword.is_invalid_identifier() {
				// 	return Err(ParseError::new(
				// 		ParseErrors::ReservedIdentifier,
				// 		token.get_span(),
				// 	));
				// }
				// if options.interpolation_points && name == crate::marker::MARKER {
				// 	let marker_id = state.new_partial_point_marker(position.get_start());
				// 	Expression::Marker { marker_id, position }
				// } else if {
				// TODO if next is arrrow
				// } else {
				Expression::VariableReference(name.to_owned(), position)
				// }
			}
		};

		// Operator precedence == 2, nothing can beat so
		// if let Expression::ArrowFunction(..) = first_expression {
		// 	return Ok(first_expression);
		// }

		Self::from_reader_after_first_expression(reader, return_precedence, first_expression)
	}

	pub fn from_reader_after_first_expression(
		reader: &mut crate::new::Lexer,
		return_precedence: u8,
		first_expression: Expression,
	) -> ParseResult<Self> {
		let mut top = first_expression;
		loop {
			reader.skip();

			// Order is important here
			let postfix_binary_operators = &[
				"**",
				"+",
				"-",
				"*",
				"+",
				"-",
				"*",
				"/",
				">>>",
				"<<",
				">>",
				"<",
				">",
				"<=",
				">=",
				"===",
				"==",
				"!==",
				"!=",
				"%",
				"??",
				"&&",
				"||",
				"&",
				"|",
				"^",
				"<@>",
				"|>",
			];

			// TODO could abstract this as left<->right etc + string options
			if let Some(operator_str) = reader.is_one_of_operators(postfix_binary_operators) {
				let operator = match operator_str {
					"+" => BinaryOperator::Add,
					"-" => BinaryOperator::Subtract,
					"*" => BinaryOperator::Multiply,
					"+" => BinaryOperator::Add,
					"-" => BinaryOperator::Subtract,
					"*" => BinaryOperator::Multiply,
					"/" => BinaryOperator::Divide,
					"**" => BinaryOperator::Exponent,
					"<" => BinaryOperator::LessThan,
					">" => BinaryOperator::GreaterThan,
					"<=" => BinaryOperator::LessThanEqual,
					">=" => BinaryOperator::GreaterThanEqual,
					"==" => BinaryOperator::Equal,
					"===" => BinaryOperator::StrictEqual,
					"!=" => BinaryOperator::NotEqual,
					"!==" => BinaryOperator::StrictNotEqual,
					"%" => BinaryOperator::Modulo,
					"??" => BinaryOperator::NullCoalescing,
					"&&" => BinaryOperator::LogicalAnd,
					"||" => BinaryOperator::LogicalOr,
					"<<" => BinaryOperator::BitwiseShiftLeft,
					">>" => BinaryOperator::BitwiseShiftRight,
					">>>" => BinaryOperator::BitwiseShiftRightUnsigned,
					"&" => BinaryOperator::BitwiseAnd,
					"|" => BinaryOperator::BitwiseOr,
					"^" => BinaryOperator::BitwiseXOr,
					// ∘
					"<@>" => BinaryOperator::Compose,
					"|>" => BinaryOperator::Pipe,
					slice => unreachable!("{slice:?}"),
				};

				if operator
					.associativity_direction()
					.should_return(return_precedence, operator.precedence())
				{
					return Ok(top);
				}

				reader.advance(operator_str.len() as u32);

				// if !options.extra_operators && operator.is_non_standard() {
				// 	return Err(ParseError::new(
				// 		ParseErrors::NonStandardSyntaxUsedWithoutEnabled,
				// 		op_pos.with_length(token.length() as usize),
				// 	));
				// }

				let rhs = Self::from_reader_with_precedence2(reader, operator.precedence())?;

				top = Expression::BinaryOperation {
					position: top.get_position().union(rhs.get_position()),
					lhs: Box::new(top),
					operator,
					rhs: Box::new(rhs),
				};
			} else {
				return Ok(top);
			}
		}

		Ok(top)
	}
	// let Token(peeked_token, _peeked_pos) = &reader.peek().unwrap();

	// 		if reader.is_no_move(',') {
	// 			return Ok(top);
	// 			// TODO need a reader.is_a_or_b for the optional-ness
	// 		} else if reader.is_no_move('(') {
	// 			if AssociativityDirection::LeftToRight
	// 				.should_return(parent_precedence, FUNCTION_CALL_PRECEDENCE)
	// 			{
	// 				return Ok(top);
	// 			}
	// 			let is_optional = reader.is_and_move(')
	// 		} else if reader.is_no_move('?') {
	// 			if AssociativityDirection::RightToLeft
	// 				.should_return(parent_precedence, CONDITIONAL_TERNARY_PRECEDENCE)
	// 			{
	// 				return Ok(top);
	// 			}
	// 			reader.next_character();
	// 			let condition_position = top.get_position();
	// 			let condition = Box::new(top);
	// 			let lhs = Box::new(Self::from_reader(reader, state, options)?);
	// 			reader.expect_next(TSXToken::Colon)?;
	// 			let rhs = Self::from_reader(reader, state, options)?;
	// 			let position = condition_position.union(rhs.get_position());
	// 			top = Expression::ConditionalTernary {
	// 				position,
	// 				condition,
	// 				truthy_result: lhs,
	// 				falsy_result: Box::new(rhs),
	// 			};
	// 		}
	// 			// TSXToken::OpenParentheses | TSXToken::OptionalCall => {

	// 			// 	let next = reader.next().unwrap();
	// 			// 	let is_optional = matches!(next.0, TSXToken::OptionalCall);
	// 			// 	let (arguments, _, end) =
	// 			// 		parse_bracketed(reader, state, options, None, TSXToken::CloseParentheses)?;
	// 			// 	let position = top.get_position().union(end);
	// 			// 	top = Expression::FunctionCall {
	// 			// 		function: Box::new(top),
	// 			// 		type_arguments: None,
	// 			// 		arguments,
	// 			// 		position,
	// 			// 		is_optional,
	// 			// 	};
	// 			// }
	// 			// TSXToken::OpenBracket | TSXToken::OptionalIndex => {
	// 			// 	if AssociativityDirection::LeftToRight
	// 			// 		.should_return(parent_precedence, INDEX_PRECEDENCE)
	// 			// 	{
	// 			// 		return Ok(top);
	// 			// 	}
	// 			// 	let next = reader.next().unwrap();
	// 			// 	let is_optional = matches!(next.0, TSXToken::OptionalIndex);

	// 			// 	let indexer = MultipleExpression::from_reader(reader, state, options)?;
	// 			// 	let end = reader.expect_next_get_end(TSXToken::CloseBracket)?;
	// 			// 	let position = top.get_position().union(end);
	// 			// 	top = Expression::Index {
	// 			// 		position,
	// 			// 		indexee: Box::new(top),
	// 			// 		indexer: Box::new(indexer),
	// 			// 		is_optional,
	// 			// 	};
	// 			// }
	// 			// TSXToken::Dot | TSXToken::OptionalChain => {
	// 			// 	if AssociativityDirection::LeftToRight
	// 			// 		.should_return(parent_precedence, MEMBER_ACCESS_PRECEDENCE)
	// 			// 	{
	// 			// 		return Ok(top);
	// 			// 	}

	// 			// 	let Token(accessor, accessor_position) = reader.next().unwrap();
	// 			// 	// TODO not sure
	// 			// 	if matches!(top, Self::ObjectLiteral(..)) {
	// 			// 		return Err(ParseError::new(
	// 			// 			ParseErrors::CannotAccessObjectLiteralDirectly,
	// 			// 			accessor_position.with_length(1),
	// 			// 		));
	// 			// 	}

	// 			// 	let is_optional = matches!(accessor, TSXToken::OptionalChain);

	// 			// 	let Token(peek, at) = reader.peek().ok_or_else(parse_lexing_error)?;
	// 			// 	let is_next_not_identifier = peek.is_expression_delimiter()
	// 			// 		|| (peek.is_statement_or_declaration_start()
	// 			// 			&& state.line_starts.byte_indexes_on_different_lines(
	// 			// 				accessor_position.0 as usize,
	// 			// 				at.0 as usize,
	// 			// 			));

	// 			// 	let (property, position) = if options.partial_syntax && is_next_not_identifier {
	// 			// 		let marker = state.new_partial_point_marker(accessor_position);
	// 			// 		let position = accessor_position.union(source_map::End(at.0));
	// 			// 		(PropertyReference::Marker(marker), position)
	// 			// 	} else {
	// 			// 		let is_private =
	// 			// 			reader.conditional_next(|t| matches!(t, TSXToken::HashTag)).is_some();
	// 			// 		let (property, position) = token_as_identifier(
	// 			// 			reader.next().ok_or_else(parse_lexing_error)?,
	// 			// 			"variable reference",
	// 			// 		)?;
	// 			// 		(PropertyReference::Standard { property, is_private }, position)
	// 			// 	};
	// 			// 	let position = top.get_position().union(position);
	// 			// 	top = Expression::PropertyAccess {
	// 			// 		parent: Box::new(top),
	// 			// 		property,
	// 			// 		position,
	// 			// 		is_optional,
	// 			// 	};
	// 			// }

	// 			// TODO how should this work under the new lexing system>
	// 			// TSXToken::TemplateLiteralStart => {
	// 			// 	// TODO I think this is true
	// 			// 	if AssociativityDirection::LeftToRight
	// 			// 		.should_return(parent_precedence, FUNCTION_CALL_PRECEDENCE)
	// 			// 	{
	// 			// 		return Ok(top);
	// 			// 	}
	// 			// 	reader.next();
	// 			// 	// TODO should check adjacency
	// 			// 	let start = TokenStart::new(top.get_position().start);
	// 			// 	let tag = Some(Box::new(top));
	// 			// 	let mut template_literal = TemplateLiteral::from_reader_with_tag(
	// 			// 		reader, state, options, tagsubl
	// 			// 	);
	// 			// 	top = template_literal.map(Expression::TemplateLiteral)?;
	// 			// }

	// 			else if reader.is_no_move('=') {
	// 				if AssociativityDirection::RightToLeft
	// 					.should_return(parent_precedence, ASSIGNMENT_PRECEDENCE)
	// 				{
	// 					return Ok(top);
	// 				}

	// 				let position = top.get_position();
	// 				let lhs: LHSOfAssignment = top.try_into()?;

	// 				let Token(_assignment, assignment_pos) = reader.next().unwrap();
	// 				let new_rhs = Self::from_reader_with_precedence(
	// 					reader,
	// 					state,
	// 					options,
	// 					parent_precedence,
	// 					Some(assignment_pos),
	// 				)?;

	// 				let position = position.union(new_rhs.get_position());

	// 				top = Expression::Assignment { position, lhs, rhs: Box::new(new_rhs) };
	// 			}

	// 			else if reader.is_and_move("//") {
	// 				// TODO while not comment
	// 				if reader.peek_n(1).is_some_and(|t| t.0.is_expression_postfix()) {
	// 					let (content, is_multiline, position) =
	// 						TSXToken::try_into_comment(reader.next().unwrap()).unwrap();
	// 					top = Expression::Comment {
	// 						content,
	// 						on: Box::new(top),
	// 						position,
	// 						is_multiline,
	// 						prefix: false,
	// 					};
	// 				} else {
	// 					return Ok(top);
	// 				}
	// 			}

	// 			else if reader.is_and_move("/*") {
	// 				let (content, is_multiline, position) =
	// 					TSXToken::try_into_comment(reader.next().unwrap()).unwrap();
	// 				top = Expression::Comment {
	// 					content,
	// 					on: Box::new(top),
	// 					position,
	// 					is_multiline,
	// 					prefix: false,
	// 				};
	// 			}

	// 			// TODO postfix operator ?
	// 			// TSXToken::Keyword(TSXKeyword::As | TSXKeyword::Satisfies | TSXKeyword::Is)
	// 			// 	if options.type_annotations =>
	// 			// {
	// 			// 	if AssociativityDirection::LeftToRight
	// 			// 		.should_return(parent_precedence, RELATION_PRECEDENCE)
	// 			// 	{
	// 			// 		return Ok(top);
	// 			// 	}

	// 			// 	if (cfg!(not(feature = "extras"))
	// 			// 		&& matches!(peeked_token, TSXToken::Keyword(TSXKeyword::Is)))
	// 			// 		|| (cfg!(not(feature = "full-typescript"))
	// 			// 			&& matches!(peeked_token, TSXToken::Keyword(TSXKeyword::As)))
	// 			// 	{
	// 			// 		return Ok(top);
	// 			// 	}

	// 			// 	let token = reader.next().unwrap();
	// 			// 	let reference = TypeAnnotation::from_reader(reader, state, options)?;
	// 			// 	let position = top.get_position().union(reference.get_position());

	// 			// 	let special_operators = match token.0 {
	// 			// 		#[cfg(feature = "full-typescript")]
	// 			// 		TSXToken::Keyword(TSXKeyword::As) => SpecialOperators::AsCast {
	// 			// 			value: top.into(),
	// 			// 			rhs: match reference {
	// 			// 				// TODO temp :0
	// 			// 				TypeAnnotation::Name(
	// 			// 					crate::type_annotations::TypeName::Name(name),
	// 			// 					span,
	// 			// 				) if name == "const" => TypeOrConst::Const(span),
	// 			// 				reference => TypeOrConst::Type(Box::new(reference)),
	// 			// 			},
	// 			// 		},
	// 			// 		TSXToken::Keyword(TSXKeyword::Satisfies) => SpecialOperators::Satisfies {
	// 			// 			value: top.into(),
	// 			// 			type_annotation: Box::new(reference),
	// 			// 		},
	// 			// 		#[cfg(feature = "extras")]
	// 			// 		TSXToken::Keyword(TSXKeyword::Is) => SpecialOperators::Is {
	// 			// 			value: top.into(),
	// 			// 			type_annotation: Box::new(reference),
	// 			// 		},
	// 			// 		_ => unreachable!(),
	// 			// 	};
	// 			// 	top = Self::SpecialOperators(special_operators, position);
	// 			// }

	// 			// TODO ...? How to feature gate this?
	// 			// #[cfg(feature = "full-typescript")]
	// 			// TSXToken::LogicalNot if options.type_annotations => {
	// 			// 	let Token(_token, not_pos) = reader.next().unwrap();
	// 			// 	let position = top.get_position().union(not_pos.get_end_after(1));
	// 			// 	top = Self::SpecialOperators(
	// 			// 		SpecialOperators::NonNullAssertion(Box::new(top)),
	// 			// 		position,
	// 			// 	);
	// 			// }

	// 			// else if reader.is_next_keyword_no_move("is") {
	// 			// 	if AssociativityDirection::LeftToRight
	// 			// 		.should_return(parent_precedence, RELATION_PRECEDENCE)
	// 			// 	{
	// 			// 		return Ok(top);
	// 			// 	}

	// 			// 	// let Token(_token, in_pos) = reader.next().unwrap();
	// 			// 	let rhs = Expression::from_reader_with_precedence(
	// 			// 		reader,
	// 			// 		state,
	// 			// 		options,
	// 			// 		RELATION_PRECEDENCE,
	// 			// 		Some(in_pos),
	// 			// 	)?;
	// 			// 	let position = top.get_position().union(rhs.get_position());
	// 			// 	top = Self::SpecialOperators(
	// 			// 		SpecialOperators::In {
	// 			// 			lhs: InExpressionLHS::Expression(Box::new(top)),
	// 			// 			rhs: Box::new(rhs),
	// 			// 		},
	// 			// 		position,
	// 			// 	);
	// 			// }

	// 			// else if reader.is_next_keyword_no_move("instanceof") {
	// 			// 	if AssociativityDirection::LeftToRight
	// 			// 		.should_return(parent_precedence, RELATION_PRECEDENCE)
	// 			// 	{
	// 			// 		return Ok(top);
	// 			// 	}

	// 			// 	let Token(_, instance_of_pos) = reader.next().unwrap();
	// 			// 	let rhs = Expression::from_reader_with_precedence(
	// 			// 		reader,
	// 			// 		state,
	// 			// 		options,
	// 			// 		RELATION_PRECEDENCE,
	// 			// 		Some(instance_of_pos),
	// 			// 	)?;
	// 			// 	let position = top.get_position().union(rhs.get_position());
	// 			// 	top = Self::SpecialOperators(
	// 			// 		SpecialOperators::InstanceOf { lhs: Box::new(top), rhs: Box::new(rhs) },
	// 			// 		position,
	// 			// 	);
	// 			// }

	// 			token => {
	// 				// Splitting here side-steps some complaints the borrow checker has with passing
	// 				// a mutable reader here
	// 				let token = if let TSXToken::OpenChevron = token {
	// 					if is_generic_arguments(reader) {
	// 						let _ = reader.next();
	// 						let (type_arguments, _) = generic_arguments_from_reader_sub_open_angle(
	// 							reader, state, options, None,
	// 						)?;
	// 						let (arguments, _, end) = parse_bracketed(
	// 							reader,
	// 							state,
	// 							options,
	// 							Some(TSXToken::OpenParentheses),
	// 							TSXToken::CloseParentheses,
	// 						)?;
	// 						top = Expression::FunctionCall {
	// 							position: top.get_position().union(end),
	// 							function: Box::new(top),
	// 							type_arguments: Some(type_arguments),
	// 							arguments,
	// 							is_optional: false,
	// 						};
	// 						continue;
	// 					}
	// 					&TSXToken::OpenChevron
	// 				} else {
	// 					token
	// 				};

	// 				if let Ok(operator) = UnaryPostfixAssignmentOperator::try_from(token) {
	// 					if operator
	// 						.associativity_direction()
	// 						.should_return(parent_precedence, operator.precedence())
	// 					{
	// 						return Ok(top);
	// 					}
	// 					let token = reader.next().unwrap();

	// 					// Increment and decrement are the only two postfix operations
	// 					let position = top.get_position().union(token.get_end());
	// 					top = Expression::UnaryPostfixAssignmentOperation {
	// 						operand: top.try_into()?,
	// 						operator,
	// 						position,
	// 					};
	// 				} else if let Ok(operator) = BinaryOperator::try_from(token) {
	//
	// 				} else if let Ok(operator) = BinaryAssignmentOperator::try_from(token) {
	// 					if operator
	// 						.associativity_direction()
	// 						.should_return(parent_precedence, operator.precedence())
	// 					{
	// 						return Ok(top);
	// 					}
	// 					let Token(_, op_pos) = reader.next().unwrap();
	// 					let new_rhs = Self::from_reader_with_precedence(
	// 						reader,
	// 						state,
	// 						options,
	// 						operator.precedence(),
	// 						Some(op_pos),
	// 					)?;
	// 					top = Expression::BinaryAssignmentOperation {
	// 						position: top.get_position().union(new_rhs.get_position()),
	// 						lhs: top.try_into()?,
	// 						operator,
	// 						rhs: Box::new(new_rhs),
	// 					};
	// 				} else {
	// 					return Ok(top);
	// 				}
	// 			}
	// 		}
	// 	}
}

impl MultipleExpression {
	fn from_reader2(reader: &mut crate::new::Lexer) -> ParseResult<Self> {
		let first = Expression::from_reader2(reader)?;
		reader.skip();
		if reader.starts_with(',') {
			let mut top: MultipleExpression = first.into();
			while reader.is_and_move(',') {
				let rhs = Expression::from_reader2(reader)?;
				let position = top.get_position().union(rhs.get_position());
				top = MultipleExpression::Multiple { lhs: Box::new(top), rhs, position };
			}
			Ok(top)
		} else {
			Ok(MultipleExpression::Single(first))
		}
	}
}
