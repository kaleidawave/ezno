use ezno_parser::{
	expressions::ExpressionId, extractor::GetFunction, ASTNode, CursorId, Expression, ParseOutput,
	SourceId, Span, Statement,
};

// TODO this should be in source_map
const NULL_SPAN: Span = Span { start: 0, end: 0, source_id: SourceId::NULL };

#[test]
fn cursor_in_expression() {
	let expression = "function x() { return }";
	let position_of_return = expression.find("return").unwrap() + "return".len() + 1;
	let cursors = vec![(position_of_return, CursorId(0, Default::default()))];

	let ParseOutput(expression, mut state) = Expression::from_string(
		expression.to_owned(),
		Default::default(),
		SourceId::NULL,
		None,
		cursors.clone(),
	)
	.unwrap();

	let function = if let Expression::ExtractedExpressionFunction(id) = expression {
		state.function_extractor.get_function(id.0)
	} else {
		panic!()
	};

	if let [Statement::ReturnStatement(_, expr)] = function.body.0.as_slice() {
		let multiple_expression = expr.as_ref().unwrap();
		assert!(multiple_expression.lhs.is_none());
		if let Expression::Cursor { cursor_id, .. } = &multiple_expression.rhs {
			assert_eq!(cursor_id.0, 0);
		} else {
			panic!()
		}
	} else {
		panic!()
	}
}

#[test]
fn cursor_at_property_access() {
	let expression = "x.";
	let ParseOutput(expression, _) = Expression::from_string(
		expression.to_owned(),
		Default::default(),
		SourceId::NULL,
		None,
		vec![(2, CursorId(0, Default::default()))],
	)
	.unwrap();

	assert_eq!(
		expression,
		Expression::PropertyAccess {
			parent: Box::new(Expression::VariableReference(
				"x".to_owned(),
				NULL_SPAN,
				ExpressionId::NULL
			)),
			property: ezno_parser::PropertyReference::Cursor(CursorId(0, Default::default())),
			position: NULL_SPAN,
			expression_id: ExpressionId::NULL,
			is_optional: false
		}
	);
}
