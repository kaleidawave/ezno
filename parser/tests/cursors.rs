use ezno_parser::{
	expressions::ExpressionId, ASTNode, CursorId, Expression, SourceId, Span, Statement,
	StatementOrDeclaration,
};

#[test]
fn cursor_in_expression() {
	let expression = "function x() { return }";
	let position_of_return = expression.find("return").unwrap() + "return".len() + 1;
	let cursors = vec![(position_of_return, CursorId(0, Default::default()))];

	let expression = Expression::from_string(
		expression.to_owned(),
		Default::default(),
		SourceId::NULL,
		None,
		cursors.clone(),
	)
	.unwrap();

	let Expression::ExpressionFunction(function) = expression else {
		panic!()
	};

	if let [StatementOrDeclaration::Statement(Statement::Return(_, expr))] =
		function.body.0.as_slice()
	{
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
	let expression = Expression::from_string(
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
				Span::NULL_SPAN,
				ExpressionId::NULL
			)),
			property: ezno_parser::PropertyReference::Cursor(CursorId(0, Default::default())),
			position: Span::NULL_SPAN,
			expression_id: ExpressionId::NULL,
			is_optional: false
		}
	);
}
