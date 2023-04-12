use ezno_parser::{
	statements::UnconditionalElseStatement, ASTNode, Expression, Module, SourceId, Span, Statement,
	ToStringSettings, VisitSettings, VisitorMut, VisitorsMut,
};

#[test]
fn visiting() {
	let input = r#"
        const x = "hello world";
        function y() {
            if (condition) {
                do_thing("hello world" + " test")
            }
        }
        "#;

	let mut module =
		Module::from_string(input.to_owned(), Default::default(), SourceId::NULL, None, Vec::new())
			.unwrap();

	let mut visitors = VisitorsMut {
		expression_visitors_mut: vec![Box::new(MakeStringsUppercase)],
		statement_visitors_mut: vec![Box::new(AddElseClause)],
		jsx_element_visitors_mut: Default::default(),
		variable_visitors_mut: Default::default(),
		block_visitors_mut: Default::default(),
	};
	module.visit_mut(&mut visitors, &mut (), &VisitSettings::default());

	let output = module.to_string(&ToStringSettings::minified());

	let expected = r#"const x="HELLO WORLD";function y(){if(condition){do_thing("HELLO WORLD"+" TEST")}else{console.log("ELSE!")}}"#;
	assert_eq!(output, expected);
}

/// Uppercase all string literals
struct MakeStringsUppercase;

impl VisitorMut<Expression, ()> for MakeStringsUppercase {
	fn visit_mut(&mut self, item: &mut Expression, _data: &mut (), _chain: &ezno_parser::Chain) {
		if let Expression::StringLiteral(content, _quoted, _, _) = item {
			*content = content.to_uppercase();
		}
	}
}

/// Add else cases to if statements without one. In the else statements, it logs "else!"
struct AddElseClause;

impl VisitorMut<Statement, ()> for AddElseClause {
	fn visit_mut(&mut self, item: &mut Statement, _data: &mut (), _chain: &ezno_parser::Chain) {
		if let Statement::IfStatement(if_statement) = item {
			if if_statement.trailing_else.is_none() {
				let inner = Statement::from_string(
					"console.log(\"else!\")".to_owned(),
					Default::default(),
					SourceId::NULL,
					None,
					Vec::new(),
				)
				.unwrap()
				.into();
				if_statement.trailing_else =
					Some(UnconditionalElseStatement { inner, position: Span::NULL_SPAN })
			}
		}
	}
}
