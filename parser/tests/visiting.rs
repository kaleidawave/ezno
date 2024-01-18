use ezno_parser::{
	statements::UnconditionalElseStatement,
	visiting::{BlockItemMut, Chain, VisitOptions, VisitorMut, VisitorsMut},
	ASTNode, Expression, Module, SourceId, Span, Statement, StatementOrDeclaration,
	ToStringOptions,
};
use pretty_assertions::assert_eq;

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

	let mut module = Module::from_string(input.to_owned(), Default::default()).unwrap();

	let mut visitors = VisitorsMut {
		expression_visitors_mut: vec![Box::new(MakeStringsUppercase)],
		statement_visitors_mut: vec![Box::new(AddElseClause)],
		variable_visitors_mut: Default::default(),
		block_visitors_mut: Default::default(),
	};
	module.visit_mut(&mut visitors, &mut (), &VisitOptions::default(), SourceId::NULL);

	let output = module.to_string(&ToStringOptions::minified());

	let expected = r#"const x="HELLO WORLD";function y(){if(condition){do_thing("HELLO WORLD"+" TEST")}else console.log("ELSE!");}"#;
	assert_eq!(output, expected);
}

/// Uppercase all string literals
struct MakeStringsUppercase;

impl VisitorMut<Expression, ()> for MakeStringsUppercase {
	fn visit_mut(&mut self, item: &mut Expression, _data: &mut (), _chain: &Chain) {
		if let Expression::StringLiteral(content, _quoted, _) = item {
			*content = content.to_uppercase();
		}
	}
}

/// Add else cases to if statements without one. In the else statements, it logs "else!"
struct AddElseClause;

impl VisitorMut<BlockItemMut<'_>, ()> for AddElseClause {
	fn visit_mut(&mut self, item: &mut BlockItemMut, _data: &mut (), _chain: &Chain) {
		if let BlockItemMut::SingleStatement(Statement::If(if_statement))
		| BlockItemMut::StatementOrDeclaration(StatementOrDeclaration::Statement(
			Statement::If(if_statement),
		)) = item
		{
			if if_statement.trailing_else.is_none() {
				let inner =
					Statement::from_string("console.log(\"else!\")".to_owned(), Default::default())
						.unwrap()
						.into();

				if_statement.trailing_else = Some(UnconditionalElseStatement {
					inner,
					position: source_map::Nullable::NULL,
				});
			}
		}
	}
}
