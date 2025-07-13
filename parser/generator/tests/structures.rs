use ezno_ast_generator::{expr, stmt};
use ezno_parser::ASTNode;
use pretty_assertions::assert_eq;

#[test]
fn expr() {
	let expr = expr!(x = 4);
	let out = format!("{expr:#?}");
	assert_eq!(
		out,
		r#"Assignment {
    lhs: VariableOrPropertyAccess(
        Variable(
            "x",
            0..1,
        ),
    ),
    rhs: NumberLiteral(
        Number(
            4.0,
        ),
        4..5,
    ),
    position: 0..5,
}"#,
		"Recieved {out}"
	);
}

#[test]
fn stmt_with_expr_interpolation() {
	let number = 4.2f64.sin();
	let statement = stmt!(let y = #number);
	let out = format!("{statement:#?}");
	assert_eq!(
		out,
		r#"Variable(
    Exportable {
        is_exported: false,
        item: VariableDeclaration {
            kind: Let,
            declarations: [
                VariableDeclarationItem {
                    name: None(
                        Name(
                            Standard(
                                "y",
                                4..5,
                            ),
                        ),
                    ),
                    type_annotation: None,
                    expression: Some(
                        NumberLiteral(
                            Number(
                                -0.8715757724135882,
                            ),
                            0..0,
                        ),
                    ),
                    position: 4..9,
                },
            ],
            position: 0..29,
        },
    },
)"#,
		"Recieved {out}"
	);
}

#[test]
fn stmt_with_var_name_interpolation() {
	let name = "test";
	let statement = stmt!(let #name = 4);
	let out = format!("{statement:#?}");
	assert_eq!(
		out,
		r#"Variable(
    Exportable {
        is_exported: false,
        item: VariableDeclaration {
            kind: Let,
            declarations: [
                VariableDeclarationItem {
                    name: None(
                        Name(
                            Standard(
                                "test",
                                0..0,
                            ),
                        ),
                    ),
                    type_annotation: None,
                    expression: Some(
                        NumberLiteral(
                            Number(
                                4.0,
                            ),
                            27..28,
                        ),
                    ),
                    position: 4..28,
                },
            ],
            position: 0..28,
        },
    },
)"#,
		"Recieved {out}"
	);
}

#[test]
fn interpolation_of_a_statement() {
	let statement = stmt!(let x = 4);
	let my_func = stmt!(function x() {
		console.log(3);
		#statement
	});
	let out = "function x() {
	console.log(3);
	let x = 4
}";
	assert_eq!(my_func.to_string(&Default::default()), out);
}
