use ezno_ast_generator::{expr, stmt};
use ezno_parser::{
	source_map,
	statements_and_declarations::{
		StatementOrDeclaration, VariableDeclaration, VariableDeclarationItem,
		VariableDeclarationKeyword,
	},
	ASTNode, Expression,
};
use pretty_assertions::assert_eq;

#[test]
fn expr() {
	let expr = expr!(x = 4);
	let out = format!("{expr:#?}");
	assert_eq!(out, "", "Expected {out}");
}

#[test]
fn stmt_with_expr_interpolation() {
	let number = 4.2f64.sin();
	let statement = stmt!(let y = #number);
	let out = format!("{statement:#?}");
	assert_eq!(out, "", "Expected {out}");
}

#[test]
fn stmt_with_var_name_interpolation() {
	let name = "test";
	let statement = stmt!(let #name = 4);
	let out = format!("{statement:#?}");
	assert_eq!(out, "", "Expected {out}");
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
