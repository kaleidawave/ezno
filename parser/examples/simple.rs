#[allow(unused)]
use ezno_parser::{ASTNode, Module, Expression};

fn main() {
    let source = "'Hello World!'".to_owned();
    let parse_options = Default::default();
    let result = Expression::from_string_with_options(source.clone(), parse_options, Some(40));
    eprintln!("{result:#?}");
}