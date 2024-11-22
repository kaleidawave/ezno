use ezno_parser::{
	declarations::VariableDeclaration,
	visiting::{Chain, ImmutableVariableOrProperty, VisitOptions, Visitor, Visitors},
	ASTNode, Declaration, Expression, Module, StatementOrDeclaration, VariableField,
};
use std::collections::HashSet;

struct Offsets {
	pub offsets: Vec<u32>,
	/// TODO use &str references
	pub top_level_variables: HashSet<String>,
	#[allow(unused, reason = "Want to do this in the future")]
	pub top_level_types: HashSet<String>,
}

/// TODO this could use visting right?
/// TODO abstract to library
/// TODO do for funtions and types
fn get_top_level_identifiers(m: &Module) -> (HashSet<String>, HashSet<String>) {
	let (mut variables, types): (HashSet<_>, HashSet<_>) = Default::default();
	for item in &m.items {
		match item {
			StatementOrDeclaration::Declaration(Declaration::Variable(variable)) => {
				match variable {
					VariableDeclaration::ConstDeclaration { declarations, position: _ } => {
						for declaration in declarations {
							if let VariableField::Name(identifier) = declaration.name.get_ast_ref()
							{
								variables.insert(identifier.as_option_str().unwrap().to_owned());
							}
						}
					}
					VariableDeclaration::LetDeclaration { declarations, position: _ } => {
						for declaration in declarations {
							if let VariableField::Name(identifier) = declaration.name.get_ast_ref()
							{
								variables.insert(identifier.as_option_str().unwrap().to_owned());
							}
						}
					}
				}
			}
			StatementOrDeclaration::Declaration(Declaration::Function(function)) => {
				variables.insert(function.on.name.identifier.as_option_str().unwrap().to_owned());
			}
			_ => {}
		}
	}
	(variables, types)
}

fn main() {
	let code = "
let x = 2;
let y = x + 2;
let z = 6;
"
	.trim();

	// function func() {{ return [x, z] }}
	let module = Module::from_string(code.into(), Default::default()).unwrap();

	let (top_level_variables, top_level_types) = get_top_level_identifiers(&module);

	let mut visitors = Visitors {
		expression_visitors: vec![Box::new(NameReferenceFinder)],
		statement_visitors: Default::default(),
		variable_visitors: vec![Box::new(NameIndexFinder)],
		block_visitors: Default::default(),
	};

	// eprintln!("variables={:#?}", (&top_level_variables, &top_level_types));

	let mut offsets: Offsets =
		Offsets { offsets: Default::default(), top_level_variables, top_level_types };

	module.visit::<Offsets>(
		&mut visitors,
		&mut offsets,
		&VisitOptions { visit_nested_blocks: true, reverse_statements: false },
		source_map::Nullable::NULL,
	);

	// TODO why is this backwards
	// eprintln!("offsets={:#?}", offsets);

	offsets.offsets.sort_unstable();
	let mut rest = code.to_owned();
	for (idx, offset) in offsets.offsets.iter_mut().enumerate().rev() {
		let current_offset = *offset as usize;
		rest.insert_str(current_offset, "000");
		// need to ammed offset now string has been changed
		*offset += ("000".len() * idx) as u32;
	}
	rest.push('\n');

	let mut total = rest.clone();
	const SIZE: usize = 10;
	total.reserve(rest.len() * (SIZE - 1));

	for i in 1..SIZE {
		let name = format!("{i:03}");
		for offset in offsets.offsets.iter().copied() {
			let range = offset as usize..(offset as usize + 3);
			rest.replace_range(range, &name);
		}

		total.push_str(&rest);
	}

	eprintln!("{total}");
}

/// TODO this could be collected in the same process as above
struct NameIndexFinder;

impl<'a> Visitor<ImmutableVariableOrProperty<'a>, Offsets> for NameIndexFinder {
	fn visit(&mut self, item: &ImmutableVariableOrProperty<'a>, data: &mut Offsets, chain: &Chain) {
		if chain.len() == 1 && item.get_variable_name().is_some() {
			data.offsets.push(item.get_position().end);
			// data.insert(name.to_owned());
		}
	}
}

struct NameReferenceFinder;

impl Visitor<Expression, Offsets> for NameReferenceFinder {
	fn visit(&mut self, item: &Expression, data: &mut Offsets, _chain: &Chain) {
		if let Expression::VariableReference(name, position) = item {
			if data.top_level_variables.contains(name) {
				data.offsets.push(position.end);
			}
		}
	}
}
