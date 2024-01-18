use std::{collections::HashSet, io::Write};

use ezno_parser::{
	visiting::{VisitOptions, Visitors},
	ASTNode, Declaration, Module, StatementOrDeclaration,
};
use source_map::SourceId;

fn main() -> Result<(), Box<dyn std::error::Error>> {
	let mut args = std::env::args().skip(1);
	let path = args.next().ok_or("expected path to markdown file")?;
	let out = args.next();

	let content = std::fs::read_to_string(&path)?;

	let filters: Vec<&str> = vec!["import", "export", "declare"];

	let blocks = if path.ends_with(".md") {
		let mut blocks = Vec::new();

		let mut lines = content.lines();
		while let Some(line) = lines.next() {
			if line.starts_with("```ts") {
				let mut indented_code = lines
					.by_ref()
					.take_while(|line| !line.starts_with("```"))
					.fold("\t".to_owned(), |mut a, s| {
						a.push_str(s);
						a.push_str("\n\t");
						a
					});

				debug_assert_eq!(indented_code.pop(), Some('\t'));

				if !filters.iter().any(|filter| indented_code.contains(filter)) {
					blocks.push(indented_code);
				}
			}
		}
		blocks
	} else {
		todo!("parse module, split by statement braced")
	};

	let mut final_blocks: Vec<(HashSet<String>, String)> = Vec::new();
	for code in blocks {
		let module = Module::from_string(code.clone(), Default::default()).map_err(Box::new)?;

		let mut names = HashSet::new();

		let mut visitors = Visitors {
			expression_visitors: Default::default(),
			statement_visitors: Default::default(),
			variable_visitors: vec![Box::new(NameFinder)],
			block_visitors: Default::default(),
		};

		module.visit::<HashSet<String>>(
			&mut visitors,
			&mut names,
			&VisitOptions { visit_nested_blocks: false, reverse_statements: false },
			source_map::Nullable::NULL,
		);

		// TODO quick fix to also register interface and type alias names to prevent conflicts
		for item in module.items {
			match item {
				StatementOrDeclaration::Declaration(Declaration::TypeAlias(t)) => {
					names.insert(t.type_name.name.clone());
				}
				StatementOrDeclaration::Declaration(Declaration::Interface(i)) => {
					names.insert(i.on.name.clone());
				}
				_ => {}
			}
		}

		if let Some((items, block)) =
			final_blocks.iter_mut().find(|(uses, _)| uses.is_disjoint(&names))
		{
			items.extend(names.into_iter());
			block.push_str("\n");
			block.push_str(&code);
		} else {
			final_blocks.push((names, code));
		}
	}

	eprintln!("{:?} blocks", final_blocks.len());

	if let Some(out) = out {
		let mut out = std::fs::File::create(out).expect("Cannot open file");
		for (_items, block) in final_blocks {
			writeln!(out, "() => {{\n{block}}};\n").unwrap();
		}
	} else {
		let mut out = std::io::stdout();
		for (_items, block) in final_blocks {
			// eprintln!("block includes: {items:?}\n{block}\n---");
			writeln!(out, "() => {{\n{block}}};\n").unwrap();
		}
	}

	Ok(())
}

struct NameFinder;

impl<'a>
	ezno_parser::visiting::Visitor<
		ezno_parser::visiting::ImmutableVariableOrProperty<'a>,
		HashSet<String>,
	> for NameFinder
{
	fn visit(
		&mut self,
		item: &ezno_parser::visiting::ImmutableVariableOrProperty<'a>,
		data: &mut HashSet<String>,
		_chain: &ezno_parser::visiting::Chain,
	) {
		if let Some(name) = item.get_variable_name() {
			data.insert(name.to_owned());
		}
	}
}
