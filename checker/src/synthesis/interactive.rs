/// For the REPL in Ezno's CLI
use std::{mem, path::PathBuf};

use source_map::{FileSystem, MapFileStore, SourceId, WithPathMap};

use crate::{
	add_definition_files_to_root, types::printing::print_type, CheckingData, DiagnosticsContainer,
	RootContext, TypeId,
};

use super::{block::synthesise_block, expressions::synthesise_multiple_expression};

pub struct State<'a, T: crate::ReadFromFS> {
	checking_data: CheckingData<'a, T, super::EznoParser>,
	root: RootContext,
	source: SourceId,
}

impl<'a, T: crate::ReadFromFS> State<'a, T> {
	pub fn new(
		resolver: &'a T,
		type_definition_files: Vec<PathBuf>,
	) -> Result<Self, (DiagnosticsContainer, MapFileStore<WithPathMap>)> {
		let mut root = RootContext::new_with_primitive_references();
		let mut checking_data =
			CheckingData::new(Default::default(), resolver, Default::default(), ());

		add_definition_files_to_root(type_definition_files, &mut root, &mut checking_data);

		if checking_data.diagnostics_container.contains_error() {
			Err((checking_data.diagnostics_container, checking_data.modules.files))
		} else {
			let source =
				checking_data.modules.files.new_source_id("CLI.tsx".into(), String::default());
			Ok(Self { checking_data, root, source })
		}
	}

	pub fn check_item(
		&mut self,
		item: &parser::Module,
	) -> Result<(Option<String>, DiagnosticsContainer), DiagnosticsContainer> {
		let (ty, ..) = self.root.new_lexical_environment_fold_into_parent(
			crate::Scope::PassThrough { source: self.source },
			&mut self.checking_data,
			|environment, checking_data| {
				if let Some(parser::StatementOrDeclaration::Statement(
					parser::Statement::Expression(expression),
				)) = item.items.last()
				{
					synthesise_block(
						&item.items[..(item.items.len() - 1)],
						environment,
						checking_data,
					);
					let result = synthesise_multiple_expression(
						expression,
						environment,
						checking_data,
						TypeId::ANY_TYPE,
					);
					Some(print_type(result, &checking_data.types, environment, false))
				} else {
					synthesise_block(&item.items, environment, checking_data);
					None
				}
			},
		);
		let dc = mem::take(&mut self.checking_data.diagnostics_container);
		if dc.contains_error() {
			Err(dc)
		} else {
			Ok((ty, dc))
		}
	}

	#[must_use]
	pub fn get_source_id(&self) -> SourceId {
		self.source
	}

	#[must_use]
	pub fn get_fs_ref(&self) -> &MapFileStore<WithPathMap> {
		&self.checking_data.modules.files
	}

	pub fn get_fs_mut(&mut self) -> &mut MapFileStore<WithPathMap> {
		&mut self.checking_data.modules.files
	}
}
