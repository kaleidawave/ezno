use crate::{
	features, ASTImplementation, BinarySerializable, CheckingData, LocalInformation, ReadFromFS,
	RootContext, TypeStore,
};
use std::collections::HashMap;

use source_map::{FileSystem, Nullable, SourceId};

const CACHE_MARKER: &[u8] = b"ezno-cache-file";

#[derive(binary_serialize_derive::BinarySerializable)]
pub(crate) struct Cache {
	pub(crate) variables: HashMap<String, features::variables::VariableOrImport>,
	pub(crate) named_types: HashMap<String, crate::TypeId>,
	pub(crate) info: LocalInformation,
	pub(crate) types: TypeStore,
	// /// Retains position information
	// pub(crate) content: String,
}

pub fn deserialize_cache<T: ReadFromFS, A: ASTImplementation>(
	length: usize,
	mut content: Vec<u8>,
	checking_data: &mut CheckingData<T, A>,
	root: &mut RootContext,
) {
	crate::utilities::notify!("Using cache :)");
	assert_eq!(length, 1, "only a single cache is current supported");

	let end_content =
		content[CACHE_MARKER.len()..(CACHE_MARKER.len() + U32_BYTES as usize)].to_owned();

	let at_end =
		<u32 as BinarySerializable>::deserialize(&mut end_content.into_iter(), SourceId::NULL);

	let source_id = {
		// Get source and content which is at the end.
		let mut drain =
			content.drain((CACHE_MARKER.len() + U32_BYTES as usize + at_end as usize)..);

		// Okay as end
		let (_source_id, path) =
			<(SourceId, String) as BinarySerializable>::deserialize(&mut drain, SourceId::NULL);

		let get_source_at_path =
			checking_data.modules.files.get_source_at_path(std::path::Path::new(&path));

		if let Some(source_id) = get_source_at_path {
			eprintln!("reusing source id {source_id:?}");
			source_id
		} else {
			// Collect from end
			let source_content = String::from_utf8(drain.collect::<Vec<_>>()).unwrap();
			checking_data.modules.files.new_source_id(path.into(), source_content)
		}
	};

	let mut bytes = content.drain((CACHE_MARKER.len() + U32_BYTES as usize)..);

	// TODO WIP
	let Cache { variables, named_types, info, types } = Cache::deserialize(&mut bytes, source_id);

	root.variables = variables;
	root.named_types = named_types;
	root.info = info;
	checking_data.types = types;
}

const U32_BYTES: u32 = u32::BITS / u8::BITS;

pub fn generate_cache<T: ReadFromFS, A: ASTImplementation>(
	on: &std::path::Path,
	read: T,
	parser_requirements: A::ParserRequirements,
) -> Vec<u8> {
	let mut checking_data =
		CheckingData::<T, A>::new(Default::default(), read, None, parser_requirements);

	let mut root = RootContext::new_with_primitive_references();

	{
		super::add_definition_files_to_root(vec![on.to_path_buf()], &mut root, &mut checking_data);

		// TODO
		// assert!(
		// 	!checking_data.diagnostics_container.contains_error(),
		// 	"found error in definition file {:#?}",
		// 	checking_data.diagnostics_container.get_diagnostics()
		// );
	}

	let mut buf = CACHE_MARKER.to_vec();

	// This reserves a u32 bytes which marks where the content lives
	buf.extend_from_slice(&[0u8; (u32::BITS / u8::BITS) as usize]);

	let cache = Cache {
		variables: root.variables,
		named_types: root.named_types,
		info: root.info,
		types: checking_data.types,
	};

	cache.serialize(&mut buf);

	// Add content
	{
		let cache_len: usize = buf.len() - CACHE_MARKER.len() - U32_BYTES as usize;
		// Set length
		buf[CACHE_MARKER.len()..(CACHE_MARKER.len() + U32_BYTES as usize)]
			.copy_from_slice(&(cache_len as u32).to_le_bytes());

		// TODO not great
		let Some(super::files::File::Source(source, content)) =
			checking_data.modules.get_file(&mut checking_data.resolver, on, None)
		else {
			panic!()
		};

		let path = on.to_str().unwrap().to_owned();
		(source, path).serialize(&mut buf);
		buf.extend_from_slice(content.as_bytes());
	}

	buf
}
