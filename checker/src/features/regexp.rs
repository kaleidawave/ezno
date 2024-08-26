use regress::{backends, Flags, Regex};
use source_map::{SourceId, SpanWithSource};

use super::objects::ObjectBuilder;
use crate::{
	types::{
		properties::{PropertyKey, PropertyValue, Publicity},
		TypeStore,
	},
	BinarySerializable, Constant, Environment, Type, TypeId,
};

#[derive(Debug, Clone)]
pub struct RegExp {
	source: String,
	re: Regex,
	groups: u32,
	named_group_indices: crate::Map<String, u16>,
	flags_unsupported: bool,
	used: bool,
}

impl RegExp {
	pub fn new(pattern: &str, flag_options: Option<&str>) -> Result<Self, String> {
		let source = if let Some(flag_options) = flag_options {
			format!("/{pattern}/{flag_options}")
		} else {
			format!("/{pattern}/")
		};

		let mut flags = Flags::default();
		let mut flags_unsupported = false;

		if let Some(flag_options) = flag_options {
			for flag in flag_options.chars() {
				#[allow(clippy::match_same_arms)]
				match flag {
					'd' => flags_unsupported = true, // indices for substring matches are not supported
					'g' => flags_unsupported = true, // stateful regex is not supported
					'i' => flags.icase = true,
					'm' => flags.multiline = true,
					's' => flags.dot_all = true,
					'u' => flags.unicode = true,
					'v' => flags.unicode_sets = true,
					'y' => flags_unsupported = true, // sticky search is not supported
					_ => panic!("Unknown flag: {flag:?}"),
				}
			}
		}

		let compiled_regex = {
			let mut ire = backends::try_parse(pattern.chars().map(u32::from), flags)
				.map_err(|err| err.text)?;
			if !flags.no_opt {
				backends::optimize(&mut ire);
			}

			backends::emit(&ire)
		};

		// crate::utilities::notify!("{:?}", compiled_regex);

		// let insns = compiled_regex.insns;
		// let brackets = compiled_regex.brackets;
		// let start_pred = compiled_regex.start_pred;
		// let loops = compiled_regex.loops;
		let groups = compiled_regex.groups + 1;
		let named_group_indices =
			compiled_regex.named_group_indices.iter().map(|(l, r)| (l.clone(), *r)).collect();
		// let flags = compiled_regex.flags;

		let re = Regex::from(compiled_regex);

		Ok(Self { source, re, groups, named_group_indices, flags_unsupported, used: false })
	}

	#[must_use]
	pub fn source(&self) -> &str {
		&self.source
	}

	#[must_use]
	pub fn used(&self) -> bool {
		self.used
	}

	pub(crate) fn exec(
		&self,
		pattern_type_id: TypeId,
		types: &mut TypeStore,
		environment: &mut Environment,
		call_site: SpanWithSource,
	) -> TypeId {
		let pattern_type = types.get_type_by_id(pattern_type_id);

		match (self.flags_unsupported, pattern_type) {
			(false, Type::Constant(Constant::String(pattern))) => {
				// Needed to not mutually borrow mutable types
				let pattern = pattern.clone();
				self.exec_constant(&pattern, pattern_type_id, types, environment, call_site)
			}
			_ => self.exec_variable(types, environment, call_site),
		}
	}

	pub(crate) fn exec_constant(
		&self,
		pattern: &str,
		pattern_type_id: TypeId,
		types: &mut TypeStore,
		environment: &mut Environment,
		call_site: SpanWithSource,
	) -> TypeId {
		let mut object =
			ObjectBuilder::new(Some(TypeId::ARRAY_TYPE), types, call_site, &mut environment.info);

		object.append(
			Publicity::Public,
			PropertyKey::String("input".into()),
			PropertyValue::Value(pattern_type_id),
			call_site,
			&mut environment.info,
		);

		match self.re.find(pattern) {
			Some(match_) => {
				{
					let index = types.new_constant_type(Constant::Number(
						(match_.start() as f64).try_into().unwrap(),
					));
					object.append(
						Publicity::Public,
						PropertyKey::String("index".into()),
						PropertyValue::Value(index),
						call_site,
						&mut environment.info,
					);
				}

				for (idx, group) in match_.groups().enumerate() {
					let key = PropertyKey::from_usize(idx);
					let value = match group {
						Some(range) => {
							types.new_constant_type(Constant::String(pattern[range].to_string()))
						}
						None => todo!(),
					};

					object.append(
						Publicity::Public,
						key,
						PropertyValue::Value(value),
						call_site,
						&mut environment.info,
					);
				}

				{
					let named_groups = {
						let mut named_groups_object = ObjectBuilder::new(
							Some(TypeId::NULL_TYPE),
							types,
							call_site,
							&mut environment.info,
						);

						for (name, group) in match_.named_groups() {
							let key = PropertyKey::String(name.to_string().into());
							let value = match group {
								Some(range) => types.new_constant_type(Constant::String(
									pattern[range].to_string(),
								)),
								None => todo!(),
							};

							named_groups_object.append(
								Publicity::Public,
								key,
								PropertyValue::Value(value),
								call_site,
								&mut environment.info,
							);
						}

						named_groups_object.build_object()
					};

					object.append(
						Publicity::Public,
						PropertyKey::String("groups".into()),
						PropertyValue::Value(named_groups),
						call_site,
						&mut environment.info,
					);
				}

				{
					let length = types.new_constant_type(Constant::Number(
						f64::from(self.groups).try_into().unwrap(),
					));

					object.append(
						Publicity::Public,
						PropertyKey::String("length".into()),
						PropertyValue::Value(length),
						call_site,
						&mut environment.info,
					);
				}

				object.build_object()
			}
			None => TypeId::NULL_TYPE,
		}
	}

	pub(crate) fn exec_variable(
		&self,
		types: &mut TypeStore,
		environment: &mut Environment,
		call_site: SpanWithSource,
	) -> TypeId {
		let mut object =
			ObjectBuilder::new(Some(TypeId::ARRAY_TYPE), types, call_site, &mut environment.info);

		object.append(
			Publicity::Public,
			PropertyKey::String("input".into()),
			PropertyValue::Value(TypeId::STRING_TYPE),
			call_site,
			&mut environment.info,
		);

		{
			object.append(
				Publicity::Public,
				PropertyKey::String("index".into()),
				PropertyValue::Value(TypeId::NUMBER_TYPE),
				call_site,
				&mut environment.info,
			);
		}

		for idx in 0..self.groups {
			let key = PropertyKey::from_usize(idx as usize);

			object.append(
				Publicity::Public,
				key,
				PropertyValue::Value(TypeId::STRING_TYPE),
				call_site,
				&mut environment.info,
			);
		}

		{
			let named_groups = {
				let mut named_groups_object = ObjectBuilder::new(
					Some(TypeId::NULL_TYPE),
					types,
					call_site,
					&mut environment.info,
				);

				for name in self.named_group_indices.keys() {
					let key = PropertyKey::String(name.to_string().into());

					named_groups_object.append(
						Publicity::Public,
						key,
						PropertyValue::Value(TypeId::STRING_TYPE),
						call_site,
						&mut environment.info,
					);
				}

				named_groups_object.build_object()
			};

			object.append(
				Publicity::Public,
				PropertyKey::String("groups".into()),
				PropertyValue::Value(named_groups),
				call_site,
				&mut environment.info,
			);
		}

		{
			let length = types
				.new_constant_type(Constant::Number(f64::from(self.groups).try_into().unwrap()));

			object.append(
				Publicity::Public,
				PropertyKey::String("length".into()),
				PropertyValue::Value(length),
				call_site,
				&mut environment.info,
			);
		}

		types.new_or_type(object.build_object(), TypeId::NULL_TYPE)
	}
}

impl std::fmt::Display for RegExp {
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
		write!(f, "{}", self.source)
	}
}

// TODO: Optimize
impl BinarySerializable for RegExp {
	fn serialize(self, buf: &mut Vec<u8>) {
		self.source.serialize(buf);
	}

	fn deserialize<I: Iterator<Item = u8>>(iter: &mut I, source_id: SourceId) -> Self {
		let source = String::deserialize(iter, source_id);

		let (pattern, flags) = source[1..].rsplit_once('/').unwrap();
		let flags = if flags.is_empty() { None } else { Some(flags) };

		Self::new(pattern, flags).unwrap()
	}
}
