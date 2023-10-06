use std::{
	collections::{HashMap, HashSet},
	path::PathBuf,
};

use parser::ParseOptions;
use source_map::{FileSystem, MapFileStore, SourceId, WithPathMap};

use crate::{CheckingData, Diagnostic, RootContext, SynthesisableModule, SynthesisedModule};

use super::block::synthesise_block;
