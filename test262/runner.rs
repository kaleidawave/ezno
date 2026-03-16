use std::borrow::Cow;
use std::fs::{create_dir, read_dir, read_to_string};
use std::path::{Path, PathBuf};
use std::time::{Duration, Instant};

const ROOT: &str = env!("CARGO_MANIFEST_DIR");

#[allow(unused_mut)]
fn main() {
	let root = Path::new(ROOT);
	let tests_dir: PathBuf = root.join("test262/test");
	let tests_dir_prefix: usize = tests_dir.display().to_string().len() + 1;

	let mut completed = 0;
	let mut successful = 0;

	let mut yaml_parsing = Duration::default();
	let mut parsing = Duration::default();

	let db_file = root.join("out/results.db");
	let _ = create_dir(db_file.parent().unwrap());
	let connection = sqlite::open(&db_file).unwrap();

	let mut store_results_in_db = false;
	// let mut list_files_with_errors = false;
	let mut only_fails = false;

	let mut args = std::env::args().skip(1);
	while let Some(arg) = args.next() {
		match arg.as_str() {
			"--store-results" => {
				store_results_in_db = true;
			}
			"--list-bad-files" => {
				todo!();
				// list_files_with_errors = true;
			}
			"--only-fails" => {
				only_fails = true;
			}
			_ => {
				panic!("unknown {arg:?}");
			}
		}
	}

	let mut statement = if store_results_in_db || only_fails {
		if !only_fails {
			// clean up existing results
			connection.execute("DROP TABLE IF EXISTS results;").unwrap();
			// connection.execute("DROP VIEW IF EXISTS bad_results;").unwrap();
		}

		{
			let query = "
		CREATE TABLE IF NOT EXISTS results (
			path        TEXT PRIMARY KEY,
			info        TEXT,
			description TEXT,
			features    TEXT,
			flags       TEXT,
			es5id       TEXT,
			negative    INTEGER NOT NULL,
			pass        INTEGER NOT NULL,
			parser_out  TEXT
		);"
			.trim_start();

			connection.execute(query).unwrap();
		}

		{
			let query = "
		CREATE VIEW IF NOT EXISTS bad_results (full_path, path, pass, parser_out, negative, flags, description) AS 
		SELECT 'test262/test262/test/' || path AS full_path, path, pass, parser_out, negative, flags, description
		FROM results
		WHERE pass = 0;"
				.trim_start();
			connection.execute(query).unwrap();
		}

		let query = if only_fails {
			"UPDATE results 
			SET pass = :pass, parser_out = :parser_out
			WHERE path = :path"
		} else {
			"INSERT INTO results VALUES (
				:path, :info, :description, :features, :flags, :es5id, :negative, :pass, :parser_out
			)"
		};
		Some(connection.prepare(query).unwrap())
	} else {
		None
	};

	let now = Instant::now();

	// let mut current: std::sync::Arc<std::sync::Mutex<PathBuf>> = std::sync::Arc::default();

	// let other = current.clone();
	// let _ = std::thread::spawn(move || {
	// 	std::thread::sleep(Duration::from_secs(14));
	// 	eprintln!("stuck on {path}", path = other.lock().unwrap().display());
	// });

	if only_fails {
		let query = "SELECT path FROM bad_results";

		let mut paths: Vec<PathBuf> = Vec::new();
		let _ = connection.iterate(query, |pairs| {
			let path: PathBuf = tests_dir.join(pairs[0].1.unwrap());
			paths.push(path);
			true
		});
		for path in paths {
			parse_path(
				&path,
				&mut completed,
				&mut successful,
				statement.as_mut(),
				&mut yaml_parsing,
				&mut parsing,
				tests_dir_prefix,
				only_fails
			);
		}
	} else {
		visit_dirs(&tests_dir, &mut |path| {
			parse_path(
				path,
				&mut completed,
				&mut successful,
				statement.as_mut(),
				&mut yaml_parsing,
				&mut parsing,
				tests_dir_prefix,
				only_fails
			)
		});
	}

	if completed == 0 {
		panic!(
			"no tests run {tests_dir}. check test262 is cloned and not only_fails on 100%",
			tests_dir = tests_dir.display()
		);
	}

	eprintln!();
	eprintln!("--- Results ---");
	eprintln!(
		"Completed {completed} tests in {duration:?} (yaml_parsing={yaml_parsing:?}, parsing={parsing:?}). {successful} successful passes. {errors} fails",
		errors = completed - successful,
		duration = now.elapsed()
	);

	if store_results_in_db || only_fails {
		let query = "SELECT parser_out, COUNT(*) 
			FROM results 
			WHERE pass = 0
			GROUP BY parser_out
			ORDER BY COUNT(*) DESC";

		eprintln!("Breakdown of fails:");
		let _ = connection.iterate(query, |pairs| {
			let reason = pairs[0].1.unwrap();
			let count = pairs[1].1.unwrap();
			eprintln!("{count} recieved {reason:?}");
			true
		});
	}
}

fn visit_dirs(path: &Path, cb: &mut impl FnMut(&Path)) {
	if path.is_dir() {
		for entry in read_dir(path).unwrap() {
			let entry = entry.unwrap();
			let path = entry.path();
			if path.is_dir() {
				visit_dirs(&path, cb);
			} else {
				cb(&path);
			}
		}
	}
}

fn parse_path(
	path: &Path,
	completed: &mut usize,
	successful: &mut usize,
	statement: Option<&mut sqlite::Statement<'_>>,
	yaml_parsing: &mut Duration,
	parsing: &mut Duration,
	tests_dir_prefix: usize,
	only_fails: bool
) {
	if let Some(path) = path.file_name().and_then(std::ffi::OsStr::to_str) {
		if path.contains("_FIXTURE") {
			return;
		}
	}

	let extension = path.extension().and_then(std::ffi::OsStr::to_str);
	if let Some("js") = extension {
		let Ok(source) = read_to_string(path) else {
			eprintln!("Could not read {path}", path = path.display());
			return;
		};

		let (code, metadata) = if let Some(after) = source.strip_prefix("/*---") {
			after.split_once("---*/").unwrap()
		} else {
			let code = source.as_str();
			let metadata = source.split_once("/*---").unwrap().1.split_once("---*/").unwrap().0;
			(code, metadata)
		};

		// Set by metadata
		let mut should_not_parse = false;
		let mut info = None::<&str>;
		let mut description = None::<&str>;
		let mut es5id = None::<&str>;
		let mut features = String::new();
		let flags = String::new();
		let mut parse_options = ezno_parser::ParseOptions::default();
		parse_options.strict_mode = true;
		parse_options.features.run_validation = true;

		// TODO want to run parse x2 here
		let mut only_strict = false;
		let mut non_strict = false;
		let mut module = false;
		let mut tla_feature = false;

		// Parse frontmatter
		{
			let now = std::time::Instant::now();
			let result = simple_yaml_parser::parse(metadata, |key, value| {
				use simple_yaml_parser::YAMLKey::Slice;

				if let &[Slice("negative"), Slice("phase")] = key
					&& let Some("parse") = value.string_value()
				{
					should_not_parse = true;
				}

				// See <https://github.com/tc39/test262/blob/main/INTERPRETING.md#flags>
				if let &[Slice("flags"), _] = key {
					if let Some(value) = value.string_value() {
						match value {
							"onlyStrict" => {
								only_strict = true;
							}
							"module" => {
								module = true;
								only_strict = true;
							}
							"noStrict" => {
								parse_options.strict_mode = false;
								non_strict = true;
							}
							"raw" => {
								parse_options.strict_mode = false;
								non_strict = true;
							} 
							"async" | "CanBlockIsTrue" | "CanBlockIsFalse" | "generated" => {}
							value => {
								panic!("Unknown flag {value:?}");
							}
						}
					}
				}

				if let &[Slice("features"), _] = key {
					if let Some("top-level-await" | "import-defer") = value.string_value() {
						tla_feature = true;
					}
				}

				// Additional information, unrelated to parsing
				// TODO negative.type, locale
				if statement.is_some() {
					match key {
						&[Slice("info")] => {
							info = value.string_value();
						}
						&[Slice("description")] => {
							description = value.string_value();
						}
						&[Slice("es5id")] => {
							es5id = value.string_value();
						}
						&[Slice("features"), _] => {
							if !features.is_empty() {
								features.push(',');
							}
							features.push_str(value.string_value().unwrap_or_default());
						}
						// &[Slice("flags"), _] => {
						// 	if !flags.is_empty() {
						// 		flags.push(',');
						// 	}
						// 	flags.push_str(value.raw_string_value().unwrap_or_default());
						// }
						_ => {}
					}
				}
			});

			// // TODO temp fix because YAML parser broken
			// if es5id == Some("7.7_A2_T6") {
			// 	should_not_parse = true;
			// }

			if let Err(err) = result {
				eprintln!("yaml-parse {path} {err:?}", path = path.display());
				return;
			}

			*yaml_parsing += now.elapsed();

			if module && tla_feature {
				parse_options.top_level_await = true;
			}
		};

		// TODO
		// if let Some(ref mut trace_file) = trace_file {
		// 	writeln!(trace_file, "{path}", path = path.display());
		// }

		let now = std::time::Instant::now();
		let result = <ezno_parser::Module as ezno_parser::ASTNode>::from_string_with_options(
			code.into(),
			parse_options,
		);
		*parsing += now.elapsed();

		let (matched, reason) = match result {
			Ok(_) if should_not_parse => (false, Cow::Borrowed("parsed when should have failed")),
			Err(error) if !should_not_parse => (false, Cow::Owned(error.reason)),
			_ => {
				*successful += 1;
				// TODO should emit -> parse -> emit and check results (roundtrip)
				// TODO should type check
				(true, Cow::Borrowed(""))
			}
		};

		// TODO re-check without strict mode 
		// if only_strict {}

		// if !matched && list_files_with_errors {
		// 	eprintln!("{path}", path = path.display());
		// }

		if let Some(statement) = statement {
			let path: &str = &path.display().to_string()[tests_dir_prefix..];
			let values: &[_] = if only_fails {
				&[
					(":path", path.into()),
					(":pass", (matched as i64).into()),
					(":parser_out", (&*reason).into()),
				]
			} else {
				&[
					(":path", path.into()),
					(":info", info.into()),
					(":description", description.into()),
					(":features", features.into()),
					(":flags", flags.into()),
					(":es5id", es5id.into()),
					(":negative", (should_not_parse as i64).into()),
					(":pass", (matched as i64).into()),
					(":parser_out", (&*reason).into()),
				]
			};

			statement.bind::<&[(_, sqlite::Value)]>(values).expect("Could not bind");

			while let Ok(sqlite::State::Row) = statement.next() {}
			let _ = statement.reset();
		}

		*completed += 1;

		if *completed % 1000 == 0 {
			eprintln!("Completed {completed} tests");
		}
	} else if extension.is_some_and(|extension| extension != ".DS_Store") {
		eprintln!("Not a test file: {path}", path = path.display());
	}
}
