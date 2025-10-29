use std::borrow::Cow;
use std::fs::{create_dir, read_dir, read_to_string};
use std::path::Path;
use std::time::{Duration, Instant};

const ADD_TO_DB: bool = true;

const ROOT: &str = env!("CARGO_MANIFEST_DIR");

#[allow(unused_mut)]
fn main() {
	let root = Path::new(ROOT);
	let tests_dir = root.join("test262/test");

	let mut completed = 0;
	let mut successful = 0;

	let mut yaml_parsing = Duration::default();
	let mut parsing = Duration::default();

	let db_file = root.join("out/results.db");
	let _ = create_dir(db_file.parent().unwrap());
	let connection = sqlite::open(&db_file).unwrap();

	// clean up existing results
	connection.execute("DROP TABLE IF EXISTS results;").unwrap();

	let query = "
CREATE TABLE results (
    path        TEXT PRIMARY KEY,
    info        TEXT,
    description TEXT,
    features    TEXT,
    flags       TEXT,
    es5id       TEXT,
    negative    INTEGER NOT NULL,
    code        TEXT,
    pass        INTEGER NOT NULL,
    parser_out  TEXT
);".trim_start();

	connection.execute(query).unwrap();

	let query = "INSERT INTO results VALUES (
        :path, :info, :description, :features, :flags, :es5id, :negative, :code, :pass, :parser_out
    )";
	let mut statement = connection.prepare(query).unwrap();

	let now = Instant::now();

	// let mut current: std::sync::Arc<std::sync::Mutex<PathBuf>> = std::sync::Arc::default();

	// let other = current.clone();
	// let _ = std::thread::spawn(move || {
	// 	std::thread::sleep(Duration::from_secs(14));
	// 	eprintln!("stuck on {path}", path = other.lock().unwrap().display());
	// });

	visit_dirs(&tests_dir, &mut |path| {
		if let Some(path) = path.file_name().and_then(std::ffi::OsStr::to_str) {
			if path.contains("_FIXTURE") {
				return;
			}
		}

		if let Some("js") = path.extension().and_then(std::ffi::OsStr::to_str) {
			let Ok(source) = read_to_string(path) else {
				eprintln!("Could not read {path}", path = path.display());
				return;
			};

			let Some(start) = source.find("/*---") else {
				eprintln!("No '/*---' under {path}", path = path.display());
				return;
			};
			let start = start + "/*---".len();

			let remaining = &source[start..];
			let Some(end) = remaining.find("---*/") else {
				eprintln!("No '---*/' under {path}", path = path.display());
				return;
			};

			let metadata = &remaining[..end];

			// {
			// 	*current.lock().unwrap() = path.to_path_buf();
			// }

			let end = end + "---*/".len();
			let code = &remaining[end..];

			// Set by metadata
			let mut should_not_parse = false;
			let mut info = None::<&str>;
			let mut description = None::<&str>;
			let mut es5id = None::<&str>;
			let mut features = None::<String>;
			let mut flags = None::<String>;

			// Parse frontmatter
			{
				let now = std::time::Instant::now();
				let result = simple_yaml_parser::parse(metadata, |key, value| {
					use simple_yaml_parser::{YAMLKey::Slice, RootYAMLValue};

					if let &[Slice("negative"), Slice("phase")] = key && let RootYAMLValue::String("parse") = value {
						should_not_parse = true;
					}

					// TODO negative.type, locale
					if ADD_TO_DB {
						match key {
							&[Slice("info")] => {
								info = value.raw_string_value();
							}
							&[Slice("description")] => {
								description = value.raw_string_value();
							}
							&[Slice("es5id")] => {
								es5id = value.raw_string_value();
							}
							&[Slice("features"), _] => {
								let f = features.get_or_insert_default();
								if !f.is_empty() {
									f.push(',');
								}
								f.push_str(value.raw_string_value().unwrap_or_default());
							}
							&[Slice("flags"), _] => {
								let f = flags.get_or_insert_default();
								if !f.is_empty() {
									f.push(',');
								}
								f.push_str(value.raw_string_value().unwrap_or_default());
							}
							_ => {}
						}
					}
				});

				if let Err(err) = result {
					eprintln!("yaml-parse {path} {err:?}", path = path.display());
					return;
				}

				yaml_parsing += now.elapsed();
			};

			// TODO
			// if let Some(ref mut trace_file) = trace_file {
			// 	writeln!(trace_file, "{path}", path = path.display());
			// }

			let now = std::time::Instant::now();
			let options = Default::default();
			let result = <ezno_parser::Module as ezno_parser::ASTNode>::from_string_with_options(
				code.into(),
				options,
				None,
			);
			parsing += now.elapsed();

			let (matched, reason) = match result {
				Ok(_) if should_not_parse => {
					(false, Cow::Borrowed("parsed when should have failed"))
				}
				Err(error) if !should_not_parse => (false, Cow::Owned(error.reason)),
				_ => {
					successful += 1;
					// TODO should emit -> parse -> emit and check results (roundtrip)
					// TODO should type check
					(true, Cow::Borrowed(""))
				}
			};

			if ADD_TO_DB {
				let values = &[
					(":path", path.display().to_string().into()),
					(":info", info.into()),
					(":description", description.into()),
					(":features", features.into()),
					(":flags", flags.into()),
					(":es5id", es5id.into()),
					(":negative", (should_not_parse as i64).into()),
					// space saving measure
					(":code", (if matched { None } else { Some(code) }).into()),
					(":pass", (matched as i64).into()),
					(":parser_out", (&*reason).into()),
				];

				statement.bind::<&[(_, sqlite::Value)]>(values).expect("Could not bind");

				while let Ok(sqlite::State::Row) = statement.next() {}
				let _ = statement.reset();
			}

			completed += 1;

			if completed % 1000 == 0 {
				eprintln!("Completed {completed} tests");
			}
		} else {
			eprintln!("Not a test file: {path}", path = path.display());
		}
	});

	if completed == 0 {
		panic!("no tests under {tests_dir}. check test262 is cloned", tests_dir=tests_dir.display());
	}

	eprintln!();
	eprintln!("--- Results ---");
	eprintln!(
		"Completed {completed} tests in {duration:?} (yaml_parsing={yaml_parsing:?}, parsing={parsing:?}). {successful} successful passes. {errors} fails",
		errors = completed - successful,
		duration = now.elapsed()
	);

	{
		let query =
			"SELECT parser_out, COUNT(*) 
			FROM results 
			WHERE pass = 0
			GROUP BY parser_out
			ORDER BY COUNT(*) DESC";

		eprintln!("Breakdown of fails:");
		let _ = connection
			.iterate(query, |pairs| {
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
