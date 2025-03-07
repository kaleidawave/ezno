use std::borrow::Cow;
use std::fs::{create_dir, read_dir, read_to_string};
use std::path::{Path, PathBuf};

#[allow(unused_mut)]
fn main() {
	let path: &Path = &PathBuf::from("./test262/test");

	let mut completed = 0;
	let mut successful = 0;

	let add_to_db = true;

	let now = std::time::Instant::now();
	let mut yaml_parsing = std::time::Duration::default();
	let mut parsing = std::time::Duration::default();

	let _ = create_dir("out");
	let connection = sqlite::open("out/database.db").unwrap();

	let query = "CREATE TABLE IF NOT EXISTS results (
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
);";
	connection.execute(query).unwrap();

	let query = "INSERT INTO results VALUES (
        :path, :info, :description, :features, :flags, :es5id, :negative, :code, :pass, :parser_out
    )";
	let mut statement = connection.prepare(query).unwrap();

	visit_dirs(path, &mut |path| {
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

			let end = end + "---*/".len();
			let code = &remaining[end..];

			// Set by metadata
			let mut should_not_parse = false;
			let mut info = None::<&str>;
			let mut description = None::<&str>;
			let mut es5id = None::<&str>;
			let mut features = None::<String>;
			let mut flags = None::<String>;

			{
				let now = std::time::Instant::now();
				let result = simple_yaml_parser::parse(metadata, |key, value| {
					use simple_yaml_parser::YAMLKey::Slice;

					// TODO description. negative.type, flags, locale
					if let (
						&[Slice("negative"), Slice("phase")],
						simple_yaml_parser::RootYAMLValue::String("parse"),
					) = (key, &value)
					{
						should_not_parse = true;
					}

					if add_to_db {
						if let [Slice("info")] = key {
							info = value.raw_string_value();
						} else if let [Slice("description")] = key {
							description = value.raw_string_value();
						} else if let [Slice("es5id")] = key {
							es5id = value.raw_string_value();
						} else if let [Slice("features"), _] = key {
							let f = features.get_or_insert_default();
							if !f.is_empty() {
								f.push(',');
							}
							f.push_str(value.raw_string_value().unwrap_or_default());
						} else if let [Slice("flags"), _] = key {
							let f = flags.get_or_insert_default();
							if !f.is_empty() {
								f.push(',');
							}
							f.push_str(value.raw_string_value().unwrap_or_default());
						}
					}
				});

				if let Err(err) = result {
					eprintln!("yaml-parse {path} {err:?}", path = path.display());
					return;
				}

				yaml_parsing += now.elapsed();
			};

			let now = std::time::Instant::now();
			let result = <ezno_parser::Module as ezno_parser::ASTNode>::from_string_with_options(
				code.into(),
				Default::default(),
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

			if add_to_db {
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

	eprintln!(
		"Completed {completed} tests in {duration:?} (yaml_parsing={yaml_parsing:?}, parsing={parsing:?}). {successful} successful passes. {errors} fails",
		errors = completed - successful,
		duration = now.elapsed()
	);

	{
		let query = "SELECT cast(SUM(pass) AS FLOAT) / COUNT(*) FROM results;";
		eprintln!("Results '{query}'");
		connection
			.iterate(query, |pairs| {
				for &(name, value) in pairs.iter() {
					eprintln!("Out {} = {}", name, value.unwrap());
				}
				true
			})
			.unwrap();
	}
	{
		let query =
			"SELECT parser_out, COUNT(*) FROM results GROUP BY parser_out ORDER BY COUNT(*) DESC";
		eprintln!("Breakdown of fails '{query}'");
		connection
			.iterate(query, |pairs| {
				for &(name, value) in pairs.iter() {
					eprintln!("Out {} = {}", name, value.unwrap());
				}
				true
			})
			.unwrap();
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
