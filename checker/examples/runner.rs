use std::{
	path::{Path, PathBuf},
};

use ezno_checker::{check_project,
	synthesis::EznoParser,
	TypeCheckOptions,INTERNAL_DEFINITION_FILE_PATH
};

fn main() {
    let mut arguments = std::env::args().skip(1);

    let arg = arguments.next();
	assert_eq!(arg.as_deref(), Some("--interactive"), "runner must be run with --interactive");
	run_interactive();
}

const SIMPLE_DTS: Option<&str> = None;

// const IN_CI: bool = option_env!("CI").is_some();

fn run_interactive() {
    use std::io::{BufRead, stdin};
    let stdin = stdin();
    let mut buf = Vec::new();

    println!("start");

    for line in stdin.lock().lines().map_while(Result::ok) {
        if line == "close" {
            if !buf.is_empty() {
                eprintln!("no end to message {buf:?}");
            }
            break;
        }

        if line == "end" {
            let source = String::from_utf8_lossy(&buf);
            {
                let mut multiple = false;
                let mut type_check_options = TypeCheckOptions::default();
                let source: &str = if let Some((options, source)) = source.split_once("\n---") {
                    // TODO more
                    for part in options.split(',') {
                        let part = part.trim();
                        match part {
                            "advanced_numbers" => {
                                type_check_options.advanced_numbers = true;
                            }
                            "multiple" => {
                                multiple = true;
                            }
                            flag => {
                                panic!("unknown flag {flag:?}");
                            }
                        }
                    }
                    source
                } else {
                    &source
                };

                let definition_file_name: PathBuf = if SIMPLE_DTS.is_some() {
                    "./checker/definitions/simple.d.ts".into()
                } else {
                    INTERNAL_DEFINITION_FILE_PATH.into()
                };
                let type_definition_files = vec![definition_file_name.clone()];

                let code: Vec<(String, String)> = if multiple {
                    let mut current = "main.tsx".to_owned();
                    let mut code = Vec::new();
                    let mut buf = String::new();
                    for line in source.lines() {
                        if let Some(location) = line.strip_prefix("// in ") {
                            code.push((current, std::mem::take(&mut buf)));
                            current = location.to_owned();
                        } else {
                            buf.push_str(line);
                            buf.push_str("\n");
                        }
                    }
                    code.push((current, buf));
                    code
                } else {
                    vec![("main.tsx".to_owned(), source.to_owned())]
                };

                let resolver = |path: &Path| -> Option<Vec<u8>> {
                    if path == definition_file_name.as_path() {
                        Some(SIMPLE_DTS.unwrap().to_owned().into_bytes())
                    } else if code.len() == 1 {
                        Some(code[0].1.to_owned().into())
                    } else {
                        code.iter()
                            .find_map(|(code_path, content)| {
                                (std::path::Path::new(code_path) == path)
                                    .then_some(content.to_owned().to_owned())
                            })
                            .map(Into::into)
                    }
                };

                let result = check_project::<_, EznoParser>(
                    vec![PathBuf::from("main.tsx")],
                    type_definition_files,
                    &resolver,
                    type_check_options,
                    (),
                    None,
                );

                for diagnostic in result.diagnostics {
                    let message = diagnostic.reason();
                    println!("- {message}");
                }
            }
            println!("end");
            buf.clear();
            continue;
        }

        buf.extend_from_slice(line.as_bytes());
        buf.push(b'\n');
    }
}
