use derive_enum_from_into::EnumFrom;
use source_map::SourceId;

use crate::{
	block::{parse_statements_and_declarations, statements_and_declarations_to_string},
	errors::parse_lexing_error,
	extensions::decorators::decorators_from_reader,
	types::{
		declares::{
			DeclareClassDeclaration, DeclareFunctionDeclaration, DeclareVariableDeclaration,
		},
		namespace::Namespace,
		type_alias::TypeAlias,
		InterfaceDeclaration,
	},
	BlockLike, BlockLikeMut, Decorated, Decorator, ParseOptions, ParseResult, ParsingState,
	StatementOrDeclaration, TSXKeyword, VisitSettings,
};

use super::{lexer, ASTNode, EmptyCursorId, ParseError, Span, TSXToken, Token, TokenReader};
use std::{borrow::Cow, io::Error as IOError};

#[cfg(not(target_family = "wasm"))]
use std::{fs, path::Path};

#[derive(Debug, EnumFrom)]
pub enum FromFileError {
	FileError(IOError),
	ParseError(ParseError),
}

#[derive(Debug, Clone)]
pub struct Module {
	pub items: Vec<StatementOrDeclaration>,
	pub source: SourceId,
}

impl PartialEq for Module {
	fn eq(&self, other: &Self) -> bool {
		self.items == other.items
	}
}

impl ASTNode for Module {
	fn to_string_from_buffer<T: source_map::ToString>(
		&self,
		buf: &mut T,
		settings: &crate::ToStringOptions,
		depth: u8,
	) {
		statements_and_declarations_to_string(&self.items, buf, settings, depth)
	}

	fn get_position(&self) -> Cow<Span> {
		Cow::Owned(
			self.items
				.first()
				.unwrap()
				.get_position()
				.union(&self.items.last().unwrap().get_position()),
		)
	}

	fn from_reader(
		reader: &mut impl TokenReader<TSXToken, Span>,
		state: &mut crate::ParsingState,
		settings: &ParseOptions,
	) -> ParseResult<Self> {
		parse_statements_and_declarations(reader, state, settings).map(|statements| {
			// TODO null bad
			let source =
				statements.last().map(|stmt| stmt.get_position().source).unwrap_or(SourceId::NULL);
			Module { source, items: statements }
		})
	}
}

impl Module {
	pub fn to_string_with_source_map(
		&self,
		settings: &crate::ToStringOptions,
		fs: &impl source_map::FileSystem,
	) -> (String, source_map::SourceMap) {
		let mut buf = source_map::StringWithSourceMap::new();
		self.to_string_from_buffer(&mut buf, settings, 0);
		buf.build(fs)
	}

	pub fn length(&self, settings: &crate::ToStringOptions) -> usize {
		let mut buf = source_map::Counter::new();
		self.to_string_from_buffer(&mut buf, settings, 0);
		buf.get_count()
	}

	#[cfg(not(target_family = "wasm"))]
	pub fn from_file(
		path: impl AsRef<Path>,
		settings: ParseOptions,
		cursors: Vec<(usize, EmptyCursorId)>,
		fs: &mut impl source_map::FileSystem,
	) -> Result<Self, FromFileError> {
		let source = fs::read_to_string(&path)?;
		let source_id = SourceId::new(fs, path.as_ref().to_path_buf(), source.clone());
		Self::from_string(source, settings, source_id, None, cursors).map_err(Into::into)
	}
}

impl Module {
	pub fn visit<TData>(
		&self,
		visitors: &mut (impl crate::VisitorReceiver<TData> + ?Sized),
		data: &mut TData,
		settings: &VisitSettings,
	) {
		use crate::visiting::Visitable;
		let mut chain = crate::Chain::new_with_initial(crate::ChainVariable::Module(self.source));
		let mut chain = temporary_annex::Annex::new(&mut chain);

		{
			visitors.visit_block(&mut crate::block::BlockLike { items: &self.items }, data, &chain);
		}

		let iter = self.items.iter();
		if settings.reverse_statements {
			iter.for_each(|item| item.visit(visitors, data, settings, &mut chain));
		} else {
			iter.rev().for_each(|item| item.visit(visitors, data, settings, &mut chain));
		}
	}

	pub fn visit_mut<TData>(
		&mut self,
		visitors: &mut (impl crate::VisitorMutReceiver<TData> + ?Sized),
		data: &mut TData,
		settings: &VisitSettings,
	) {
		use crate::visiting::Visitable;
		let mut chain = crate::Chain::new_with_initial(crate::ChainVariable::Module(self.source));
		let mut chain = temporary_annex::Annex::new(&mut chain);

		{
			visitors.visit_block_mut(
				&mut crate::block::BlockLikeMut { items: &mut self.items },
				data,
				&chain,
			);
		}

		let iter_mut = self.items.iter_mut();
		if settings.reverse_statements {
			iter_mut.for_each(|item| item.visit_mut(visitors, data, settings, &mut chain));
		} else {
			iter_mut.rev().for_each(|item| item.visit_mut(visitors, data, settings, &mut chain));
		}
	}
}

impl<'a> From<&'a Module> for BlockLike<'a> {
	fn from(module: &'a Module) -> Self {
		BlockLike { items: &module.items }
	}
}

impl<'a> From<&'a mut Module> for BlockLikeMut<'a> {
	fn from(module: &'a mut Module) -> Self {
		BlockLikeMut { items: &mut module.items }
	}
}

/// Statements for '.d.ts' files
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum TypeDefinitionModuleDeclaration {
	Variable(DeclareVariableDeclaration),
	Function(DeclareFunctionDeclaration),
	Class(DeclareClassDeclaration),
	Interface(Decorated<InterfaceDeclaration>),
	TypeAlias(TypeAlias),
	Namespace(Namespace),
	/// Information for upcoming declaration
	Comment(String),
	/// Local alias, not exported from module. Does not start with declare
	LocalTypeAlias(TypeAlias),
	// Variable without declare in front. Found in namespaces
	LocalVariableDeclaration(DeclareVariableDeclaration),
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct TypeDefinitionModule {
	pub declarations: Vec<TypeDefinitionModuleDeclaration>,
}

impl TypeDefinitionModule {
	fn from_reader(
		reader: &mut impl TokenReader<TSXToken, Span>,
		state: &mut crate::ParsingState,
		settings: &ParseOptions,
	) -> ParseResult<Self> {
		let mut declarations = Vec::new();
		loop {
			declarations
				.push(TypeDefinitionModuleDeclaration::from_reader(reader, state, settings)?);
			match reader.peek() {
				Some(Token(TSXToken::SemiColon, _)) => {
					reader.next();
					if matches!(reader.peek().unwrap().0, TSXToken::EOS) {
						break;
					}
				}
				Some(Token(TSXToken::EOS, _)) => {
					break;
				}
				_ => {
					// TODO
				}
			}
		}
		Ok(Self { declarations })
	}

	#[cfg(target_family = "wasm")]
	pub fn from_string(
		source: String,
		settings: ParseOptions,
		source: SourceId,
		cursors: Vec<(usize, EmptyCursorId)>,
	) -> ParseResult<(Self, ParsingState)> {
		// TODO this should be covered by settings
		let lex_settings = lexer::LexSettings {
			// Unfortunately some comments contain data (variable ids)
			include_comments: true,
			// Important not to parse JSX as <> is used for casting
			lex_jsx: false,
			..Default::default()
		};

		// Extension which includes pulling in the string as source
		let mut queue = tokenizer_lib::BufferedTokenQueue::new();
		lexer::lex_source(&source, &mut queue, &lex_settings, Some(source), None, cursors)?;

		let mut state = ParsingState::default();
		let res = Self::from_reader(&mut queue, &mut state, &settings);
		if res.is_ok() {
			queue.expect_next(TSXToken::EOS)?;
		}

		res.map(|ast| (ast, state))
	}

	#[cfg(not(target_family = "wasm"))]
	pub fn from_string(
		source: String,
		settings: ParseOptions,
		source_id: SourceId,
		cursors: Vec<(usize, EmptyCursorId)>,
	) -> ParseResult<(Self, ParsingState)> {
		use source_map::LineStarts;
		use std::thread;
		use tokenizer_lib::ParallelTokenQueue;

		// Extension which includes pulling in the string as source
		let (mut sender, mut reader) = ParallelTokenQueue::new();
		let line_starts = LineStarts::new(source.as_str());

		let parsing_thread = thread::spawn(move || {
			let mut state = ParsingState { line_starts };
			let res = Self::from_reader(&mut reader, &mut state, &settings);
			match res {
				Ok(ast) => {
					reader.expect_next(TSXToken::EOS)?;
					Ok((ast, state))
				}
				Err(err) => Err(err),
			}
		});
		let lex_settings = lexer::LexSettings {
			// Unfortunately some comments contain data (variable ids)
			include_comments: true,
			// Important not to parse JSX as <> is used for casting
			lex_jsx: false,
			..Default::default()
		};
		lexer::lex_source(&source, &mut sender, &lex_settings, Some(source_id), None, cursors)?;
		drop(sender);

		parsing_thread.join().expect("Parsing panicked")
	}

	#[cfg(not(target_family = "wasm"))]
	pub fn from_file(
		path: impl AsRef<Path>,
		settings: ParseOptions,
		cursors: Vec<(usize, EmptyCursorId)>,
		fs: &mut impl source_map::FileSystem,
	) -> Result<(Self, ParsingState), FromFileError> {
		let source = fs::read_to_string(&path)?;
		let source_id = SourceId::new(fs, path.as_ref().to_path_buf(), source.clone());
		Self::from_string(source, settings, source_id, cursors).map_err(Into::into)
	}
}

impl ASTNode for TypeDefinitionModuleDeclaration {
	fn from_reader(
		reader: &mut impl TokenReader<TSXToken, Span>,
		state: &mut crate::ParsingState,
		settings: &ParseOptions,
	) -> ParseResult<Self> {
		let decorators = decorators_from_reader(reader, state, settings)?;
		match reader.peek().ok_or_else(parse_lexing_error)? {
			Token(TSXToken::Keyword(TSXKeyword::Declare), _) => {
				let declare_span = reader.next().unwrap().1;
				parse_declare_item(reader, state, settings, decorators, declare_span)
			}
			Token(TSXToken::Keyword(TSXKeyword::Interface), _) => {
				let on = InterfaceDeclaration::from_reader(reader, state, settings)?;
				Ok(TypeDefinitionModuleDeclaration::Interface(Decorated { decorators, on }))
			}
			Token(TSXToken::Keyword(TSXKeyword::Type), _) => {
				Ok(TypeDefinitionModuleDeclaration::LocalTypeAlias(TypeAlias::from_reader(
					reader, state, settings,
				)?))
			}
			Token(TSXToken::Keyword(TSXKeyword::Var), _) => {
				Ok(TypeDefinitionModuleDeclaration::LocalVariableDeclaration(
					DeclareVariableDeclaration::from_reader_sub_declare(
						reader, state, settings, None, decorators,
					)?,
				))
			}
			Token(TSXToken::Comment(_), _) | Token(TSXToken::MultiLineComment(_), _) => {
				let comment = match reader.next().unwrap().0 {
					// TODO loses multiline/single-line data
					TSXToken::MultiLineComment(comment) | TSXToken::Comment(comment) => comment,
					_ => unreachable!(),
				};
				Ok(TypeDefinitionModuleDeclaration::Comment(comment))
			}
			_ => {
				let Token(token, position) = reader.next().unwrap();
				Err(ParseError::new(
					crate::ParseErrors::UnexpectedToken {
						expected: &[
							TSXToken::Keyword(TSXKeyword::Declare),
							TSXToken::Keyword(TSXKeyword::Interface),
							TSXToken::Keyword(TSXKeyword::Type),
							TSXToken::Keyword(TSXKeyword::Var),
							TSXToken::At,
						],
						found: token,
					},
					position,
				))
			}
		}
	}

	fn to_string_from_buffer<T: source_map::ToString>(
		&self,
		_buf: &mut T,
		_settings: &crate::ToStringOptions,
		_depth: u8,
	) {
		todo!("tdms to_string_from_buffer");
	}

	fn get_position(&self) -> Cow<Span> {
		todo!("tdms get_position");
	}
}

pub(crate) fn parse_declare_item(
	reader: &mut impl TokenReader<TSXToken, Span>,
	state: &mut crate::ParsingState,
	settings: &ParseOptions,
	decorators: Vec<Decorator>,
	declare_span: Span,
) -> Result<TypeDefinitionModuleDeclaration, ParseError> {
	match reader.peek().unwrap() {
		Token(TSXToken::Keyword(TSXKeyword::Var), _) => {
			Ok(TypeDefinitionModuleDeclaration::Variable(
				DeclareVariableDeclaration::from_reader_sub_declare(
					reader,
					state,
					settings,
					Some(declare_span),
					decorators,
				)?,
			))
		}
		Token(TSXToken::Keyword(TSXKeyword::Class), _) => {
			Ok(TypeDefinitionModuleDeclaration::Class(
				DeclareClassDeclaration::from_reader_sub_declare(reader, state, settings)?,
			))
		}
		Token(TSXToken::Keyword(TSXKeyword::Function), _) => {
			Ok(TypeDefinitionModuleDeclaration::Function(
				DeclareFunctionDeclaration::from_reader_sub_declare_with_decorators(
					reader, state, settings, decorators,
				)?,
			))
		}
		Token(TSXToken::Keyword(TSXKeyword::Type), _) => {
			Ok(TypeDefinitionModuleDeclaration::TypeAlias(TypeAlias::from_reader(
				reader, state, settings,
			)?))
		}
		Token(TSXToken::Keyword(TSXKeyword::Namespace), _) => {
			Ok(TypeDefinitionModuleDeclaration::Namespace(Namespace::from_reader(
				reader, state, settings,
			)?))
		}
		_ => {
			let Token(token, position) = reader.next().unwrap();
			Err(ParseError::new(
				crate::ParseErrors::UnexpectedToken {
					expected: &[
						TSXToken::Keyword(TSXKeyword::Var),
						TSXToken::Keyword(TSXKeyword::Class),
						TSXToken::Keyword(TSXKeyword::Type),
						TSXToken::Keyword(TSXKeyword::Namespace),
						TSXToken::Keyword(TSXKeyword::Function),
					],
					found: token,
				},
				position,
			))
		}
	}
}
