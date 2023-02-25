use derive_enum_from_into::EnumFrom;
use source_map::SourceId;
use temporary_annex::Annex;

use crate::{
	errors::parse_lexing_error,
	extensions::decorators::decorators_from_reader,
	extractor::ExtractedFunctions,
	statements::{
		parse_statements, statements_to_string, DeclareClassDeclaration,
		DeclareFunctionDeclaration, DeclareVariableDeclaration, InterfaceDeclaration, Namespace,
		TypeAlias,
	},
	BlockId, BlockLike, BlockLikeMut, Chain, ChainVariable, Decorated, Decorator, ParseResult,
	ParseSettings, ParsingState, Statement, TSXKeyword, VisitSettings, Visitable,
};

use super::{lexer, ASTNode, EmptyCursorId, ParseError, Span, TSXToken, Token, TokenReader};
use crate::ParseOutput;
use std::{borrow::Cow, io::Error as IOError};

#[cfg(not(target_family = "wasm"))]
use std::{fs, path::Path};

#[derive(Debug, EnumFrom)]
pub enum FromFileError {
	FileError(IOError),
	ParseError(ParseError),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Module {
	pub statements: Vec<Statement>,
	pub block_id: BlockId,
	pub source_id: SourceId,
}

impl ASTNode for Module {
	fn to_string_from_buffer<T: source_map::ToString>(
		&self,
		buf: &mut T,
		settings: &crate::ToStringSettingsAndData,
		depth: u8,
	) {
		statements_to_string(&self.statements, buf, settings, depth)
	}

	fn get_position(&self) -> Cow<Span> {
		Cow::Owned(
			self.statements
				.first()
				.unwrap()
				.get_position()
				.union(&self.statements.last().unwrap().get_position()),
		)
	}

	fn from_reader(
		reader: &mut impl TokenReader<TSXToken, Span>,
		state: &mut crate::ParsingState,
		settings: &ParseSettings,
	) -> ParseResult<Self> {
		parse_statements(reader, state, settings).map(|(statements, block_id)| {
			// TODO null bad
			let source_id = statements
				.last()
				.map(|stmt| stmt.get_position().source_id)
				.unwrap_or(SourceId::NULL);
			Module { source_id, statements, block_id }
		})
	}
}

impl Module {
	pub fn to_string_with_source_map(
		&self,
		settings: &crate::ToStringSettingsAndData,
	) -> (String, String) {
		let mut buf = source_map::StringWithSourceMap::new();
		self.to_string_from_buffer(&mut buf, settings, 0);
		buf.build()
	}

	pub fn length(&self, settings: &crate::ToStringSettingsAndData) -> usize {
		let mut buf = source_map::Counter::new();
		self.to_string_from_buffer(&mut buf, settings, 0);
		buf.get_count()
	}

	#[cfg(not(target_family = "wasm"))]
	pub fn from_file(
		path: impl AsRef<Path>,
		settings: ParseSettings,
		cursors: Vec<(usize, EmptyCursorId)>,
	) -> Result<ParseOutput<Self>, FromFileError> {
		let source = fs::read_to_string(&path)?;
		let source_id = SourceId::new(path.as_ref().to_path_buf(), source.clone());
		Self::from_string(source, settings, source_id, None, cursors).map_err(Into::into)
	}
}

impl Module {
	pub fn visit<TData>(
		&self,
		visitors: &mut (impl crate::VisitorReceiver<TData> + ?Sized),
		data: &mut TData,
		functions: &mut ExtractedFunctions,
		settings: &VisitSettings,
	) {
		let mut chain =
			Chain::new_with_initial(ChainVariable::UnderModule(self.block_id, self.source_id));

		let mut chain = Annex::new(&mut chain);

		visitors.visit_block(&crate::block::BlockLike::from(self), data, functions, &chain);

		let iter = self.statements.iter();
		if settings.reverse_statements {
			iter.rev().for_each(|statement| {
				statement.visit(visitors, data, settings, functions, &mut chain)
			});
		} else {
			iter.for_each(|statement| {
				statement.visit(visitors, data, settings, functions, &mut chain)
			});
		}
	}

	pub fn visit_mut<TData>(
		&mut self,
		visitors: &mut (impl crate::VisitorMutReceiver<TData> + ?Sized),
		data: &mut TData,
		functions: &mut ExtractedFunctions,
		settings: &VisitSettings,
	) {
		let mut chain =
			Chain::new_with_initial(ChainVariable::UnderModule(self.block_id, self.source_id));

		let mut chain = Annex::new(&mut chain);

		{
			visitors.visit_block_mut(
				&mut crate::block::BlockLikeMut {
					block_id: self.block_id,
					statements: &mut self.statements,
				},
				data,
				functions,
				&chain,
			);
		}

		let iter_mut = self.statements.iter_mut();
		if settings.reverse_statements {
			iter_mut.for_each(|statement| {
				statement.visit_mut(visitors, data, settings, functions, &mut chain)
			});
		} else {
			iter_mut.rev().for_each(|statement| {
				statement.visit_mut(visitors, data, settings, functions, &mut chain)
			});
		}
	}
}

impl<'a> From<&'a Module> for BlockLike<'a> {
	fn from(module: &'a Module) -> Self {
		BlockLike { block_id: module.block_id, statements: &module.statements }
	}
}

impl<'a> From<&'a mut Module> for BlockLikeMut<'a> {
	fn from(module: &'a mut Module) -> Self {
		BlockLikeMut { block_id: module.block_id, statements: &mut module.statements }
	}
}

/// Statements for '.d.ts' files
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum TypeDefinitionModuleStatement {
	VariableDeclaration(DeclareVariableDeclaration),
	FunctionDeclaration(DeclareFunctionDeclaration),
	ClassDeclaration(DeclareClassDeclaration),
	InterfaceDeclaration(Decorated<InterfaceDeclaration>),
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
	pub declarations: Vec<TypeDefinitionModuleStatement>,
}

impl TypeDefinitionModule {
	fn from_reader(
		reader: &mut impl TokenReader<TSXToken, Span>,
		state: &mut crate::ParsingState,
		settings: &ParseSettings,
	) -> ParseResult<Self> {
		let mut declarations = Vec::new();
		loop {
			declarations.push(TypeDefinitionModuleStatement::from_reader(reader, state, settings)?);
			match reader.peek().unwrap().0 {
				TSXToken::SemiColon => {
					reader.next();
					if matches!(reader.peek().unwrap().0, TSXToken::EOS) {
						break;
					}
				}
				TSXToken::EOS => {
					break;
				}
				_ => {}
			}
		}
		Ok(Self { declarations })
	}

	#[cfg(target_family = "wasm")]
	pub fn from_string(
		source: String,
		settings: ParseSettings,
		source_id: SourceId,
		cursors: Vec<(usize, EmptyCursorId)>,
	) -> ParseResult<Self> {
		// TODO this should be covered by settings
		let lex_settings = lexer::LexSettings {
			// Unfortunately some comments contain data (variable ids)
			include_comments: true,
			// Important not to parse JSX as <> is used for casting
			lex_jsx: false,
			..Default::default()
		};

		// Extension which includes pulling in the string as source
		let mut channel = tokenizer_lib::BufferedTokenQueue::new();
		lexer::lex_source(&source, &mut channel, &lex_settings, Some(source_id), None, cursors)?;
		Self::from_reader(&mut channel, &settings)
	}

	#[cfg(not(target_family = "wasm"))]
	pub fn from_string(
		source: String,
		settings: ParseSettings,
		source_id: SourceId,
		cursors: Vec<(usize, EmptyCursorId)>,
	) -> ParseResult<(Self, ParsingState)> {
		use std::thread;
		use tokenizer_lib::ParallelTokenQueue;

		// Extension which includes pulling in the string as source
		let (mut sender, mut reader) = ParallelTokenQueue::new();
		let parsing_thread = thread::spawn(move || {
			let mut state = ParsingState::default();
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
		settings: ParseSettings,
		cursors: Vec<(usize, EmptyCursorId)>,
	) -> Result<(Self, ParsingState), FromFileError> {
		let source = fs::read_to_string(&path)?;
		let source_id = SourceId::new(path.as_ref().to_path_buf(), source.clone());
		Self::from_string(source, settings, source_id, cursors).map_err(Into::into)
	}
}

impl ASTNode for TypeDefinitionModuleStatement {
	fn from_reader(
		reader: &mut impl TokenReader<TSXToken, Span>,
		state: &mut crate::ParsingState,
		settings: &ParseSettings,
	) -> ParseResult<Self> {
		let decorators = decorators_from_reader(reader, state, settings)?;
		match reader.peek().ok_or_else(parse_lexing_error)? {
			Token(TSXToken::Keyword(TSXKeyword::Declare), _) => {
				let declare_span = reader.next().unwrap().1;
				parse_declare_item(reader, state, settings, decorators, declare_span)
			}
			Token(TSXToken::Keyword(TSXKeyword::Interface), _) => {
				let on = InterfaceDeclaration::from_reader(reader, state, settings)?;
				Ok(TypeDefinitionModuleStatement::InterfaceDeclaration(Decorated {
					decorators,
					on,
				}))
			}
			Token(TSXToken::Keyword(TSXKeyword::Type), _) => {
				Ok(TypeDefinitionModuleStatement::LocalTypeAlias(TypeAlias::from_reader(
					reader, state, settings,
				)?))
			}
			Token(TSXToken::Keyword(TSXKeyword::Var), _) => {
				Ok(TypeDefinitionModuleStatement::LocalVariableDeclaration(
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
				Ok(TypeDefinitionModuleStatement::Comment(comment))
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
		_settings: &crate::ToStringSettingsAndData,
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
	settings: &ParseSettings,
	decorators: Vec<Decorator>,
	declare_span: Span,
) -> Result<TypeDefinitionModuleStatement, ParseError> {
	match reader.peek().unwrap() {
		Token(TSXToken::Keyword(TSXKeyword::Var), _) => {
			Ok(TypeDefinitionModuleStatement::VariableDeclaration(
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
			Ok(TypeDefinitionModuleStatement::ClassDeclaration(
				DeclareClassDeclaration::from_reader_sub_declare(reader, state, settings)?,
			))
		}
		Token(TSXToken::Keyword(TSXKeyword::Function), _) => {
			Ok(TypeDefinitionModuleStatement::FunctionDeclaration(
				DeclareFunctionDeclaration::from_reader_sub_declare_with_decorators(
					reader, state, settings, decorators,
				)?,
			))
		}
		Token(TSXToken::Keyword(TSXKeyword::Type), _) => {
			Ok(TypeDefinitionModuleStatement::TypeAlias(TypeAlias::from_reader(
				reader, state, settings,
			)?))
		}
		Token(TSXToken::Keyword(TSXKeyword::Namespace), _) => {
			Ok(TypeDefinitionModuleStatement::Namespace(Namespace::from_reader(
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
