use source_map::SourceId;
use tokenizer_lib::sized_tokens::TokenStart;

use crate::{
	block::{parse_statements_and_declarations, statements_and_declarations_to_string},
	errors::parse_lexing_error,
	extensions::decorators::decorators_from_reader,
	throw_unexpected_token_with_token,
	types::{
		declares::{
			DeclareClassDeclaration, DeclareFunctionDeclaration, DeclareVariableDeclaration,
		},
		namespace::Namespace,
		type_alias::TypeAlias,
		InterfaceDeclaration,
	},
	BlockLike, BlockLikeMut, Decorated, Decorator, ParseOptions, ParseResult,
	StatementOrDeclaration, TSXKeyword, VisitSettings,
};

use super::{ASTNode, ParseError, Span, TSXToken, Token, TokenReader};
use std::io::Error as IOError;

#[cfg(not(target_family = "wasm"))]
use std::{fs, path::Path};

#[derive(Debug)]
pub enum FromFileError {
	FileError(IOError),
	ParseError(ParseError, SourceId),
}

#[derive(Debug, Clone)]
pub struct Module {
	pub items: Vec<StatementOrDeclaration>,
	pub source: SourceId,
	pub span: Span,
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

	fn get_position(&self) -> &Span {
		&self.span
	}

	fn from_reader(
		reader: &mut impl TokenReader<TSXToken, crate::TokenStart>,
		state: &mut crate::ParsingState,
		settings: &ParseOptions,
	) -> ParseResult<Self> {
		let end = state.length;
		parse_statements_and_declarations(reader, state, settings).map(|statements| Module {
			source: state.source,
			items: statements,
			span: Span { start: 0, source: (), end },
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
		fs: &mut impl source_map::FileSystem,
	) -> Result<Self, FromFileError> {
		let source = fs::read_to_string(&path).map_err(FromFileError::FileError)?;
		let source_id = SourceId::new(fs, path.as_ref().to_path_buf(), source.clone());
		Self::from_string(source, settings, source_id, None)
			.map_err(|err| FromFileError::ParseError(err, source_id))
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
	pub fn from_string(
		script: String,
		mut options: ParseOptions,
		source: SourceId,
	) -> ParseResult<Self> {
		// Unfortunately some comments contain data (variable ids)
		options.include_comments = true;
		// Important not to parse JSX as <> is used for casting
		options.jsx = false;

		let line_starts = source_map::LineStarts::new(&script);
		super::lex_and_parse_script(line_starts, options, script, source, None, Default::default())
	}

	#[cfg(not(target_family = "wasm"))]
	pub fn from_file(
		path: impl AsRef<Path>,
		settings: ParseOptions,
		fs: &mut impl source_map::FileSystem,
	) -> Result<Self, FromFileError> {
		let script = fs::read_to_string(&path).map_err(FromFileError::FileError)?;
		let source = SourceId::new(fs, path.as_ref().to_path_buf(), script.clone());
		Self::from_string(script, settings, source)
			.map_err(|err| FromFileError::ParseError(err, source))
	}
}

impl ASTNode for TypeDefinitionModule {
	fn get_position(&self) -> &Span {
		todo!()
	}

	fn from_reader(
		reader: &mut impl TokenReader<TSXToken, crate::TokenStart>,
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
					if matches!(reader.peek(), Some(Token(TSXToken::EOS, _))) {
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

	fn to_string_from_buffer<T: source_map::ToString>(
		&self,
		_buf: &mut T,
		_settings: &crate::ToStringOptions,
		_depth: u8,
	) {
		todo!()
	}
}

impl ASTNode for TypeDefinitionModuleDeclaration {
	fn from_reader(
		reader: &mut impl TokenReader<TSXToken, crate::TokenStart>,
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
			_ => throw_unexpected_token_with_token(
				reader.next().unwrap(),
				&[
					TSXToken::Keyword(TSXKeyword::Declare),
					TSXToken::Keyword(TSXKeyword::Interface),
					TSXToken::Keyword(TSXKeyword::Type),
					TSXToken::Keyword(TSXKeyword::Var),
					TSXToken::At,
				],
			),
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

	fn get_position(&self) -> &Span {
		todo!("tdms get_position");
	}
}

pub(crate) fn parse_declare_item(
	reader: &mut impl TokenReader<TSXToken, crate::TokenStart>,
	state: &mut crate::ParsingState,
	settings: &ParseOptions,
	decorators: Vec<Decorator>,
	start: TokenStart,
) -> Result<TypeDefinitionModuleDeclaration, ParseError> {
	match reader.peek() {
		Some(Token(TSXToken::Keyword(TSXKeyword::Var), _)) => {
			Ok(TypeDefinitionModuleDeclaration::Variable(
				DeclareVariableDeclaration::from_reader_sub_declare(
					reader,
					state,
					settings,
					Some(start),
					decorators,
				)?,
			))
		}
		Some(Token(TSXToken::Keyword(TSXKeyword::Class), _)) => {
			Ok(TypeDefinitionModuleDeclaration::Class(
				DeclareClassDeclaration::from_reader_sub_declare(reader, state, settings)?,
			))
		}
		Some(Token(TSXToken::Keyword(TSXKeyword::Function), _)) => {
			Ok(TypeDefinitionModuleDeclaration::Function(
				DeclareFunctionDeclaration::from_reader_sub_declare_with_decorators(
					reader, state, settings, decorators,
				)?,
			))
		}
		Some(Token(TSXToken::Keyword(TSXKeyword::Type), _)) => {
			Ok(TypeDefinitionModuleDeclaration::TypeAlias(TypeAlias::from_reader(
				reader, state, settings,
			)?))
		}
		Some(Token(TSXToken::Keyword(TSXKeyword::Namespace), _)) => {
			Ok(TypeDefinitionModuleDeclaration::Namespace(Namespace::from_reader(
				reader, state, settings,
			)?))
		}
		_ => throw_unexpected_token_with_token(
			reader.next().ok_or_else(parse_lexing_error)?,
			&[
				TSXToken::Keyword(TSXKeyword::Var),
				TSXToken::Keyword(TSXKeyword::Class),
				TSXToken::Keyword(TSXKeyword::Type),
				TSXToken::Keyword(TSXKeyword::Namespace),
				TSXToken::Keyword(TSXKeyword::Function),
			],
		),
	}
}
