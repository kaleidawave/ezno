use tokenizer_lib::sized_tokens::TokenStart;

use crate::{
	block::{parse_statements_and_declarations, statements_and_declarations_to_string},
	derive_ASTNode,
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
	BlockLike, BlockLikeMut, Decorated, Decorator, LocalToStringInformation, ParseOptions,
	ParseResult, StatementOrDeclaration, TSXKeyword, VisitOptions,
};

use super::{ASTNode, Span, TSXToken, Token, TokenReader};

#[derive(Debug, Clone)]
#[apply(derive_ASTNode)]
pub struct Module {
	pub items: Vec<StatementOrDeclaration>,
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
		options: &crate::ToStringOptions,
		local: crate::LocalToStringInformation,
	) {
		statements_and_declarations_to_string(&self.items, buf, options, local);
	}

	fn get_position(&self) -> &Span {
		&self.span
	}

	fn from_reader(
		reader: &mut impl TokenReader<TSXToken, crate::TokenStart>,
		state: &mut crate::ParsingState,
		options: &ParseOptions,
	) -> ParseResult<Self> {
		let end = state.length_of_source;
		parse_statements_and_declarations(reader, state, options).map(|statements| Module {
			items: statements,
			span: Span { start: 0, source: (), end },
		})
	}
}

impl Module {
	pub fn to_string_with_source_map(
		&self,
		options: &crate::ToStringOptions,
		this: source_map::SourceId,
		fs: &impl source_map::FileSystem,
	) -> (String, Option<source_map::SourceMap>) {
		let mut buf = source_map::StringWithOptionalSourceMap::new(true);
		self.to_string_from_buffer(
			&mut buf,
			options,
			LocalToStringInformation { depth: 0, under: this },
		);
		buf.build(fs)
	}

	// #[must_use]
	// pub fn length(&self, options: &crate::ToStringOptions) -> usize {
	// 	let mut buf = source_map::Counter::new();
	// 	self.to_string_from_buffer(
	// 		&mut buf,
	// 		options,
	// 		LocalToStringInformation { depth: 0, under: source_map::Nullable::NULL },
	// 	);
	// 	buf.get_count()
	// }
}

impl Module {
	pub fn visit<TData>(
		&self,
		visitors: &mut (impl crate::VisitorReceiver<TData> + ?Sized),
		data: &mut TData,
		options: &VisitOptions,
		source: source_map::SourceId,
	) {
		use crate::visiting::Visitable;
		let mut chain = crate::Chain::new_with_initial(crate::ChainVariable::Module(source));
		let mut chain = temporary_annex::Annex::new(&mut chain);

		{
			visitors.visit_block(&crate::block::BlockLike { items: &self.items }, data, &chain);
		}

		let iter = self.items.iter();
		if options.reverse_statements {
			iter.for_each(|item| item.visit(visitors, data, options, &mut chain));
		} else {
			iter.rev().for_each(|item| item.visit(visitors, data, options, &mut chain));
		}
	}

	pub fn visit_mut<TData>(
		&mut self,
		visitors: &mut (impl crate::VisitorMutReceiver<TData> + ?Sized),
		data: &mut TData,
		options: &VisitOptions,
		source: source_map::SourceId,
	) {
		use crate::visiting::Visitable;
		let mut chain = crate::Chain::new_with_initial(crate::ChainVariable::Module(source));
		let mut chain = temporary_annex::Annex::new(&mut chain);

		{
			visitors.visit_block_mut(
				&mut crate::block::BlockLikeMut { items: &mut self.items },
				data,
				&chain,
			);
		}

		let iter_mut = self.items.iter_mut();
		if options.reverse_statements {
			iter_mut.for_each(|item| item.visit_mut(visitors, data, options, &mut chain));
		} else {
			iter_mut.rev().for_each(|item| item.visit_mut(visitors, data, options, &mut chain));
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
	pub position: Span,
}

impl TypeDefinitionModule {
	pub fn from_string(script: &str, mut options: ParseOptions) -> ParseResult<Self> {
		// Important not to parse JSX as <> is used for casting
		options.jsx = false;

		let line_starts = source_map::LineStarts::new(script);
		super::lex_and_parse_script(line_starts, options, script, None).map(|(ast, _)| ast)
	}
}

impl ASTNode for TypeDefinitionModule {
	fn get_position(&self) -> &Span {
		&self.position
	}

	fn from_reader(
		reader: &mut impl TokenReader<TSXToken, crate::TokenStart>,
		state: &mut crate::ParsingState,
		options: &ParseOptions,
	) -> ParseResult<Self> {
		let mut declarations = Vec::new();
		loop {
			declarations
				.push(TypeDefinitionModuleDeclaration::from_reader(reader, state, options)?);
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
		let end = state.length_of_source;
		Ok(Self { declarations, position: Span { start: 0, end, source: () } })
	}

	fn to_string_from_buffer<T: source_map::ToString>(
		&self,
		_buf: &mut T,
		_options: &crate::ToStringOptions,
		_local: crate::LocalToStringInformation,
	) {
		todo!("tdm to buffer")
	}
}

impl ASTNode for TypeDefinitionModuleDeclaration {
	fn from_reader(
		reader: &mut impl TokenReader<TSXToken, crate::TokenStart>,
		state: &mut crate::ParsingState,
		options: &ParseOptions,
	) -> ParseResult<Self> {
		let decorators = decorators_from_reader(reader, state, options)?;
		match reader.peek().ok_or_else(parse_lexing_error)? {
			Token(TSXToken::Keyword(TSXKeyword::Declare), _) => {
				let declare_span = reader.next().unwrap().1;
				parse_declare_item(reader, state, options, decorators, declare_span)
			}
			Token(TSXToken::Keyword(TSXKeyword::Interface), _) => {
				let on = InterfaceDeclaration::from_reader(reader, state, options)?;
				Ok(TypeDefinitionModuleDeclaration::Interface(Decorated::new(decorators, on)))
			}
			Token(TSXToken::Keyword(TSXKeyword::Type), _) => {
				Ok(TypeDefinitionModuleDeclaration::LocalTypeAlias(TypeAlias::from_reader(
					reader, state, options,
				)?))
			}
			Token(TSXToken::Keyword(TSXKeyword::Var), _) => {
				Ok(TypeDefinitionModuleDeclaration::LocalVariableDeclaration(
					DeclareVariableDeclaration::from_reader_sub_declare(
						reader, state, options, None, decorators,
					)?,
				))
			}
			Token(TSXToken::Comment(_) | TSXToken::MultiLineComment(_), _) => {
				let (TSXToken::MultiLineComment(comment) | TSXToken::Comment(comment)) =
					reader.next().unwrap().0
				else {
					unreachable!()
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
		_options: &crate::ToStringOptions,
		_local: crate::LocalToStringInformation,
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
	options: &ParseOptions,
	decorators: Vec<Decorator>,
	start: TokenStart,
) -> ParseResult<TypeDefinitionModuleDeclaration> {
	match reader.peek() {
		Some(Token(
			TSXToken::Keyword(TSXKeyword::Var | TSXKeyword::Const | TSXKeyword::Let),
			_,
		)) => Ok(TypeDefinitionModuleDeclaration::Variable(
			DeclareVariableDeclaration::from_reader_sub_declare(
				reader,
				state,
				options,
				Some(start),
				decorators,
			)?,
		)),
		Some(Token(TSXToken::Keyword(TSXKeyword::Class), _)) => {
			Ok(TypeDefinitionModuleDeclaration::Class(
				DeclareClassDeclaration::from_reader_sub_declare(reader, state, options)?,
			))
		}
		Some(Token(TSXToken::Keyword(TSXKeyword::Function), _)) => {
			Ok(TypeDefinitionModuleDeclaration::Function(
				DeclareFunctionDeclaration::from_reader_sub_declare_with_decorators(
					reader, state, options, decorators,
				)?,
			))
		}
		Some(Token(TSXToken::Keyword(TSXKeyword::Type), _)) => {
			Ok(TypeDefinitionModuleDeclaration::TypeAlias(TypeAlias::from_reader(
				reader, state, options,
			)?))
		}
		Some(Token(TSXToken::Keyword(TSXKeyword::Namespace), _)) => {
			Ok(TypeDefinitionModuleDeclaration::Namespace(Namespace::from_reader(
				reader, state, options,
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
