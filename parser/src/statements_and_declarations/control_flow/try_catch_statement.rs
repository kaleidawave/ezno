use crate::{
	derive_ASTNode, ASTNode, Block, ParseError, ParseErrors, TypeAnnotation, VariableField,
	WithComment,
};
use source_map::Span;
use visitable_derive::Visitable;

#[cfg_attr(target_family = "wasm", tsify::declare)]
pub type ExceptionVarField = WithComment<VariableField>;

#[apply(derive_ASTNode)]
#[derive(Debug, Clone, Visitable, get_field_by_type::GetFieldByType)]
#[get_field_by_type_target(Span)]
pub struct TryCatchStatement {
	pub try_inner: Block,
	pub catch_inner: Option<Block>,
	pub exception_var: Option<(ExceptionVarField, Option<TypeAnnotation>)>,
	pub finally_inner: Option<Block>,
	pub position: Span,
}

impl ASTNode for TryCatchStatement {
	fn get_position(&self) -> Span {
		self.position
	}

	fn from_reader(reader: &mut crate::Lexer) -> Result<Self, crate::ParseError> {
		let start = reader.expect_keyword("try")?;
		let try_inner = Block::from_reader(reader)?;

		let mut catch_inner: Option<Block> = None;
		let mut exception_var: Option<(ExceptionVarField, Option<TypeAnnotation>)> = None;

		// Optional `catch` clause
		if reader.is_keyword_advance("catch") {
			// state.append_keyword_at_pos(reader.next().unwrap().1 .0, TSXKeyword::Catch);

			// Optional exception variable field `catch (e)`
			if reader.is_operator_advance("(") {
				let variable_field = WithComment::<VariableField>::from_reader(reader)?;

				// Optional type reference `catch (e: type)`
				let exception_var_type: Option<TypeAnnotation> = if reader.is_operator_advance(":")
				{
					let annotation = TypeAnnotation::from_reader(reader)?;
					crate::lexer::utilities::assert_type_annotations(
						reader,
						annotation.get_position(),
					)?;
					Some(annotation)
				} else {
					None
				};
				exception_var = Some((variable_field, exception_var_type));

				reader.expect(')')?;
			}

			catch_inner = Some(Block::from_reader(reader)?);
		}

		// Optional `finally` clause
		let finally_inner: Option<Block> = if reader.is_keyword_advance("finally") {
			// state.append_keyword_at_pos(reader.next().unwrap().1 .0, TSXKeyword::Finally);
			Some(Block::from_reader(reader)?)
		} else {
			None
		};

		// Determine span based on which clauses are present
		let position: Span = if let Some(finally_block) = &finally_inner {
			start.union(finally_block.get_position())
		} else if let Some(catch_block) = &catch_inner {
			start.union(catch_block.get_position())
		} else {
			// Parse error if neither catch nor finally clause is present
			return Err(ParseError::new(
				ParseErrors::ExpectedCatchOrFinally,
				start.union(try_inner.get_position()),
			));
		};

		Ok(Self { try_inner, catch_inner, exception_var, finally_inner, position })
	}

	fn to_string_from_buffer<T: source_map::ToString>(
		&self,
		buf: &mut T,
		options: &crate::ToStringOptions,
		local: crate::LocalToStringInformation,
	) {
		// Required `try` block
		buf.push_str("try");
		options.push_gap_optionally(buf);
		self.try_inner.to_string_from_buffer(buf, options, local.next_level());

		// Optional `catch` block
		if let Some(catch) = &self.catch_inner {
			options.push_gap_optionally(buf);
			buf.push_str("catch");
			options.push_gap_optionally(buf);

			// Optional exception variable: `catch (e)`
			if let Some((exception_var, exception_var_type)) = &self.exception_var {
				buf.push('(');
				exception_var.to_string_from_buffer(buf, options, local);

				// Optional type annotation: `catch (e: any)`
				if let Some(exception_var_type) = exception_var_type {
					buf.push_str(": ");
					exception_var_type.to_string_from_buffer(buf, options, local);
				}
				buf.push(')');
				options.push_gap_optionally(buf);
			}

			catch.to_string_from_buffer(buf, options, local.next_level());
		}

		// Optional `finally` block
		if let Some(finally) = &self.finally_inner {
			options.push_gap_optionally(buf);
			buf.push_str("finally");
			options.push_gap_optionally(buf);
			finally.to_string_from_buffer(buf, options, local.next_level());
		}
	}
}
