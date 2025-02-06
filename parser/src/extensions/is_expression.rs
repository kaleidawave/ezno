use iterator_endiate::EndiateIteratorExt;
use source_map::Span;
use visitable_derive::Visitable;

use crate::{
	derive_ASTNode,
	expressions::{ExpressionOrBlock, MultipleExpression},
	ASTNode, TypeAnnotation,
};

#[apply(derive_ASTNode)]
#[derive(Debug, PartialEq, Clone, Visitable, get_field_by_type::GetFieldByType)]
#[get_field_by_type_target(Span)]
pub struct IsExpression {
	pub matcher: Box<MultipleExpression>,
	pub branches: Vec<(TypeAnnotation, ExpressionOrBlock)>,
	pub position: Span,
}

impl ASTNode for IsExpression {
	fn get_position(&self) -> Span {
		self.position
	}

	fn from_reader(reader: &mut crate::Lexer) -> crate::ParseResult<Self> {
		let start = reader.expect_keyword("is")?;
		reader.expect('(')?;
		let matcher = MultipleExpression::from_reader(reader)?;
		reader.expect(')')?;
		reader.expect('{')?;
		let mut branches = Vec::new();
		loop {
			// ReturnType important here for
			let type_annotation = TypeAnnotation::from_reader_with_precedence(
				reader,
				crate::type_annotations::TypeOperatorKind::ReturnType,
			)?;
			reader.expect_operator("=>")?;
			let body = ExpressionOrBlock::from_reader(reader)?;
			if reader.is_operator_advance("}") {
				branches.push((type_annotation, body));
				break;
			}
			if let ExpressionOrBlock::Expression(..) = body {
				reader.expect(',')?;
			}
			branches.push((type_annotation, body));
		}
		Ok(IsExpression {
			position: start.union(reader.get_end()),
			matcher: Box::new(matcher),
			branches,
		})
	}

	fn to_string_from_buffer<T: source_map::ToString>(
		&self,
		buf: &mut T,
		options: &crate::ToStringOptions,
		local: crate::LocalToStringInformation,
	) {
		buf.push_str("is (");
		self.matcher.to_string_from_buffer(buf, options, local);
		buf.push_str(") {");
		for (at_end, (l, r)) in self.branches.iter().endiate() {
			l.to_string_from_buffer(buf, options, local);
			buf.push_str(" => ");
			r.to_string_from_buffer(buf, options, local);
			if !at_end {
				buf.push_str(", ");
			}
		}
		buf.push('}');
	}
}
