#![allow(unused_imports)]

use checker::{
	Constant, FourthPassData, FourthPassVisitor, FourthPassVisitors, FunctionPointer,
	InternalFunctionId, SecondPassVisitor, ThirdPassData, ThirdPassVisitor,
};
use parser::{
	expressions::ExpressionId,
	extractor::{ExtractedFunction, ExtractedFunctions},
	ASTNode, Chain, Expression,
};

use checker::{FirstPassVisitor, TypeDisplay};

pub(super) struct StringLiteralPrinter;

impl FirstPassVisitor<Expression> for StringLiteralPrinter {
	fn visit_mut(
		&mut self,
		item: &mut Expression,
		_data: &mut checker::FirstPassData,
		_functions: &mut ExtractedFunctions,
		_chain: &Chain,
	) {
		if let Expression::StringLiteral(value, _, _, _) = item {
			println!("Found: {:?}", value);
		}
	}
}
