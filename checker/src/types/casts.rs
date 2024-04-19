use super::Constant;

/// TODO needs environment for to primitive
///
/// <https://tc39.es/ecma262/multipage/abstract-operations.html#sec-tonumber>
pub(crate) fn cast_as_number(cst: &Constant, strict_casts: bool) -> Result<f64, ()> {
	if strict_casts && !matches!(cst, Constant::Number(_)) {
		return Err(());
	}
	match cst {
		Constant::Number(number) => Ok(f64::from(*number)),
		Constant::String(str) => match str.parse::<f64>() {
			Ok(value) => Ok(value),
			Err(_) => {
				// TODO maybe warning
				Ok(f64::NAN)
			}
		},
		Constant::Boolean(val) => Ok(if *val { 1f64 } else { 0f64 }),
		Constant::NaN | Constant::Undefined => Ok(f64::NAN),
		Constant::Null => Ok(0f64),
		Constant::Symbol { key: _ } => todo!(),
	}
}

pub(crate) fn cast_as_string(cst: &Constant, strict_casts: bool) -> Result<String, ()> {
	if strict_casts && !matches!(cst, Constant::String(_)) {
		return Err(());
	}
	Ok(cst.as_js_string())
}

/// <https://tc39.es/ecma262/multipage/abstract-operations.html#sec-toboolean>
///
/// TODO this ridiculous clause: <https://tc39.es/ecma262/multipage/additional-ecmascript-features-for-web-browsers.html#sec-IsHTMLDDA-internal-slot-to-boolean>
#[allow(clippy::unnecessary_wraps)]
pub(crate) fn cast_as_boolean(cst: &Constant, strict_casts: bool) -> Result<bool, ()> {
	if strict_casts {
		crate::utilities::notify!("TODO assert boolean type here, maybe levels. Need to also return where can do collapsation");
	}
	Ok(match cst {
		Constant::Number(number) => number.into_inner() != 0.,
		Constant::String(value) => !value.is_empty(),
		Constant::Boolean(value) => *value,
		Constant::NaN | Constant::Undefined | Constant::Null => false,
		Constant::Symbol { key: _ } => todo!(),
	})
}
