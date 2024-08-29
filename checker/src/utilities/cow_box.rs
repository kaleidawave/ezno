pub enum CowBox<'a, T> {
	Borrowed(&'a T),
	Owned(Box<T>),
}

impl<'a, T> From<&'a T> for CowBox<'a, T> {
	fn from(item: &'a T) -> CowBox<'a, T> {
		CowBox::Borrowed(item)
	}
}

impl<T> From<Box<T>> for CowBox<'static, T> {
	fn from(item: Box<T>) -> CowBox<'static, T> {
		CowBox::Owned(item)
	}
}

impl<'a, T> core::ops::Deref for CowBox<'a, T> {
	type Target = T;

	fn deref(&self) -> &T {
		match *self {
			CowBox::Borrowed(borrowed) => borrowed,
			CowBox::Owned(ref owned) => owned,
		}
	}
}
