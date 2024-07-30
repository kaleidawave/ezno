pub enum Accumulator<T> {
	Finished(T),
	Partial {},
	None,
}

impl<T> Accumulator<T> {
	pub fn or_else() {}
}
