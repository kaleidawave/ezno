use std::{collections::HashMap, iter};

use crate::{TypeId, Variable};

use super::{CanUseThis, Context, ContextId, ContextType, Scope};

#[derive(Debug)]
pub(crate) struct ExistingContext {
	pub variables: HashMap<String, Variable>,
	pub named_types: HashMap<String, TypeId>,
	pub scope: Scope,
	pub can_use_this: CanUseThis,
}

#[derive(Debug, Default)]
pub(crate) struct ExistingContextStore {
	pub existing_environments: HashMap<ContextId, ExistingContext>,
	pub parent_references: HashMap<ContextId, ContextId>,
}

impl ExistingContextStore {
	pub(crate) fn resurrect_existing_environment<T: ContextType>(
		&mut self,
		on: &mut Context<T>,
		ctx_id: ContextId,
	) -> Vec<(ContextId, ExistingContext)> {
		if on.context_id == ctx_id {
			unreachable!()
		}
		iter::successors(Some(ctx_id), |cur| self.parent_references.get(cur).cloned())
			.take_while(|id| *id != ContextId::ROOT)
			.map(|id| (id, self.existing_environments.remove(&id).unwrap()))
			.collect()
	}
}
