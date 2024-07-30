use std::collections::HashSet;

use super::{Event, FinalEvent};
use crate::{
	context::InformationChain,
	events::CallingTiming,
	types::{
		calling::Callable,
		printing::{print_property_key_into_buf, print_type_into_buf},
		properties::PropertyKey,
		GenericChain, TypeStore,
	},
	PropertyValue,
};

pub fn debug_effects<C: InformationChain>(
	buf: &mut String,
	events: &[Event],
	types: &TypeStore,
	info: &C,
	depth: u8,
	debug: bool,
) {
	use std::fmt::Write;

	let args = GenericChain::None;

	let mut idx = 0;

	while idx < events.len() {
		for _ in 0..depth {
			buf.push('\t');
		}
		let event = &events[idx];
		match event {
			Event::ReadsReference { reference, reflects_dependency, position: _ } => {
				write!(buf, "read '{reference:?}' into {reflects_dependency:?}").unwrap();
			}
			Event::SetsVariable(variable, value, _) => {
				write!(buf, "{variable:?}' = ").unwrap();
				print_type_into_buf(*value, buf, &mut HashSet::new(), args, types, info, debug);
			}
			Event::Getter {
				on,
				under,
				reflects_dependency,
				publicity: _,
				position: _,
				mode: _,
			} => {
				buf.push_str("read ");
				print_type_into_buf(*on, buf, &mut HashSet::new(), args, types, info, debug);
				if let PropertyKey::String(_) = under {
					buf.push('.');
				}
				print_property_key_into_buf(
					under,
					buf,
					&mut HashSet::new(),
					args,
					types,
					info,
					debug,
				);
				write!(buf, " into {reflects_dependency:?}").unwrap();
			}
			Event::Setter { on, under, new, initialisation, publicity: _, position: _ } => {
				print_type_into_buf(*on, buf, &mut HashSet::new(), args, types, info, debug);
				buf.push('[');
				print_property_key_into_buf(
					under,
					buf,
					&mut HashSet::default(),
					args,
					types,
					info,
					debug,
				);
				buf.push_str("] = ");
				if let PropertyValue::Value(new) = new {
					print_type_into_buf(*new, buf, &mut HashSet::new(), args, types, info, debug);
				} else {
					write!(buf, "{new:?}").unwrap();
				}
				if *initialisation {
					buf.push_str(" (initialisation)")
				}
			}
			Event::CallsType {
				on,
				with: _,
				reflects_dependency,
				timing,
				called_with_new: _,
				call_site: _,
				possibly_thrown: _,
			} => {
				buf.push_str("call ");
				match on {
					Callable::Fixed(function, _) => {
						write!(buf, " {function:?} ").unwrap();
					}
					Callable::Type(on) => {
						let mut cycles = HashSet::new();
						print_type_into_buf(*on, buf, &mut cycles, args, types, info, debug);
					}
				}
				write!(buf, " into {reflects_dependency:?} ",).unwrap();
				buf.push_str(match timing {
					CallingTiming::Synchronous => "now",
					CallingTiming::QueueTask => "queue",
					CallingTiming::AtSomePointManyTimes => "sometime",
				});
				// TODO args
			}
			Event::Conditionally { condition, truthy_events, otherwise_events, position: _ } => {
				let truthy_events = *truthy_events as usize;
				let otherwise_events = *otherwise_events as usize;

				buf.push_str("if ");
				print_type_into_buf(*condition, buf, &mut HashSet::new(), args, types, info, debug);
				buf.push_str(" then\n");

				let events_if_true = &events[(idx + 1)..=(idx + truthy_events)];
				if truthy_events != 0 {
					debug_effects(buf, events_if_true, types, info, depth + 1, debug);
				}

				if otherwise_events != 0 {
					let start = idx + truthy_events + 1;
					let otherwise = &events[(start)..(start + otherwise_events)];
					for _ in 0..depth {
						buf.push('\t');
					}
					buf.push_str("else\n");
					debug_effects(buf, otherwise, types, info, depth + 1, debug);
				}
				idx += truthy_events + otherwise_events + 1;
				continue;
			}
			Event::CreateObject { prototype: _, referenced_in_scope_as, position: _ } => {
				write!(buf, "create object as {referenced_in_scope_as:?}").unwrap();
			}
			Event::Iterate { iterate_over, initial: _, kind: _ } => {
				buf.push_str("iterate\n");
				let inner_events = &events[(idx + 1)..(idx + 1 + *iterate_over as usize)];
				debug_effects(buf, inner_events, types, info, depth + 1, debug);
				idx += *iterate_over as usize + 1;
				continue;
			}
			Event::FinalEvent(FinalEvent::Throw { thrown, .. }) => {
				buf.push_str("throw ");
				print_type_into_buf(*thrown, buf, &mut HashSet::new(), args, types, info, debug);
			}
			Event::FinalEvent(FinalEvent::Break { .. }) => {
				buf.push_str("break");
			}
			Event::FinalEvent(FinalEvent::Continue { .. }) => {
				buf.push_str("continue");
			}
			Event::FinalEvent(FinalEvent::Return { returned, position: _ }) => {
				buf.push_str("return ");
				print_type_into_buf(*returned, buf, &mut HashSet::new(), args, types, info, debug);
			}
			Event::ExceptionTrap { .. } => todo!(),
			Event::RegisterVariable { name, .. } => {
				write!(buf, "register variable {name}").unwrap();
			}
			Event::EndOfControlFlow(_) => {
				buf.push_str("end");
			}
			Event::Miscellaneous(misc) => match misc {
				crate::events::MiscellaneousEvents::Has { .. } => {
					buf.push_str("Has");
				}
				crate::events::MiscellaneousEvents::Delete { .. } => {
					buf.push_str("Delete");
				}
			},
		}
		buf.push('\n');
		idx += 1;
	}
}
