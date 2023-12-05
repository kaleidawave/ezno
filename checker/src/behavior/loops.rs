pub struct ValueWithEvents {
    events: Vec<Event>,
    value: TypeId,
}

pub enum LoopBehavior {
    While(TypeId),
    For {
        initializer: TypeId,
        condition: ValueWithEvents,
        afterthought: ValueWithEvents
    }
}

// TODO
// events -> get diff, start, end. (end - start).abs() / diff < options.loop_max

fn evaluate() {
    
}