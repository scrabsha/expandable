use std::{
    fmt::{Debug, Write as _},
    fs,
    fs::OpenOptions,
    io::Write as _,
    process,
    sync::Mutex,
};

use crate::{
    grammar::{StackSymbol, State, Transition},
    TokenDescription,
};

struct LoggableTransition {
    in_state: State,
    in_top: Option<StackSymbol>,
    descr: TokenDescription,
    out_state: State,
    out_top: Option<StackSymbol>,
}

impl LoggableTransition {
    fn from_actual_transition(
        state: State,
        descr: TokenDescription,
        top: Option<StackSymbol>,
        transition: Transition,
    ) -> LoggableTransition {
        // Don't count the top symbol as consumed if it has not been popped.
        let top = if transition.pop { top } else { None };

        LoggableTransition {
            in_state: state,
            in_top: top,
            descr,
            out_state: transition.state,
            out_top: transition.push,
        }
    }

    fn pp(&self) -> String {
        let LoggableTransition {
            in_state,
            in_top,
            descr,
            out_state,
            out_top,
        } = self;

        let mut fields = vec![
            ("in_state", in_state as &dyn Debug),
            ("out_state", out_state as &dyn Debug),
            ("token", &descr as &dyn Debug),
        ];

        if let Some(in_top) = in_top {
            fields.push(("in_top", in_top as &dyn Debug));
        }

        if let Some(out_top) = out_top {
            fields.push(("out_top", out_top as &dyn Debug));
        }

        let mut buf = String::new();

        buf.push_str("{");

        for (name, value) in fields {
            write!(buf, " \"{name}\": \"{:?}\",", value).expect("failed to write to string");
        }

        buf.pop();
        buf.push_str(" }");

        buf
    }
}

fn transition_id() -> usize {
    static TRANSITION_ID: Mutex<usize> = Mutex::new(0);

    let mut lock = TRANSITION_ID.lock().unwrap();
    let id = *lock;
    *lock += 1;
    id
}

pub(crate) fn log_transition(
    state: State,
    descr: TokenDescription,
    top: Option<StackSymbol>,
    transition: Transition,
) {
    let dir_path = format!("target/coverage/{:?}", process::id());
    fs::create_dir_all(&dir_path).expect("failed to create coverage directory");

    let file_path = format!(
        "target/coverage/{:?}/{:?}.json",
        process::id(),
        transition_id()
    );
    let transition = LoggableTransition::from_actual_transition(state, descr, top, transition);

    let mut file = OpenOptions::new()
        .create(true)
        .append(true)
        .open(file_path)
        .expect("failed to create coverage file");

    writeln!(file, "{}", transition.pp()).expect("failed to write to coverage file");
}
