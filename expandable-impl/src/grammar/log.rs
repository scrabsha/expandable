use std::{
    fmt::{Debug, Write},
    fs,
    fs::OpenOptions,
    io::Write as _,
    path::Path,
    process,
    sync::Mutex,
};

use crate::{
    grammar::{StackSymbol, State, Transition},
    TokenDescription,
};

pub(crate) fn log_transition(
    state: State,
    descr: TokenDescription,
    top: Option<StackSymbol>,
    transition: Transition,
) {
    if !Path::new("target/coverage/all.json").exists() {
        // Let's pretend fs errors never happen for a while.
        log_all_transitions();
    }
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

fn log_all_transitions() {
    fs::create_dir_all("target/coverage").expect("failed to create coverage directory");
    let mut file = OpenOptions::new()
        .create_new(true)
        .append(true)
        .open("target/coverage/all.json")
        .expect("failed to create coverage file");

    writeln!(file, "[").expect("failed to write to coverage file");

    let mut first = true;
    State::TRANSITIONS
        .iter()
        .flat_map(|(transes, _, state)| {
            transes
                .iter()
                .map(|(descr, pop, out, push)| (*state, descr, pop, out, push))
        })
        .for_each(|(state, descr, pop, out, push)| {
            if first {
                first = false;
            } else {
                writeln!(file, ",").expect("failed to write to coverage file");
            }

            write!(
                file,
                "  {{ \"in_state\": \"{:?}\", \"out_state\": \"{:?}\", \"token\": \"{:?}\", \
                 \"in_top\": \"{:?}\", \"out_top\": \"{:?}\" }}",
                state, out, descr, pop, push
            )
            .expect("failed to write to coverage file");
        });

    writeln!(file, ",\n]");
}

fn log_transitions(f: &mut impl Write, state: State) {}
