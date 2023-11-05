use std::env;

const EXPANDABLE_INSTRUMENT: &str = "EXPANDABLE_INSTRUMENT";

fn main() {
    println!("cargo:rerun-if-env-changed={EXPANDABLE_INSTRUMENT}");

    if env::var(EXPANDABLE_INSTRUMENT).is_ok() {
        println!("cargo:rustc-cfg=transition_coverage");
    }
}
