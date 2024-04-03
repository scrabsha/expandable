use std::{io::Write, process::Command};

const HEADER: &str = r#"// This file is automatically @generated by `grammar-gen`.
// It is not intended for manual editing.
// See the documentation in `expandable` for more information:
// https://github.com/scrabsha/expandable
"#;

pub(crate) fn fmt(code: &str) -> String {
    let output = Command::new("rustfmt")
        .arg("--edition=2018")
        .arg("--emit=stdout")
        .arg("--color=never")
        .arg("--")
        .stdin(std::process::Stdio::piped())
        .stdout(std::process::Stdio::piped())
        .spawn()
        .unwrap();

    let mut stdin = output.stdin.as_ref().unwrap();

    stdin.write_all(code.as_bytes()).unwrap();

    let output = output.wait_with_output().unwrap();
    assert!(output.status.success());

    let code = String::from_utf8(output.stdout).unwrap();

    format!("{HEADER}\n{code}")
}
