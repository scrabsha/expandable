#! /usr/bin/env -S cargo -Zscript

use std::{
    cmp::min,
    fs,
    io::Write,
    path::{Path, PathBuf},
    process::{Command, ExitStatus, Stdio},
};

fn main() {
    let out_path = std::env::args().nth(1).expect("No output path");

    let cases = TestCaseCrawler::from_dir("specs/ferrocene")
        .crawl()
        .into_iter()
        .map(|tc| {
            println!("Processing `{}`", tc.path.display());
            tc.try_run()
        })
        .collect::<Vec<_>>();

    write_final_table(cases, &out_path);
}

struct TestCaseCrawler {
    acc: Vec<TestCase>,
    init_dir: PathBuf,
}

impl TestCaseCrawler {
    fn from_dir(init_dir: &str) -> TestCaseCrawler {
        TestCaseCrawler {
            acc: vec![],
            init_dir: PathBuf::from(init_dir),
        }
    }

    fn crawl(mut self) -> Vec<TestCase> {
        self.crawl_dir(&self.init_dir.clone(), &[]);

        self.acc.sort();

        self.acc
    }

    fn crawl_dir(&mut self, path: &Path, section: &[usize]) {
        for entry in fs::read_dir(path).expect("read_dir failed") {
            let entry = entry.expect("read_dir entry failed");
            let path = entry.path();
            if path.is_dir() {
                let mut section = section.to_vec();

                let suffix = path
                    .file_name()
                    .expect("file_name failed")
                    .to_str()
                    .expect("file_name not UTF-8")
                    .parse::<usize>()
                    .expect("dir name not a number");
                section.push(suffix);

                self.crawl_dir(&path, section.as_slice());
            } else if path.extension().map(|ext| ext == "rs").unwrap_or_default() {
                self.crawl_file(&path, section);
            }
        }
    }

    fn crawl_file(&mut self, path: &Path, section: &[usize]) {
        let paragraph = path
            .file_stem()
            .expect("file_name failed")
            .to_str()
            .expect("file_name not UTF-8")
            .parse::<usize>()
            .expect("file name not a number");

        self.acc.push(TestCase {
            path: path.to_path_buf(),
            section: section.to_vec(),
            paragraph,
        });
    }
}

#[derive(Debug, Eq, Ord, PartialEq, PartialOrd)]
struct TestCase {
    section: Vec<usize>,
    paragraph: usize,
    path: PathBuf,
}

impl TestCase {
    fn try_run(self) -> FinalTestCase {
        let TestCase {
            section,
            paragraph,
            path,
        } = self;

        let doc = fs::read_to_string(&path)
            .expect("read_to_string failed")
            .lines()
            .filter_map(|line| {
                if line.starts_with("//!") {
                    let idx = min(line.len(), 4);
                    Some(line[idx..].to_string())
                } else {
                    None
                }
            })
            .collect::<Vec<_>>()
            .join("\n");

        let (rule, url) = doc
            .split_once("\n\n")
            .expect("Failed to split doc into rule and source");

        let rule = rule.replace('\n', " ");

        let compiles = compile_snippet(&path).success();

        FinalTestCase {
            section,
            paragraph,
            rule: rule.to_string(),
            url: url.to_string(),
            compiles,
        }
    }
}

fn compile_snippet(path: &Path) -> ExitStatus {
    Command::new("rustc")
        .arg("--crate-type=lib")
        .arg(path)
        .stdout(Stdio::null())
        .stderr(Stdio::null())
        .status()
        .expect("failed to run rustc")
}

struct FinalTestCase {
    section: Vec<usize>,
    paragraph: usize,
    rule: String,
    url: String,
    compiles: bool,
}

fn write_final_table(cases: Vec<FinalTestCase>, out_path: &str) {
    let mut out = fs::File::create(out_path).expect("Failed to create output file");
    writeln!(out, "| Section | Rule | Detected by `rustc` |").unwrap();
    writeln!(out, "| ------- | ---- | -------- |").unwrap();

    for case in cases {
        let pretty_section = case
            .section
            .iter()
            .map(|n| n.to_string())
            .collect::<Vec<_>>()
            .join(".");

        let paragraph = case.paragraph;
        let url = case.url;
        let rule = case.rule;
        let compiles = case.compiles;
        let detected_by_rustc = if !compiles { "✅" } else { "❌" };

        writeln!(
            out,
            "| [{pretty_section}:{paragraph}]({url}) | {rule} | {detected_by_rustc} |",
        )
        .unwrap();
    }
}
