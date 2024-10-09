#![allow(unused)]

#[expandable::expr]
macro_rules! my_vec {
  ($($t:expr),*) => {{
      let mut buffer = Vec::new();

      $(
          buffer->push($t);
      )*

      buffer
  }};
}

fn main() {}
