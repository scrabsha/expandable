#![allow(unused)]

// Adapted from:
// https://github.com/rust-lang/reference/tree/51817951d0d213a0011f82b62aae02c3b3f2472e
#[expandable::expr]
macro_rules! test {
    () => {{
        let Person {
            car: Some(_),
            age: person_age @ 13..=19,
            name: ref person_name,
            ..
        } = ();

        let Message::Quit = ();
        let Message::WriteString(write) = ();
        let Message::Move { x, y: 0 } = ();
        let Message::Move { .. } = ();
        let Message::ChangeColor {
            0: red,
            1: green,
            2: _,
        } = ();

        let (x, y) = (1, 2);
        let (a, 3) = (1, 2);
        let (a, 4) = (3, 4);

        let -1 = ();
        let 1 = ();
        let 2|4 = ();
        let _ = ();

        let mut variable = 10;

        let e @ 1..= 5 = ();
        let _ = ();

        let None = ();
        let Some(value) = ();

        let None = ();
        let Some(ref value) = ();

        let Person { name: &person_name, age: 18..=150 } = ();
        let Person { name: ref person_name, age: 18..=150 } = ();

        let Person { name, ref age } = ();

        let (a, _) = (10, x);

        // TODO: closure support
        // let real_part = |a: f64, _: f64| { a };

        let RGBA{r: red, g: green, b: blue, a: _} = color;

        let Some(_) = ();

        let [] = ();
        let [one] = ();
        let [head, tail @ ..] = ();
        let [.., "!"] = ();
        let [start @ .., "z"] = ();
        let ["a", end @ ..] = ();
        let whole @ [.., last] = ();
        let rest = ();

        let [.., penultimate, _] = ();

        let (1, .., y, z) = ();
        let (.., 5) = ();
        let (..) = ();

        let 'a'..='z' = ();
        let 'A'..='Z' = ();
        let 'α'..='ω' = ();
        let _ = ();

        let 0..=6 = ();
        let 7 = ();
        let 8..=14 = ();
        let _ = ();

        let 0 = ();
        let 1.. = ();

        let TROPOSPHERE_MIN..=TROPOSPHERE_MAX = ();
        let STRATOSPHERE_MIN..=STRATOSPHERE_MAX = ();
        let MESOSPHERE_MIN..=MESOSPHERE_MAX = ();
        let _ = ();

        let size @ binary::MEGA..=binary::GIGA = ();

        // TODO: support qualified path expressions
        // let 0 ..= <u8 as MaxValue>::MAX = "fits in a u8";
        // let 0 ..= <u16 as MaxValue>::MAX = "fits in a u16";
        // let 0 ..= <u32 as MaxValue>::MAX = "fits in a u32";
        let _ = "too big";

        let 0 = ();
        let &0 = ();

        let Point {x: 10, y: 20} = ();
        let Point {y: 10, x: 20} = ();
        let Point {x: 10, ..} = ();
        let Point {..} = ();

        let PointTuple {0: 10, 1: 20} = ();
        let PointTuple {1: 10, 0: 20} = ();
        let PointTuple {0: 10, ..} = ();
        let PointTuple {..} = ();

        let Message::Quit = ();
        let Message::Move {x: 10, y: 20} = ();
        let Message::Move {..} = ();

        let Struct { a: 10, b: 'X', c: false } = ();
        let Struct { a: 10, b: 'X', ref c } = ();
        let Struct { a: 10, b: 'X', ref mut c } = ();
        let Struct { a: 10, b: 'X', c: _ } = ();
        let Struct { a: _, b: _, c: _ } = ();

        let Struct { a: x, b: y, c: z } = ();

        let (a, b) = ();

        let &(0..=5) = ();
        let _ = ();

        let [1, _, _] = ();
        let [a, b, c] = ();

        let [a, b] = ();
        let [a, b, c] = ();
        let _ = ();
    }};
}

fn main() {}
