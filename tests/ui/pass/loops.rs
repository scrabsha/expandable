#![allow(unused)]

#[expandable::expr]
macro_rules! all_loops {
    () => {
        loop {}
    };

    () => {
        loop {
            break 42;
        }
    };

    () => {
        while a > 0 {}
    };

    () => {
        for i in 0..42 {
            printf("%d\n", i);
        }
    };
}

fn main() {}
