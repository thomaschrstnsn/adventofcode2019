use super::common::{parsers, read_day_lines_as};

pub fn input() -> Vec<u32> {
    read_day_lines_as(1, parsers::u32)
}

pub fn calc_fuel(mass: u32) -> u32 {
    mass / 3 - 2
}

macro_rules! fuel_tests {
    ($($name:ident: $value:expr,)*) => {
    $(
        #[test]
        fn $name() {
            let (input, expected) = $value;
            assert_eq!(expected, calc_fuel(input));
        }
    )*
    }
}

fuel_tests! {
    ex1: (12,2),
    ex2: (14,2),
    ex3: (1969, 654),
    ex4: (100756, 33583),
}

pub fn part_1(masses: Vec<u32>) -> u32 {
    masses.iter().map(|&m| calc_fuel(m)).sum()
}
