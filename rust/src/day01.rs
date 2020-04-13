use super::common::{parsers, read_day_lines_as};

pub fn input() -> Vec<u32> {
    read_day_lines_as(1, parsers::u32)
}

pub fn calc_fuel(mass: u32) -> u32 {
    mass / 3 - 2
}

pub mod part_1 {
    macro_rules! fuel_tests {
        ($($name:ident: $value:expr,)*) => {
        $(
            #[test]
            fn $name() {
                let (input, expected) = $value;
                assert_eq!(expected, super::calc_fuel(input));
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

    pub fn solution(masses: &Vec<u32>) -> u32 {
        masses.iter().map(|&m| super::calc_fuel(m)).sum()
    }
}

pub mod part_2 {
    fn rec_fuel(mass: u32) -> u32 {
        let mut result: u32 = 0;
        let mut fuel: u32 = mass;

        loop {
            fuel = fuel / 3;

            if fuel < 2 {
                break;
            }

            fuel -= 2;

            result += fuel;
        }

        result
    }

    macro_rules! rec_fuel_tests {
        ($($name:ident: $value:expr,)*) => {
        $(
            #[test]
            fn $name() {
                let (input, expected) = $value;
                assert_eq!(expected, rec_fuel(input));
            }
        )*
        }
    }

    rec_fuel_tests! {
        ex1: (12,2),
        ex2: (14,2),
        ex3: (1969, 966),
        ex4: (100756, 50346),
    }

    pub fn solution(masses: &Vec<u32>) -> u32 {
        masses.iter().map(|&m| rec_fuel(m)).sum()
    }
}
