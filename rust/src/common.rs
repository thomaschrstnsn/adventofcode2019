use std::fs;

pub fn read_day_data(day: u8) -> String {
    fs::read_to_string(format!("../day{:02}input.txt", day))
        .expect("Something went wrong reading the file")
}

pub fn read_day_lines_as<T>(day: u8, op: fn(&str) -> T) -> Vec<T> {
    read_day_data(day).lines().map(|x| op(x)).collect()
}

pub mod parsers {
    pub fn u32(s: &str) -> u32 {
        s.parse().expect("could not parse as unsigned int")
    }
}
