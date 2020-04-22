use rust::day04::{input, part_1, part_2};

fn main() {
    let (first, last) = input();

    println!("solution part 1: {:?}", part_1::solution(first, last));
    println!("solution part 2: {:?}", part_2::solution(first, last));
}
