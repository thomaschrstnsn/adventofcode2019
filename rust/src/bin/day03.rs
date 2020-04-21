use rust::day03::{input, part_1, part_2};

fn main() {
    let input = input();

    println!("solution part 1: {:?}", part_1::solution(&input));
    println!("solution part 2: {:?}", part_2::solution(&input));
}
