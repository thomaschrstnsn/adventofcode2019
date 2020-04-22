use super::common::{parsers, read_day_data};

pub fn input() -> Vec<u32> {
    read_day_data(2).split(",").map(parsers::u32).collect()
}

#[derive(PartialEq, Eq, Debug)]
pub enum Status {
    Running,
    Stopped,
    Failed,
}

pub struct State {
    instruction_pointer: usize,
    memory: Vec<u32>,
    status: Status,
}

fn instruction(memory: &Vec<u32>, instruction_pointer: usize) -> Vec<u32> {
    memory
        .iter()
        .skip(instruction_pointer)
        .take(4)
        .cloned()
        .collect()
}

fn perform(s: &mut State, op: fn(u32, u32) -> u32, addr1: &u32, addr2: &u32, dest: &u32) {
    s.memory[*dest as usize] = op(s.memory[*addr1 as usize], s.memory[*addr2 as usize]);
    s.instruction_pointer += 4;
}

fn stop(s: &mut State) {
    s.status = Status::Stopped;
}

fn run_one_instruction(state: &mut State) {
    let instruction = instruction(&state.memory, state.instruction_pointer);
    match &instruction[..] {
        [1, a1, a2, d] => perform(state, |x, y| x + y, a1, a2, d),
        [2, a1, a2, d] => perform(state, |x, y| x * y, a1, a2, d),
        [99, _, _, _] => stop(state),
        _ => {
            state.status = if instruction[0] == 99 {
                Status::Stopped
            } else {
                println!(
                    "invalid instruction found: {:?} instruction pointer: {}",
                    instruction, state.instruction_pointer
                );
                Status::Failed
            };
        }
    };
}

fn run_program(memory: &mut Vec<u32>) -> State {
    let mut state = State {
        instruction_pointer: 0,
        memory: memory.clone(),
        status: Status::Running,
    };
    while state.status == Status::Running {
        run_one_instruction(&mut state);
    }
    state
}

macro_rules! run_program_tests {
    ($($name:ident: $value:expr,)*) => {
    $(
        #[test]
        fn $name() {
            let (input, expected) = $value;
            let state = run_program(&mut input.clone());
            assert_eq!(expected, state.memory);
            assert_eq!(Status::Stopped, state.status)
        }
    )*
    }
}

run_program_tests! {
    ex1: (vec![1,0,0,0,99], vec![2,0,0,0,99]),
    ex2: (vec![2,3,0,3,99], vec![2,3,0,6,99]),
    ex3: (vec![2,4,4,5,99,0], vec![2,4,4,5,99,9801]),
    ex4: (vec![1,1,1,4,99,5,6,0,99], vec![30,1,1,4,2,5,6,0,99]),
}

pub mod part_1 {
    use super::*;
    pub fn solution(memory: &Vec<u32>) -> u32 {
        let mut fixed = memory.clone();
        fixed[1] = 12;
        fixed[2] = 2;
        let after: State = run_program(&mut fixed);
        after.memory[0]
    }
}

pub mod part_2 {
    use super::*;
    pub fn solution(memory: &Vec<u32>) -> Option<u32> {
        for noun in 0..=99 {
            for verb in 0..=99 {
                let mut fixed = memory.clone();
                fixed[1] = noun;
                fixed[2] = verb;
                let after: State = run_program(&mut fixed);

                if after.memory[0] == 19690720 {
                    return Option::Some(noun * 100 + verb);
                }
            }
        }
        None
    }
}
