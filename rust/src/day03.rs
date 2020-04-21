use super::common::read_day_data;

#[derive(Debug)]
pub enum Direction {
    Up,
    Down,
    Left,
    Right,
}

#[derive(Debug)]
pub struct PathElement {
    direction: Direction,
    distance: u32,
}

type Path = Vec<PathElement>;

mod parsing {
    use super::{Direction, Path, PathElement};

    #[derive(Debug)]
    enum Error {
        InvalidDirection(String),
        InvalidDistance(String),
    }

    fn distance(s: &str) -> Result<u32, Error> {
        let res = s.parse();
        match res {
            Ok(x) => Ok(x),
            Err(_) => Err(Error::InvalidDistance(String::from(s))),
        }
    }

    fn direction_and_next(s: &str) -> Result<(Direction, &str), Error> {
        match s.split_at(1) {
            ("R", dist) => Ok((Direction::Right, dist)),
            ("U", dist) => Ok((Direction::Up, dist)),
            ("D", dist) => Ok((Direction::Down, dist)),
            ("L", dist) => Ok((Direction::Left, dist)),
            _ => Err(Error::InvalidDirection(String::from(s))),
        }
    }

    fn path_element(s: &str) -> Result<PathElement, Error> {
        let (dir, next) = direction_and_next(s)?;
        let dist = distance(next)?;
        Ok(PathElement {
            direction: dir,
            distance: dist,
        })
    }

    pub fn line(s: &str) -> Path {
        s.split(",")
            .map(path_element)
            .map(|rd| rd.expect("a path element"))
            .collect()
    }
}

pub fn input() -> Vec<Path> {
    read_day_data(3).lines().map(parsing::line).collect()
}

#[derive(PartialEq, Eq, Hash, Copy, Clone)]
pub struct Point {
    x: i32,
    y: i32,
}

pub mod part_1 {
    use super::*;

    macro_rules! solution_tests {
        ($($name:ident: $value:expr,)*) => {
        $(
            #[test]
            fn $name() {
                let ((input1, input2), expected) = $value;
                let paths = vec![input1, input2].iter().map(|&x| parsing::line(x)).collect();
                let result = solution(&paths);
                assert_eq!(Ok(expected), result);
            }
        )*
        }
    }

    solution_tests! {
        baby: (("R8,U5,L5,D3", "U7,R6,D4,L4"), 6),
        ex1: (("R75,D30,R83,U83,L12,D49,R71,U7,L72","U62,R66,U55,R34,D71,R55,D58,R83"),159),
        ex2: (("R98,U47,R26,D63,R33,U87,L62,D20,R33,U53,R51","U98,R91,D20,R16,D67,R40,U7,R15,U6,R7"),135),
    }

    use std::collections::HashSet;

    fn points_on_elem(pelm: &PathElement, mut p: &mut Point) -> HashSet<Point> {
        let mut res: HashSet<Point> = HashSet::new();
        let operation: fn(&mut Point) -> () = match pelm.direction {
            Direction::Up => |i| i.y += 1,
            Direction::Down => |i| i.y -= 1,
            Direction::Left => |i| i.x -= 1,
            Direction::Right => |i| i.x += 1,
        };
        for _i in 1..=pelm.distance {
            operation(&mut p);
            res.insert(p.clone());
        }
        res
    }
    fn points_on_path(p: &Path) -> HashSet<Point> {
        let mut cur = Point { x: 0, y: 0 };
        let mut res: HashSet<Point> = HashSet::new();
        for pelm in p {
            let new_points = points_on_elem(&pelm, &mut cur);
            let combined: HashSet<_> = res.union(&new_points).map(|&x| x).collect();
            res = combined;
        }
        res
    }
    fn manhattan_distance(p: &Point) -> i32 {
        p.x.abs() + p.y.abs()
    }

    pub fn solution(input: &Vec<Path>) -> Result<i32, String> {
        if input.len() != 2 {
            return Err(String::from("Expected exactly two paths"));
        }
        let px = points_on_path(&input[0]);
        let py = points_on_path(&input[1]);

        Ok(px.intersection(&py).map(manhattan_distance).min().unwrap())
    }
}

pub mod part_2 {
    use super::*;

    macro_rules! solution_tests {
        ($($name:ident: $value:expr,)*) => {
        $(
            #[test]
            fn $name() {
                let ((input1, input2), expected) = $value;
                let paths = vec![input1, input2].iter().map(|&x| parsing::line(x)).collect();
                let result = solution(&paths);
                assert_eq!(Ok(expected), result);
            }
        )*
        }
    }

    solution_tests! {
        ex1: (("R75,D30,R83,U83,L12,D49,R71,U7,L72","U62,R66,U55,R34,D71,R55,D58,R83"),610),
        ex2: (("R98,U47,R26,D63,R33,U87,L62,D20,R33,U53,R51","U98,R91,D20,R16,D67,R40,U7,R15,U6,R7"),410),
    }

    use std::collections::HashMap;
    use std::collections::HashSet;

    type PointDistanceMap = HashMap<Point, u32>;

    fn points_on_elem(
        pelm: &PathElement,
        mut map: &mut PointDistanceMap,
        mut p: &mut Point,
        mut i: &mut u32,
    ) -> () {
        let operation: fn(&mut Point) -> () = match pelm.direction {
            Direction::Up => |i| i.y += 1,
            Direction::Down => |i| i.y -= 1,
            Direction::Left => |i| i.x -= 1,
            Direction::Right => |i| i.x += 1,
        };
        for _i in 1..=pelm.distance {
            operation(&mut p);
            *i += 1;
            if !map.contains_key(p) {
                map.insert(p.clone(), *i);
            }
        }
    }
    fn points_on_path(p: &Path) -> PointDistanceMap {
        let mut cur = Point { x: 0, y: 0 };
        let mut res = HashMap::new();
        let mut dist = 0;
        for pelm in p {
            let new_points = points_on_elem(&pelm, &mut res, &mut cur, &mut dist);
        }
        res
    }

    fn path_distance(mpx: &PointDistanceMap, mpy: &PointDistanceMap, p: &Point) -> u32 {
        let dx = mpx.get(p).expect("point should be in map");
        let dy = mpy.get(p).expect("point should be in map");
        dx + dy
    }

    pub fn solution(input: &Vec<Path>) -> Result<u32, String> {
        if input.len() != 2 {
            return Err(String::from("Expected exactly two paths"));
        }
        let mpx = points_on_path(&input[0]);
        let mpy = points_on_path(&input[1]);

        let px: HashSet<Point> = mpx.keys().map(|&p| p).collect();
        let py: HashSet<Point> = mpy.keys().map(|&p| p).collect();

        Ok(px
            .intersection(&py)
            .map(|&p| path_distance(&mpx, &mpy, &p))
            .min()
            .unwrap())
    }
}
