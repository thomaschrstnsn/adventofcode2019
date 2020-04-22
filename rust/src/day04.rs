pub fn input() -> (u32, u32) {
    (356261, 846303)
}

pub mod part_1 {
    macro_rules! valid_password_tests {
        ($($name:ident: $value:expr,)*) => {
        $(
            #[test]
            fn $name() {
                let (input, expected) = $value;
                let result = valid_password(input);
                assert_eq!(expected, result);
            }
        )*
        }
    }

    valid_password_tests! {
        valid: (111111, true),
        invalid1: (223450, false),
        invalid2: (123789, false),
    }

    fn is_not_decreasing(s: &str) -> bool {
        let mut prev = 0;
        for c in s.chars() {
            let i = c.to_digit(10).expect("should be digit");
            if i < prev {
                return false;
            } else {
                prev = i;
            }
        }
        return true;
    }

    fn has_two_adjacent(s: &str) -> bool {
        let mut prev = 'x';
        for c in s.chars() {
            if c == prev {
                return true;
            } else {
                prev = c;
            }
        }
        return false;
    }

    fn valid_password(n: u32) -> bool {
        let s = format!("{}", n);

        let has_six_chars = s.chars().count() == 6;
        let is_not_decreasing = is_not_decreasing(&s);
        let has_two_adjacent = has_two_adjacent(&s);

        return has_six_chars && is_not_decreasing && has_two_adjacent;
    }

    pub fn solution(first: u32, last: u32) -> usize {
        (first..=last).filter(|&x| valid_password(x)).count()
    }
}

pub mod part_2 {
    macro_rules! valid_password_tests {
        ($($name:ident: $value:expr,)*) => {
        $(
            #[test]
            fn $name() {
                let (input, expected) = $value;
                let result = valid_password(input);
                assert_eq!(expected, result);
            }
        )*
        }
    }

    valid_password_tests! {
        valid: (112233, true),
        invalid: (123444, false),
        valid2: (111122, false),
    }

    fn is_not_decreasing(s: &str) -> bool {
        let mut prev = 0;
        for c in s.chars() {
            let i = c.to_digit(10).expect("should be digit");
            if i < prev {
                return false;
            } else {
                prev = i;
            }
        }
        return true;
    }

    fn has_exactly_two_adjacent(s: &str) -> bool {
        let mut prev = 'x';
        let mut count = 0;
        for c in s.chars() {
            if c != prev {
                if count == 2 {
                    return true;
                }
                count = 1;
                prev = c;
            } else {
                count += 1;
            }
        }
        return false;
    }

    fn valid_password(n: u32) -> bool {
        let s = format!("{}", n);

        let has_six_chars = s.chars().count() == 6;
        let is_not_decreasing = is_not_decreasing(&s);
        let has_two_adjacent = has_exactly_two_adjacent(&s);

        return has_six_chars && is_not_decreasing && has_two_adjacent;
    }

    pub fn solution(first: u32, last: u32) -> usize {
        (first..=last).filter(|&x| valid_password(x)).count()
    }
}
