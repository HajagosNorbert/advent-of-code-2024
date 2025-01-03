use std::{collections::HashMap, fs};
fn main() {
    let input = fs::read_to_string("./inputs/day11.txt").unwrap();
    let input = parse_input(&input);
    let solution1 = part1(&input);
    let solution2 = part2(&input);

    println!("Part 1: {solution1}\nPart 2: {solution2}");
}

fn part1(input: &[i64]) -> i64 {
    simulate(input, 25)
}

fn simulate(input: &[i64], times: u8) -> i64 {
    let mut cache: HashMap<(i64, u8), i64> = HashMap::new();
    let mut sum = 0;
    for &num in input {
        sum += sim_core(&mut cache, num, times);
    }
    sum
}

fn sim_core(cache: &mut HashMap<(i64, u8), i64>, num: i64, iters: u8) -> i64 {
    if iters == 0 {
        return 1;
    }
    let key = (num, iters);
    if let Some(&stones) = cache.get(&key) {
        return stones;
    }

    let new_stones = if num == 0 {
        sim_core(cache, 1, iters - 1)
    } else if let Some((left, right)) = split_num_if_even_digited(num) {
        sim_core(cache, left, iters - 1) + sim_core(cache, right, iters - 1)
    } else {
        sim_core(cache, num * 2024, iters - 1)
    };

    cache.insert(key, new_stones);
    new_stones
}

fn parse_input(input: &str) -> Vec<i64> {
    input
        .trim_ascii()
        .split_ascii_whitespace()
        .map(|value| value.parse().unwrap())
        .collect()
}

fn split_num_if_even_digited(num: i64) -> Option<(i64, i64)> {
    let digit_count = count_digits(num);
    if digit_count % 2 != 0 {
        return None;
    }
    let divisor = i64::pow(10, digit_count / 2);
    return Some((num / divisor, num % divisor));
}

fn count_digits(num: i64) -> u32 {
    let base: i64 = 10;
    let mut digit_count = 1;
    while num / base.pow(digit_count) > 0 {
        digit_count += 1;
    }
    digit_count
}

fn part2(input: &[i64]) -> i64 {
    simulate(input, 75)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn simulate_single() {
        assert_eq!(simulate(&[17], 1), 2);
        assert_eq!(simulate(&[17], 1), 2);
    }

    #[test]
    fn simulate_multiple() {
        assert_eq!(simulate(&[125, 17], 0), 2);
        assert_eq!(simulate(&[125, 17], 6), 22);
        assert_eq!(simulate(&[125, 17], 25), 55312);
    }

    #[test]
    fn test_split() {
        assert_eq!(split_num_if_even_digited(1234), Some((12, 34)));
        assert_eq!(split_num_if_even_digited(1034), Some((10, 34)));
        assert_eq!(split_num_if_even_digited(1000), Some((10, 0)));
    }

    #[test]
    fn test_count_digit() {
        assert_eq!(count_digits(7), 1);
        assert_eq!(count_digits(77), 2);
        assert_eq!(count_digits(10), 2);
    }
}
