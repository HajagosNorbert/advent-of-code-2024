use std::collections::HashMap;

fn main() {
    let input = std::fs::read_to_string("./inputs/day5.txt").unwrap();
    let input = input.trim();
    let (rules, updates) = parse_input(&input);
    let preceeding_pages_for_page = preceedings_from_raw_rules(rules);
    let solution1 = part1(&preceeding_pages_for_page, &updates);
    let solution2 = part2(&preceeding_pages_for_page, &updates);

    println!("Part 1: {solution1}\nPart 2: {solution2}");
}

fn preceedings_from_raw_rules(raw_rules: Vec<(i64, i64)>) -> OrderRules {
    todo!()
}

fn part1(rules: &HashMap<i64, Vec<i64>>, updates: &[Vec<i64>]) -> i64 {
    updates
        .iter()
        .filter(|update| is_update_valid(rules, update))
        .map(|update: &Vec<i64>| pick_middle_page(update))
        .sum()
}

fn pick_middle_page(update: &[i64]) -> i64 {
    *update.get(update.len() / 2 + 1).unwrap()
}

fn is_update_valid(rules: &HashMap<i64, Vec<i64>>, update: &[i64]) -> bool {
    true
}

fn part2(rules: &HashMap<i64, Vec<i64>>, updates: &[Vec<i64>]) -> i64 {
    todo!()
}
// idea1: for each page in the ordering rules, store which pages must be updated before it.
// Then walk the pages updated from left to right and ask this question for each page `x`:
// do any of the subsequent page `y` have a rule such that `y` < `x`? If yes, the update is not valid.

type OrderRules = HashMap<i64, Vec<i64>>;

fn parse_input(input: &str) -> (Vec<(i64, i64)>, Vec<Vec<i64>>) {
    let (section1, section2) = input.split_once("\n\n").unwrap();

    let rules: Vec<(i64, i64)> = section1
        .split('\n')
        .map(|line| line.split_once('|').unwrap())
        .map(|(before, after)| (before.parse().unwrap(), after.parse().unwrap()))
        .collect();

    let updates: Vec<Vec<i64>> = section2
        .split('\n')
        .map(|line| {
            line.split(',')
                .map(|num_text| num_text.parse().unwrap())
                .collect()
        })
        .collect();
    (rules, updates)
}
