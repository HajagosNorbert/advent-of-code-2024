module [part1, part2]

import Utils exposing [unwrap]

part1 = \input ->
    ids = separate_ids input
    left_ids= List.sortAsc ids.left_ids
    right_ids= List.sortAsc ids.right_ids
    distances = List.map2 left_ids right_ids Num.absDiff
    result = List.sum distances
    Num.toStr result

part2 = \input -> 
    pairs = into_pairs input
    empty_occurences = Dict.empty {}

    increment_left = \occurance_amount ->
        when occurance_amount is
            Ok (l,r) -> Ok (l + 1, r)
            Err Missing -> Ok (1, 0)

    increment_right = \occurance_amount ->
        when occurance_amount is
            Ok (l,r) -> Ok (l, r + 1)
            Err Missing -> Ok (0, 1)

    increment_both = \occurance_amount ->
        when occurance_amount is
            Ok (l,r) -> Ok (l+1, r + 1)
            Err Missing -> Ok (1, 1)

    occurences = List.walk pairs empty_occurences \occ, {left, right} ->
        if left == right then
            Dict.update occ left increment_both
        else
            Dict.update occ left increment_left
            |> Dict.update right increment_right

    result = Dict.walk occurences 0 \sum, num, (l, r) -> sum + (num*l*r)
    Num.toStr result


separate_ids = \input ->
    list_of_pairs = into_pairs input
    len = List.len list_of_pairs
    pair_of_empty_list = { left_ids: List.withCapacity len, right_ids: List.withCapacity len }
    pair_of_list = List.walk list_of_pairs pair_of_empty_list \{ left_ids, right_ids }, pair -> {
        left_ids: List.append left_ids pair.left,
        right_ids: List.append right_ids pair.right,
    }
    pair_of_list

into_pairs = \input ->
    rows = Str.splitOn input "\n"
    list_of_pair = List.map rows \row ->
        { after, before } = Str.splitFirst row "   " |> Utils.unwrap "malformed input"
        {
            left: Str.toI64 before |> unwrap "NaN",
            right: Str.toI64 after |> unwrap "NaN",
        }
    list_of_pair
    

sample_input1 =
    """
    3   4
    4   3
    2   5
    1   3
    3   9
    3   3
    """
expect
    result = separate_ids sample_input1
    expected = { left_ids: [3, 4, 2, 1, 3, 3], right_ids: [4, 3, 5, 3, 9, 3] }
    result == expected

expect
    result = part1 sample_input1
    expected = "11"
    result == expected

expect
    result = part2 sample_input1
    expected = "31"
    result == expected

