module [part1, part2]
import Utils exposing [unwrap, ident]

part1 = \input ->
    reports = parseInput input
    safeties = List.map reports isSafe
    List.countIf safeties ident |> Num.toStr
    # dbg safeties
    #
    # "hi"

parseInput = \input ->
    Str.splitOn input "\n" |> List.map \row -> 
        Str.splitOn row " " |> List.map \num -> 
            Str.toI64 num |> unwrap

isSafe = \ initialReport ->
    help = \report, state ->
        when (state,report) is
            (Start, []) | (Start, [_]) -> Bool.true
            (Start, [a, b, .. ]) ->
                diff = b - a
                rest = List.takeLast report (List.len report - 1)
                if safely_ascending diff then
                    help rest Asc
                else if safely_descending diff then
                    help rest Desc
                else
                    Bool.false

            (Asc, [_]) -> Bool.true
            (Asc, [a, b, ..]) ->
                diff = b - a
                if safely_ascending diff then
                    rest = List.takeLast report (List.len report - 1)
                    help rest Asc
                else
                    Bool.false

            (Desc, [a, b, ..]) ->
                diff = b - a
                if safely_descending diff then
                    rest = List.takeLast report (List.len report - 1)
                    help rest Desc
                else
                    Bool.false

            (Desc, [_]) -> Bool.true

            (Asc, []) | (Desc, []) -> crash "unreachable"

    help initialReport Start

safely_ascending = \diff ->
    1 <= diff && diff <= 3

safely_descending = \diff ->
    -3 <= diff && diff <= -1

part2 = \input ->
    "there"

sample_input1 =
    """
    7 6 4 2 1
    1 2 7 8 9
    9 7 6 2 1
    1 3 2 4 5
    8 6 4 4 1
    1 3 6 7 9
    """

expect
    got = part1 sample_input1
    expected = "2"
    got == expected
