module [part1, part2]
import Utils exposing [unwrap]

part1 = \input ->
    reports = parseInput input
    safeties = List.map reports isSafe
    count_safes safeties |> Num.toStr

part2 = \input ->
    reports = parseInput input
    safeties = List.map reports isSafe
    retried_safeties = List.map2 reports safeties retryUnsafeReport
    count_safes retried_safeties |> Num.toStr

count_safes = \safetyList ->
    List.countIf
        safetyList
        (\safety ->
            when safety is
                Unsafe _ -> Bool.false
                Safe -> Bool.true)

retryUnsafeReport = \report, safety ->
    when safety is
        Safe -> Safe
        Unsafe faultyIdx ->
            safety_without_middle = isSafe (List.dropAt report faultyIdx) 
            when safety_without_middle is
                Safe -> Safe
                Unsafe _ -> 
                    safety_without_right = isSafe (List.dropAt report (faultyIdx+1)) 
                    when safety_without_right is
                        Safe -> Safe
                        Unsafe _ if faultyIdx > 0 -> isSafe (List.dropAt report (faultyIdx-1)) 
                        Unsafe _ -> Unsafe faultyIdx

parseInput = \input ->
    Str.splitOn input "\n"
    |> List.map \row ->
        Str.splitOn row " "
        |> List.map \num ->
            Str.toI64 num |> unwrap

isSafe = \initialReport ->
    help = \report, state, idx ->
        when (state, report) is
            (Start, []) | (Start, [_]) -> Safe
            (Start, [a, b, ..]) ->
                diff = b - a
                rest = List.takeLast report (List.len report - 1)
                if safely_ascending diff then
                    help rest Asc (idx + 1)
                else if safely_descending diff then
                    help rest Desc (idx +1)
                else
                    Unsafe idx

            (Asc, [_]) -> Safe
            (Asc, [a, b, ..]) ->
                diff = b - a
                if safely_ascending diff then
                    rest = List.takeLast report (List.len report - 1)
                    help rest Asc (idx + 1)
                else
                    Unsafe idx

            (Desc, [a, b, ..]) ->
                diff = b - a
                if safely_descending diff then
                    rest = List.takeLast report (List.len report - 1)
                    help rest Desc (idx + 1)
                else
                    Unsafe idx

            (Desc, [_]) -> Safe
            (Asc, []) | (Desc, []) -> crash "unreachable"

    help initialReport Start 0

safely_ascending = \diff ->
    1 <= diff && diff <= 3

safely_descending = \diff ->
    -3 <= diff && diff <= -1

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

expect
    got = part2 sample_input1
    expected = "4"
    got == expected
