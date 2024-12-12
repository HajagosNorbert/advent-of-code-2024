module [part1, part2]

import Utils exposing [unwrap]

part1 =\ input ->
    solve1 (input |> Str.toUtf8) |> Num.toStr

solve1 = \initial_input ->
    help =\input, state, sum ->
        when (state,input) is
            # (a, [])-> sum 
            (Start, []) |(Num1, []) |(Comma _, []) | (ClosingParen _ _, []) -> sum 
            (Start, ['m', 'u', 'l', '(', .. as rest]) ->
                help rest Num1 sum
            (Start, [_, .. as rest]) -> help rest Start sum
            (Num1, inp) -> 
                when parseNumber inp is
                    Err remaining -> help remaining Start sum
                    Ok num remaining -> help remaining (Comma num) sum
            (Comma num1, [',', .. as rest]) -> help rest (Num2 num1) sum
            (Comma _, [_, .. as rest]) -> help rest Start sum
            (Num2 num1, inp) ->
                when parseNumber inp is
                    Err remaining -> help remaining Start sum
                    Ok num2 remaining -> help remaining (ClosingParen num1 num2) sum
            (ClosingParen num1 num2, [')', .. as rest]) -> help rest Start (sum + num1 * num2)
            (ClosingParen _ _, [_, .. as rest]) -> help rest Start sum 

    help initial_input Start 0
    
parseNumber = \initial_input ->
    countDigits = \input, digits ->
        when input is
            [] -> digits
            [a, .. as rest] if digit a -> countDigits rest (digits+1) 
            [_, .. ] -> digits


    count = countDigits initial_input 0 
    {before, others} = List.splitAt initial_input count
    if 1 <= count && count <=  3 then
        num = Str.fromUtf8 before |> unwrap |> Str.toU64 |> unwrap
        Ok num others
    else
        Err others

digit = \char ->
    '0' <= char && char <= '9'

part2 =\ input ->
    "hi"

sample_input = "xmul(2,4)%&mul[3,7]!@^do_not_mul(5,5)+mul(32,64]then(mul(11,8)mul(8,5))"

expect
    got = part1 sample_input
    expected = "161"
    got == expected

expect
    got = parseNumber ['1', '2', 'A']
    expected = Ok 12 ['A']
    got == expected