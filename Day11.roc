module [part1, part2]

import Utils exposing [unwrap]

part1 = \input -> 
    input |> Str.splitOn " " |> solve 25 |> Num.toStr

solve = \stones, iters ->
    dbg iters
    if iters == 0 then
        List.len stones
    else
        newStones = blink stones 0
        solve newStones (iters-1)

blink = \stones, idx ->
    when List.get stones idx is
        Ok "0" -> 
            newStones = List.set stones idx "1"
            blink newStones (idx+1)
        Ok stone ->
            stoneMark = Str.toUtf8 stone
            stoneMarkLen = List.len stoneMark
            if stoneMarkLen % 2 == 0 then
                {before, others} = List.splitAt stoneMark (stoneMarkLen // 2)
                left = before |> Str.fromUtf8 |> unwrap |> Str.toI64 |> unwrap |> Num.toStr
                right = others |> Str.fromUtf8 |> unwrap |> Str.toI64 |> unwrap |> Num.toStr
                new_stones = List.set stones idx left |> insert (idx+1) right
                blink new_stones (idx + 2)
            else
                new_stone_mark = (Str.toI64 stone |> unwrap ) * 2024 |> Num.toStr
                new_stones = List.set stones idx new_stone_mark 
                blink new_stones (idx+1)

        Err OutOfBounds -> stones


insert = \l, idx, elem ->
    {before, others } = List.splitAt l idx
    List.append before elem |> List.concat others
    
part2 = \input -> 
    "there"

sample_input1 = "0 1 10 99 999"


expect 
    got = insert [1, 3] 1 2 
    expected = [1, 2, 3]
    got == expected

expect 
    got = blink ["0", "1", "10", "99", "999"] 0
    expected = ["1", "2024", "1", "0", "9", "9", "2021976"]
    got == expected


expect 
    got = solve ["125", "17"] 6 
    expected = 22
    got == expected
