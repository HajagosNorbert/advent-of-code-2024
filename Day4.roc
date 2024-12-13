module [part1, part2]

import Utils exposing [unwrap]

part1 = \input ->
    # using top, bottom, left, right abbreviations
    raw_input = input |> Str.toUtf8
    le_to_ri = toRows raw_input 
    to_to_bo = toColumns raw_input 
    tl_to_br = toDiagFromTopLeft raw_input
    tr_to_bl = toDiagFromTopRight raw_input

    le_to_ri_xmas_count = List.map le_to_ri count_xmas |> List.sum
    to_to_bo_xmas_count = List.map to_to_bo count_xmas |> List.sum
    tl_to_br_xmas_count = List.map tl_to_br count_xmas |> List.sum
    tr_to_bl_xmas_count = List.map tr_to_bl count_xmas |> List.sum

    le_to_ri_samx_count = List.map le_to_ri count_samx |> List.sum
    to_to_bo_samx_count = List.map to_to_bo count_samx |> List.sum
    tl_to_br_samx_count = List.map tl_to_br count_samx |> List.sum
    tr_to_bl_samx_count = List.map tr_to_bl count_samx |> List.sum

    le_to_ri_xmas_count
    |> Num.add to_to_bo_xmas_count
    |> Num.add tl_to_br_xmas_count
    |> Num.add tr_to_bl_xmas_count
    |> Num.add le_to_ri_samx_count
    |> Num.add to_to_bo_samx_count
    |> Num.add tl_to_br_samx_count
    |> Num.add tr_to_bl_samx_count
    |> Num.toStr

toRows = \input ->
    List.splitOn input '\n'

toColumns = \input ->
    rowLength = List.findFirstIndex input (\x -> x == '\n') |> unwrap
    rowCount = (List.len input) // (rowLength+1)
    emptyCols = List.repeat (List.withCapacity rowCount) rowLength
    (list_of_columns, _) = List.walkWithIndex input (emptyCols, 0) \(cols, col_idx), char, idx ->
        if char == '\n' then
            (cols, 0)
        else
            (List.update cols col_idx \col -> List.append col char, col_idx + 1)
    list_of_columns

toDiagFromTopLeft = \input ->
    [['a'], ['b']]

toDiagFromTopRight = \input ->
    [['a'], ['b']]

count_samx = \text ->
    count_word text ['S', 'A', 'M', 'X']

count_xmas = \text ->
    count_word text ['X', 'M', 'A', 'S']

count_word = \text, word ->
    help = \input, sum ->
        when input is
            [] -> 
                sum
            [_, .. as rest] -> 
                if List.startsWith input word then
                    help rest (sum+1)
                else
                    help rest sum
    help text 0

part2 = \input ->
    "there"

sample_input =
    """
    MMMSXXMASM
    MSAMXMSMSA
    AMXSXMAAMM
    MSAMASMSMX
    XMASAMXAMM
    XXAMMXXAMA
    SMSMSASXSS
    SAXAMASAAA
    MAMMMXMMMM
    MXMXAXMASX
    """

expect
    got = part1 sample_input
    expected = "18"
    got == expected
