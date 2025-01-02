module [part1, part2]

import Utils exposing [unwrap]

part1 = \input ->
    # using top, bottom, left, right abbreviations
    raw_input = input |> Str.toUtf8
    dimensions = calc_dimensions raw_input
    le_to_ri = toRows raw_input
    to_to_bo = toColumns raw_input dimensions
    tl_to_br = toDiagFromTopLeft raw_input dimensions
    tr_to_bl = toDiagFromTopRight raw_input dimensions

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

part2 = \input ->
    raw_input = input |> Str.toUtf8
    dimensions = calc_dimensions raw_input
    tl_to_br = toDiagFromTopLeft raw_input dimensions
    tr_to_bl = toDiagFromTopRight raw_input dimensions

    tl_to_br_mas_count = List.map tl_to_br count_mas |> List.sum
    tr_to_bl_mas_count = List.map tr_to_bl count_mas |> List.sum

    tl_to_br_sam_count = List.map tl_to_br count_sam |> List.sum
    tr_to_bl_sam_count = List.map tr_to_bl count_sam |> List.sum

    tl_to_br_mas_count
    |> Num.add tr_to_bl_mas_count
    |> Num.add tl_to_br_sam_count
    |> Num.add tr_to_bl_sam_count
    |> Num.toStr

toRows = \input ->
    List.splitOn input '\n'

toColumns = \input, { rowLength, rowCount } ->
    emptyCols = List.repeat (List.withCapacity rowCount) rowLength
    list_of_columns = List.walkWithIndex input emptyCols \cols, char, idx ->
        if char == '\n' then
            cols
        else
            col_idx = idx % (rowLength + 1)
            List.update cols col_idx \col -> List.append col char
    list_of_columns

toDiagFromTopLeft = \input, dimensions ->
    dimensions
    |> reserveDiags 
    |> fill_diagonals input (diag_mapper_from_top_left dimensions.rowLength)
    
toDiagFromTopRight = \input, dimensions  ->
    dimensions
    |> reserveDiags 
    |> fill_diagonals input (diag_mapper_from_top_right dimensions.rowLength)

diag_mapper_from_top_left = \rowLength -> \char_idx ->
    y = char_idx // (rowLength + 1)
    x = char_idx % (rowLength + 1)
    y + rowLength - 1 - x
    
diag_mapper_from_top_right = \rowLength -> \char_idx ->
    y = char_idx // (rowLength + 1)
    x = char_idx % (rowLength + 1)
    y + x

fill_diagonals = \list_of_empty_diags, input, char_idx_to_diag_idx ->
    List.walkWithIndex input list_of_empty_diags \diags, char, char_idx ->
        if char == '\n' then
            diags
        else
            diag_idx = char_idx_to_diag_idx char_idx
            List.update diags diag_idx \diag -> List.append diag char

calc_diagonal_count = \rowLength, rowCount -> rowLength + rowCount - 1

reserveDiags = \{ rowLength, rowCount } ->
    empty_list_for_diags = List.withCapacity (calc_diagonal_count rowLength rowCount)
    { shorter, longer } =
        if rowLength < rowCount then
            { shorter: rowLength, longer: rowCount }
        else
            { shorter: rowCount, longer: rowLength }


    reserveAscending = \diags, diagLength ->
        if diagLength < shorter then
            newDiags = List.append diags (List.withCapacity diagLength)
            reserveAscending newDiags (diagLength + 1)
        else
            diags

    reserveSameLengthed = \diags, remaining ->
        if remaining > 0 then
            newDiags = List.append diags (List.withCapacity shorter)
            reserveSameLengthed newDiags (remaining - 1)
        else
            diags

    reserveDescending = \diags, diagLength ->
        if diagLength > 0 then
            newDiags = List.append diags (List.withCapacity diagLength)
            reserveDescending newDiags (diagLength - 1)
        else
            diags

    sameLengthedDiagonalCount = longer - shorter + 1

   empty_list_for_diags 
    |> reserveAscending 1
    |> reserveSameLengthed sameLengthedDiagonalCount
    |> reserveDescending (shorter - 1)


calc_dimensions = \input ->
    rowLength = List.findFirstIndex input (\x -> x == '\n') |> unwrap
    rowCount = (List.len input) // (rowLength + 1)
    { rowLength, rowCount }

count_samx = \text ->
    count_word text ['S', 'A', 'M', 'X']

count_xmas = \text ->
    count_word text ['X', 'M', 'A', 'S']

count_mas = \text ->
    count_word text ['M', 'A', 'S']

count_sam = \text ->
    count_word text ['S', 'A', 'M']

count_word = \text, word ->
    help = \input, sum ->
        when input is
            [] ->
                sum

            [_, .. as rest] ->
                if List.startsWith input word then
                    help rest (sum + 1)
                else
                    help rest sum
    help text 0


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
    got = fill_diagonals [[], [], [], []] [1, 2, 3, '\n', 4, 5, 6] (diag_mapper_from_top_right 3)
    expected = [[1], [2, 4], [3, 5], [6]]
    got == expected


expect
    got = part1 sample_input
    expected = "18"
    got == expected
