app [main] { pf: platform "https://github.com/roc-lang/basic-cli/releases/download/0.17.0/lZFLstMUCUvd5bjnnpYromZJXkQUrdhbva4xdBInicE.tar.br" }

import pf.Stdout
import pf.Stderr
import pf.Arg
import pf.File
import Day1

main =
    args = Arg.list! {}
    firstArgRes = List.get args 1 |> Result.try Str.toU8
    when firstArgRes is
        Ok day ->
            when pickSolver day is
                Ok {part1, part2} ->
                    path = "inputs/day$(Num.toStr day).txt"
                    File.readUtf8 path |> Task.attempt \r ->
                        when r is
                            Ok input ->
                                summary = summarize (part1 input) (part2 input)
                                Stdout.line! summary
                            Err _ -> Stderr.line! "Error when reading '$(path)'. Does the file exist?"
                Err (Unsolved otherDay) -> Stderr.line! "No solution for day $(Num.toStr otherDay) exists."
        Err OutOfBounds -> Stderr.line! "How am I supposed to know which day to solve? give me an arg from 1-25."
        Err InvalidNumStr -> Stderr.line! "The arg should be a number from 1 to 25."

pickSolver = \day ->
    when day is
        1 -> Ok { part1 : Day1.part1 , part2: Day1.part2}
        otherDay -> Err (Unsolved otherDay)

summarize = \part1Solution, part2Solution ->
    """
    Part 1: $(part1Solution)
    Part 1: $(part2Solution)
    """
