module Day01

let parseInput =
    System.IO.File.ReadAllLines
    >> Util.blockWise " "
    >> Seq.map (List.sumBy System.Convert.ToInt32)

let solve1 = parseInput >> Seq.max

let solve2 =
    parseInput
    >> Seq.sortDescending
    >> Seq.take 3
    >> Seq.sum
