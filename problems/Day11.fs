module Day11

let loadNotes filename =
    System.IO.File.ReadAllLines filename
    |> Util.blockWise " "

let solve1 filename = loadNotes filename

let solve2 filename = -1
