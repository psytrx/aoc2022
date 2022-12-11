let sw = new System.Diagnostics.Stopwatch()

let filename = "./input/day11-test.txt"

sw.Restart()
let s1 = Day11.solve1 filename
printfn "%O (%O)" s1 sw.Elapsed

sw.Restart()
let s2 = Day11.solve2 filename
printfn "%O (%O)" s2 sw.Elapsed
