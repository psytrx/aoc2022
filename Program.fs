let sw = new System.Diagnostics.Stopwatch()

let filename = "./input/day10.txt"

sw.Restart()
let s1 = Day10.solve1 filename
printfn "%O (%O)" s1 sw.Elapsed

sw.Restart()
let s2 = Day10.solve2 filename
printfn "%O (%O)" s2 sw.Elapsed
