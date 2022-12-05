let sw = new System.Diagnostics.Stopwatch()

let filename = "./input/day04.txt"

sw.Restart()
let s1 = Day04.Solve1 filename
printfn "%i (%O)" s1 sw.Elapsed

sw.Restart()
let s2 = Day04.Solve2 filename
printfn "%i (%O)" s2 sw.Elapsed
