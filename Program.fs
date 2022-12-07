let sw = new System.Diagnostics.Stopwatch()

let filename = "./input/day05-test.txt"

sw.Restart()
let s1 = Day05.Solve1 filename
printfn "%i (%O)" s1 sw.Elapsed

sw.Restart()
let s2 = Day05.Solve2 filename
printfn "%i (%O)" s2 sw.Elapsed
