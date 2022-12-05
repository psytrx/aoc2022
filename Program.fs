let sw = new System.Diagnostics.Stopwatch()

sw.Restart()
let s1 = Day01.solve1 ()
printfn "%i (%O)" s1 sw.Elapsed

sw.Restart()
let s2 = Day01.solve2 ()
printfn "%i (%O)" s2 sw.Elapsed
