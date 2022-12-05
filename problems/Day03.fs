module Day03

let GetPriority (c: char) =
    let ord = System.Convert.ToInt32(c)

    if c >= 'a' && c <= 'z' then
        ord - 96
    else if c >= 'A' && c <= 'Z' then
        ord - 38
    else
        failwith "invalid character"

let Solve1 filename =
    System.IO.File.ReadAllLines(filename)
    |> Array.map (fun line ->
        let mid = line.Length / 2
        let l, r = line[.. mid - 1], line[mid..]
        let isect = Set.intersect (Set.ofSeq l) (Set.ofSeq r)

        if isect.Count <> 1 then
            failwith "invalid number of intersections"

        let c = isect |> Set.toArray |> Array.head
        let p = GetPriority c
        p)
    |> Array.sum

let Solve2 filename =
    System.IO.File.ReadAllLines(filename)
    |> Array.length
