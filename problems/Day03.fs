module Day03

let GetPriority (c: char) =
    let ord = System.Convert.ToInt32(c)

    if c >= 'a' && c <= 'z' then
        ord - 96
    else if c >= 'A' && c <= 'Z' then
        ord - 38
    else
        failwith "invalid character"

let GroupByThree (xs: 'a []) =
    let rec aux i acc =
        if i >= xs.Length then
            acc
        else
            aux (i + 3) (acc @ [ xs.[i .. i + 2] ])

    aux 0 []

let Solve1 filename =
    System.IO.File.ReadAllLines(filename)
    |> Array.map (fun line ->
        let m = line.Length / 2
        let l, r = line[.. m - 1], line[m..]
        let isect = Set.intersect (Set.ofSeq l) (Set.ofSeq r)

        if isect.Count <> 1 then
            failwith "invalid number of intersections"

        isect |> Set.toArray |> Array.head |> GetPriority)
    |> Array.sum

let Solve2 filename =
    System.IO.File.ReadAllLines(filename)
    |> GroupByThree
    |> List.map (fun bags ->
        let isect = bags |> Array.map Set.ofSeq |> Set.intersectMany

        if isect.Count <> 1 then
            failwith "invalid number of intersections"

        isect |> Set.toArray |> Array.head |> GetPriority)
    |> List.sum
