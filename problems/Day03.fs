module Day03

let GetPriority (c: char) =
    let ord = System.Convert.ToInt32(c)

    if c >= 'a' && c <= 'z' then
        ord - 96
    else if c >= 'A' && c <= 'Z' then
        ord - 38
    else
        failwith "invalid character"

let GroupByN n (xs: 'a []) =
    let rec aux i acc =
        if i >= xs.Length then
            acc
        else
            aux (i + n) (acc @ [ xs.[i .. i + n - 1] ])

    aux 0 []

let Solve1 filename =
    System.IO.File.ReadAllLines(filename)
    |> Array.map (fun line ->
        let m = line.Length / 2
        let l, r = line[.. m - 1], line[m..]

        Set.intersect (Set.ofSeq l) (Set.ofSeq r)
        |> Seq.head
        |> GetPriority)
    |> Array.sum

let Solve2 filename =
    System.IO.File.ReadAllLines(filename)
    |> GroupByN 3
    |> List.sumBy (
        Array.map Set.ofSeq
        >> Set.intersectMany
        >> Seq.head
        >> GetPriority
    )
