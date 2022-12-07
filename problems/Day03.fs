module Day03

let priority (c: char) =
    let ord = int c

    if c >= 'a' && c <= 'z' then
        ord - 96
    else if c >= 'A' && c <= 'Z' then
        ord - 38
    else
        failwith "invalid character"


let solve1 filename =
    System.IO.File.ReadAllLines(filename)
    |> Array.map (fun line ->
        let m = line.Length / 2
        let l, r = line[.. m - 1], line[m..]

        Set.intersect (Set.ofSeq l) (Set.ofSeq r)
        |> Seq.head
        |> priority)
    |> Array.sum

let solve2 filename =
    System.IO.File.ReadAllLines(filename)
    |> Util.bundleWise 3
    |> List.sumBy (
        Array.map Set.ofSeq
        >> Set.intersectMany
        >> Seq.head
        >> priority
    )
