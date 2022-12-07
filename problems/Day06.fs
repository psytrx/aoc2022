module Day06

let findPacketMarker signal =
    let rec aux i len signal =
        let set = Set.ofList (List.take len signal)

        if set.Count = len then
            i + len
        else
            aux (i + 1) len (List.skip 1 signal)

    aux 0 signal

let solve n =
    System.IO.File.ReadAllText
    >> List.ofSeq
    >> findPacketMarker n

let solve1 = solve 4
let solve2 = solve 14
