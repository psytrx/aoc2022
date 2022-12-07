module Day06

let findStartOfPacketMarker signal =
    let rec aux i signal =
        match signal with
        | a :: b :: c :: d :: rest ->
            let set = Set([ a; b; c; d ])

            if set.Count = 4 then
                i + 4
            else
                aux (i + 1) (b :: c :: d :: rest)
        | _ -> failwith "could not find start of packet marker"

    aux 0 signal

let solve1 filename =
    System.IO.File.ReadAllText(filename)
    |> List.ofSeq
    |> findStartOfPacketMarker

let solve2 filename = -1
