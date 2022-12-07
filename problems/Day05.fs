module Day05

let parseStackLine (s: string) =
    let rec aux xs acc =
        match xs with
        | [ ' '; ' '; ' ' ] -> acc @ [ None ]
        | ' ' :: ' ' :: ' ' :: ' ' :: tail -> aux tail (acc @ [ None ])
        | '[' :: name :: ']' :: ' ' :: tail -> aux tail (acc @ [ Some name ])
        | [ '['; name; ']' ] -> acc @ [ Some name ]
        | _ -> failwithf "invalid stack line format: '%s'" s

    aux (Seq.toList s) []

let parseStacks lines =
    lines
    |> List.map parseStackLine
    |> List.transpose
    |> List.map (List.choose id)

type Move = { N: int; From: int; To: int }

let parseMoves lines =
    lines
    |> List.map (fun line ->
        let (n', from', to') = line |> Sscanf.sscanf "move %d from %d to %d"
        { From = from'; To = to'; N = n' })

let parseInput lines =
    let blocks = lines |> Util.blockWise "" |> List.ofSeq

    match blocks with
    | [ stackLines; moves ] ->
        let stacks' = parseStacks stackLines.[.. stackLines.Length - 2]
        let moves' = parseMoves moves
        (stacks', moves')
    | _ -> failwith "invalid input format"

let solve1 filename =
    let mutable (stacks, moves) =
        System.IO.File.ReadAllLines(filename)
        |> parseInput

    let stacks' = stacks |> List.toArray

    for move in moves do
        for _ in [ 1 .. move.N ] do
            stacks'.[move.To - 1] <-
                List.head (stacks'.[move.From - 1])
                :: stacks'.[move.To - 1]

            stacks'.[move.From - 1] <- List.tail stacks'.[move.From - 1]

    stacks'
    |> Array.map List.head
    |> System.String
    |> printfn "%O"

    -1

let solve2 filename =
    let mutable (stacks, moves) =
        System.IO.File.ReadAllLines(filename)
        |> parseInput

    let stacks' = stacks |> List.toArray

    for move in moves do
        stacks'.[move.To - 1] <-
            stacks'.[move.From - 1][.. move.N - 1]
            @ stacks'.[move.To - 1]

        stacks'.[move.From - 1] <- stacks'.[move.From - 1] |> List.skip move.N

    stacks'
    |> Array.map List.head
    |> System.String
    |> printfn "%O"

    -1
