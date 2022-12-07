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

let parseInput lines =
    let blocks = lines |> Util.blockWise "" |> List.ofSeq

    match blocks with
    | [ stackLines; moveLines ] ->
        let stacks = parseStacks stackLines.[.. stackLines.Length - 2]

        let moves =
            moveLines
            |> List.map (Sscanf.sscanf "move %i from %i to %i")

        (stacks, moves)
    | _ -> failwith "invalid input format"

let solve applyMove filename =
    let (stacks, moves) =
        System.IO.File.ReadAllLines(filename)
        |> parseInput

    let mutable stacks' = stacks |> List.toArray

    for move in moves do
        stacks' <- applyMove stacks' move

    stacks' |> Array.map List.head |> System.String

let solve1 filename =
    let applyMove (stacks: list<char> array) move =
        match move with
        | (from, to', n) ->
            for _ in [ 1..n ] do
                stacks.[to' - 1] <- List.head (stacks.[from - 1]) :: stacks.[to' - 1]
                stacks.[from - 1] <- List.tail stacks.[from - 1]

            stacks

    solve applyMove filename


let solve2 filename =
    let applyMove (stacks: list<char> array) move =
        match move with
        | (from, to', n) ->
            stacks.[to' - 1] <- stacks.[from - 1][.. n - 1] @ stacks.[to' - 1]
            stacks.[from - 1] <- stacks.[from - 1] |> List.skip n

        stacks

    solve applyMove filename
