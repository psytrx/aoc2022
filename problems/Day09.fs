module Day09

type Direction =
    | Up
    | Down
    | Left
    | Right

let parseLine (line: string) =
    let dir, d = Sscanf.sscanf "%s %d" line

    match dir with
    | "U" -> (Up, d)
    | "D" -> (Down, d)
    | "L" -> (Left, d)
    | "R" -> (Right, d)
    | _ -> failwithf "Invalid direction '%s'" dir

let flattenMotions motions =
    let rec aux acc motions =
        match motions with
        | [] -> acc
        | (dir, n) :: rest -> aux (acc @ List.replicate n dir) rest

    aux [] motions

let relaxKnot (hx, hy) (tx, ty) =
    let (dx, dy) = (hx - tx, hy - ty)

    match (abs dx, abs dy) with
    | (2, 0) -> (tx + sign dx, ty)
    | (0, 2) -> (tx, ty + sign dy)
    | (2, _) -> (tx + sign dx, ty + sign dy)
    | (_, 2) -> (tx + sign dx, ty + sign dy)
    | _ -> (tx, ty)

let pullRope direction rope =
    match rope with
    | [] -> rope
    | head :: rest ->
        let nextHead =
            match head with
            | (x, y) ->
                match direction with
                | Up -> (x, y - 1)
                | Down -> (x, y + 1)
                | Left -> (x - 1, y)
                | Right -> (x + 1, y)

        nextHead :: rest

let relaxRope rope =
    let rec aux acc rope =
        match rope with
        | [] -> acc
        | [ last ] -> acc @ [ last ]
        | head :: next :: rest ->
            let relaxedNext = relaxKnot head next
            aux (acc @ [ head ]) (relaxedNext :: rest)

    aux [] rope

let traceRope length =
    List.fold
        (fun state dir ->
            match state with
            | (trace, rope) ->
                let nextRope = pullRope dir rope |> relaxRope
                let nextTrace = Set.add (List.last nextRope) trace
                (nextTrace, nextRope))
        (set [ (0, 0) ], List.replicate length (0, 0))
    >> fst

let solve n =
    System.IO.File.ReadAllLines
    >> Array.map parseLine
    >> Array.toList
    >> flattenMotions
    >> traceRope n
    >> Set.count

let solve1 = solve 2

let solve2 = solve 10
