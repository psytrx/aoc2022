module Day09

type Direction =
    | Up
    | Down
    | Left
    | Right

let parseLine (line: string) =
    match line.Split(" ") with
    | [| dir; dist |] ->
        let d = System.Convert.ToInt32(dist)

        match dir with
        | "U" -> (Up, d)
        | "D" -> (Down, d)
        | "L" -> (Left, d)
        | "R" -> (Right, d)
        | _ -> failwithf "Invalid direction '%s'" dir
    | _ -> failwithf "Invalid line '%s'" line

let loadInput =
    System.IO.File.ReadAllLines
    >> Array.map parseLine
    >> Array.toList

let flattenMotions motions =
    let rec aux acc motions =
        match motions with
        | [] -> acc
        | (dir, 1) :: rest -> aux (acc @ [ dir ]) rest
        | (dir, n) :: rest -> aux (acc @ [ dir ]) ((dir, n - 1) :: rest)

    aux [] motions

let moveKnot direction knot =
    match knot with
    | (x, y) ->
        match direction with
        | Up -> (x, y - 1)
        | Down -> (x, y + 1)
        | Left -> (x - 1, y)
        | Right -> (x + 1, y)

let relaxKnot (hx, hy) (tx, ty) =
    let (dx, dy) = (hx - tx, hy - ty)

    match (abs dx, abs dy) with
    | (2, 0) -> (tx + sign dx, ty)
    | (0, 2) -> (tx, ty + sign dy)
    | (2, _) -> (tx + sign dx, ty + sign dy)
    | (_, 2) -> (tx + sign dx, ty + sign dy)
    | _ -> (tx, ty)

let moveRope direction rope =
    match rope with
    | [] -> rope
    | head :: rest -> (moveKnot direction head) :: rest

let relaxRope rope =
    let rec aux acc rope =
        match rope with
        | [] -> acc
        | [ last ] -> acc @ [ last ]
        | head :: next :: rest ->
            let relaxedNext = relaxKnot head next
            aux (acc @ [ head ]) (relaxedNext :: rest)

    aux [] rope

let traceRope length directions =
    let folder state dir =
        match state with
        | (trace, rope) ->
            let nextRope = moveRope dir rope |> relaxRope
            let last = List.last nextRope
            let nextTrace = Set.add last trace
            (nextTrace, nextRope)

    directions
    |> List.fold folder (set [ (0, 0) ], List.replicate length (0, 0))
    |> fst

let solve n =
    loadInput
    >> flattenMotions
    >> traceRope n
    >> Set.count

let solve1 = solve 2

let solve2 = solve 10
