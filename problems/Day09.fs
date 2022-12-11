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

let traceRope directions =
    let folder state curr =
        match state with
        | (trace, ((hx, hy), (tx, ty))) ->
            let nextHead =
                match curr with
                | Up -> (hx, hy - 1)
                | Down -> (hx, hy + 1)
                | Left -> (hx - 1, hy)
                | Right -> (hx + 1, hy)

            let dx = (fst nextHead) - tx
            let dy = (snd nextHead) - ty

            let nextTail =
                match (abs dx, abs dy) with
                | (2, 0) -> (tx + sign dx, ty)
                | (0, 2) -> (tx, ty + sign dy)
                | (2, 1) -> (tx + sign dx, ty + sign dy)
                | (1, 2) -> (tx + sign dx, ty + sign dy)
                | _ -> (tx, ty)

            (Set.add nextTail trace, (nextHead, nextTail))

    directions
    |> List.fold folder (Set.empty, ((0, 0), (0, 0)))
    |> fst


let solve1 =
    loadInput
    >> flattenMotions
    >> traceRope
    >> Set.count

let solve2 filename = -1
