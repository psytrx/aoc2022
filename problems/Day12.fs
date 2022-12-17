module Day12

let loadNodes filename =
    System.IO.File.ReadAllLines(filename)
    |> Array.mapi (fun y s ->
        s.ToCharArray()
        |> Array.mapi (fun x c -> ((x, y), c)))
    |> Array.collect id

let findPath start nodes =
    let E = nodes |> Array.find (snd >> (=) 'E')

    let elevation node =
        if node = start then
            int 'a'
        else
            match node with
            | (_, 'E') -> int 'z'
            | (_, c) -> int c

    let edges ((x, y), c) =
        [ (-1, 0); (0, -1); (1, 0); (0, 1) ]
        |> List.choose (fun (ox, oy) ->
            nodes
            |> Array.tryFind (fun ((nx, ny), _) -> x + ox = nx && y + oy = ny))
        |> List.filter (fun en -> elevation en <= elevation ((x, y), c) + 1)

    let rec aux q visited =
        match q with
        | [] -> None
        | (curr, t) :: rest ->
            if curr = E then
                Some t
            else if Set.contains curr visited then
                aux rest visited
            else
                let nextVisited = Set.add curr visited
                let neighbors = edges curr |> List.map (fun n -> (n, n :: t))
                let nextQ = rest @ neighbors
                aux nextQ nextVisited

    aux [ (start, []) ] Set.empty


let solve1 filename =
    let nodes = loadNodes filename
    let S = nodes |> Array.find (snd >> (=) 'S')

    match findPath S nodes with
    | None -> failwith "no path found"
    | Some t -> List.length t

let solve2 filename =
    let nodes = loadNodes filename

    let starts =
        nodes
        |> Array.filter (fun (_, c) -> c = 'a' || c = 'S')

    starts
    |> Array.choose (fun s -> findPath s nodes)
    |> Array.map List.length
    |> Array.min
