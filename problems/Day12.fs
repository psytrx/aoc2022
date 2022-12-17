module Day12

let solve1 filename =
    let nodes =
        System.IO.File.ReadAllLines(filename)
        |> Array.mapi (fun y s ->
            s.ToCharArray()
            |> Array.mapi (fun x c -> ((x, y), c)))
        |> Array.collect id
        |> Util.dump "%A"

    let S = nodes |> Array.find (snd >> (=) 'S')
    let E = nodes |> Array.find (snd >> (=) 'E')

    let elevation c =
        match c with
        | 'S' -> int 'a'
        | 'E' -> int 'z'
        | c -> int c

    let edges ((x, y), c) =
        [ (-1, 0); (0, -1); (1, 0); (0, 1) ]
        |> List.choose (fun (ox, oy) ->
            nodes
            |> Array.tryFind (fun ((nx, ny), _) -> x + ox = nx && y + oy = ny))
        |> List.filter (fun ((_, _), cn) -> elevation cn <= elevation c + 1)


    let rec aux q visited =
        match q with
        | [] -> failwith "no path found, queue empty"
        | (curr, t) :: rest ->
            if curr = E then
                t
            else if Set.contains curr visited then
                aux rest visited
            else
                let nextVisited = Set.add curr visited
                let neighbors = edges curr |> List.map (fun n -> (n, n :: t))
                let nextQ = rest @ neighbors
                aux nextQ nextVisited

    aux [ (S, []) ] Set.empty
    |> Util.dump "%A"
    |> List.length

let solve2 filename = -1
