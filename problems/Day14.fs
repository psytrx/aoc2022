module Day14

type Cell =
    | Air
    | Rock
    | Sand

let loadGrid filename =
    let lines =
        System.IO.File.ReadAllLines(filename)
        |> Array.map (fun line ->
            line.Split(" -> ")
            |> Array.map (Sscanf.sscanf "%d,%d"))

    let maxY =
        lines
        |> Array.map (fun line -> line |> Array.map snd |> Array.max)
        |> Array.max

    let mutable grid =
        Array.init 1000 (fun x -> Array.init (maxY + 3) (fun y -> if y = maxY + 2 then Rock else Air))

    lines
    |> Array.iter (fun line ->
        line
        |> Array.pairwise
        |> Array.iter (fun (a, b) ->
            if fst a <> fst b then
                let y = snd a

                for x in [ fst a .. sign (fst b - fst a) .. fst b ] do
                    grid.[x].[y] <- Rock
            else
                let x = fst a

                for y in [ snd a .. sign (snd b - snd a) .. snd b ] do
                    grid.[x].[y] <- Rock))

    grid, maxY

let rec advance shouldStop rested start (x, y) (grid: Cell array array) =
    if shouldStop (x, y) then
        rested
    else if grid.[x].[y + 1] = Air then
        advance shouldStop rested start (x, y + 1) grid
    else if grid.[x - 1].[y + 1] = Air then
        advance shouldStop rested start (x - 1, y + 1) grid
    else if grid.[x + 1].[y + 1] = Air then
        advance shouldStop rested start (x + 1, y + 1) grid
    else
        grid.[x].[y] <- Sand

        if (x, y) = start then
            rested + 1
        else
            advance shouldStop (rested + 1) start start grid

let solve1 filename =
    let grid, maxY = loadGrid filename
    advance (fun (_, y) -> y > maxY) 0 (500, 0) (500, 0) grid

let solve2 filename =
    let grid, _ = loadGrid filename
    advance (fun _ -> false) 0 (500, 0) (500, 0) grid
