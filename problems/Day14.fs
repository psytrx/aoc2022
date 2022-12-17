module Day14

type Cell =
    | Air
    | Rock
    | Sand

let solve1 filename =
    let lines =
        System.IO.File.ReadAllLines(filename)
        |> Array.map (fun line ->
            line.Split(" -> ")
            |> Array.map (Sscanf.sscanf "%d,%d"))

    let maxY =
        lines
        |> Array.map (fun line -> line |> Array.map snd |> Array.max)
        |> Array.max

    let mutable grid = Array.init 1000 (fun x -> Array.init (maxY + 2) (fun y -> Air))

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
                    grid.[x].[y] <- Rock

        ))


    let s = (500, 0)

    let rec advance rested (x, y) (grid: Cell array array) =
        if y > maxY then
            rested
        else if grid.[x].[y + 1] = Air then
            advance rested (x, y + 1) grid
        else if grid.[x - 1].[y + 1] = Air then
            advance rested (x - 1, y + 1) grid
        else if grid.[x + 1].[y + 1] = Air then
            advance rested (x + 1, y + 1) grid
        else
            grid.[x].[y] <- Sand
            advance (rested + 1) s grid

    advance 0 s grid

let solve2 filename = -1
