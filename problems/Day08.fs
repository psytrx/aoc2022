module Day08

let loadTreeGrid =
    System.IO.File.ReadAllLines
    >> Array.map (fun line ->
        line.ToCharArray()
        |> Array.map (string >> System.Convert.ToInt32))

let hasObstruction height xs = Array.exists (fun h -> h >= height) xs

let isVisible y x (grid: int array array) =
    let height = grid.[y].[x] |> Util.dump "%i"

    let top =
        grid.[.. y - 1]
        |> Array.map (fun row -> row.[x])
        |> hasObstruction height

    let bottom =
        grid.[y + 1 ..]
        |> Array.map (fun row -> row.[x])
        |> hasObstruction height

    let left = grid.[y].[.. x - 1] |> hasObstruction height
    let right = grid.[y].[x + 1 ..] |> hasObstruction height

    not (top && bottom && left && right)

let solve1 filename =
    let grid = loadTreeGrid filename

    grid
    |> Array.mapi (fun y row -> row |> Array.mapi (fun x _ -> isVisible y x grid))
    |> Array.sumBy (Array.filter id >> Array.length)

let solve2 filename = -1
