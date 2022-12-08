module Day08

let loadTreeGrid =
    System.IO.File.ReadAllLines
    >> Array.map (fun line ->
        line.ToCharArray()
        |> Array.map (string >> System.Convert.ToInt32))

let hasObstruction height xs = Array.exists (fun h -> h >= height) xs

let getDirections y x (grid: 'a array array) =
    let up =
        grid.[.. y - 1]
        |> Array.map (fun row -> row.[x])
        |> Array.rev

    let down = grid.[y + 1 ..] |> Array.map (fun row -> row.[x])
    let left = grid.[y].[.. x - 1] |> Array.rev
    let right = grid.[y].[x + 1 ..]

    [ up; left; down; right ]

let viewingDistance height row =
    let rec aux height row =
        match row with
        | [] -> 0
        | t :: rest ->
            if t >= height then
                1
            else
                1 + aux height rest

    aux height row

let isVisible y x (grid: int array array) =
    let height = grid.[y].[x]

    grid
    |> getDirections y x
    |> List.exists (hasObstruction height >> not)

let scenicScore y x (grid: int array array) =
    let height = grid.[y].[x]

    grid
    |> getDirections y x
    |> List.map (Array.toList >> viewingDistance height)
    |> List.reduce (*)

let solve1 filename =
    let grid = loadTreeGrid filename

    grid
    |> Array.mapi (fun y row -> row |> Array.mapi (fun x _ -> isVisible y x grid))
    |> Array.sumBy (Array.filter id >> Array.length)

let solve2 filename =
    let grid = loadTreeGrid filename

    grid
    |> Array.mapi (fun y row ->
        row
        |> Array.mapi (fun x _ -> scenicScore y x grid)
        |> Array.max)
    |> Array.max
