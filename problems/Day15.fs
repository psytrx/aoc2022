module Day15

let solve1 filename =
    let data =
        System.IO.File.ReadAllLines(filename)
        |> Array.map (fun line ->
            let sx, sy, bx, by =
                Sscanf.sscanf "Sensor at x=%d, y=%d: closest beacon is at x=%d, y=%d" line

            let r = abs (bx - sx) + abs (by - sy)
            (sx, sy), (by, by), r)

    let coverage line ((x, y), _, r) =
        let dy = abs (line - y)

        if dy > r then
            []
        else
            let dx = abs (r - dy)
            [ x - dx .. x + dx ]

    let combinedCoverage line =
        let beaconsInLine =
            data
            |> Array.filter (fun (_, (_, by), _) -> by = line)
            |> Array.map (fun (_, (bx, _), _) -> bx)

        data
        |> Array.map (coverage line)
        |> Array.collect List.toArray
        |> Array.distinct
        |> Array.except beaconsInLine
        |> Array.length

    combinedCoverage 2000000

let solve2 filename = -1
