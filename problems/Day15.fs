module Day15

let loadSensorData filename =
    System.IO.File.ReadAllLines(filename)
    |> Array.map (fun line ->
        let sx, sy, bx, by =
            Sscanf.sscanf "Sensor at x=%d, y=%d: closest beacon is at x=%d, y=%d" line

        let r = abs (bx - sx) + abs (by - sy)
        (sx, sy), (by, by), r)

let sensorLineCoverage line ((x, y), _, r) =
    let dy = abs (line - y)

    if dy > r then
        []
    else
        let dx = abs (r - dy)
        [ x - dx .. x + dx ]

let combinedLineCoverage line sensorData =
    let beaconsInLine =
        sensorData
        |> Array.filter (fun (_, (_, by), _) -> by = line)
        |> Array.map (fun (_, (bx, _), _) -> bx)

    sensorData
    |> Array.map (sensorLineCoverage line)
    |> Array.collect List.toArray
    |> Array.distinct
    |> Array.except beaconsInLine
    |> Array.length

let solve1 filename =
    loadSensorData filename
    |> combinedLineCoverage 2000000

let solve2 filename = -1
