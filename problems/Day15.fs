module Day15

let loadData filename =
    System.IO.File.ReadAllLines(filename)
    |> Array.map (fun line ->
        let sx, sy, bx, by =
            Sscanf.sscanf "Sensor at x=%d, y=%d: closest beacon is at x=%d, y=%d" line

        (sx, sy), (bx, by))

let sensorLineCoverage line ((sx, sy), (bx, by)) =
    let md = abs (bx - sx) + abs (by - sy)
    let dy = abs (line - sy)

    if dy > md then
        None
    else
        let dx = md - dy
        Some(sx - dx, sx + dx)

let extent ranges =
    let lo = Array.map fst ranges |> Array.min
    let hi = Array.map snd ranges |> Array.max
    lo, hi

let countCovered ranges =
    let (elo, ehi) = extent ranges
    let mutable c = 0

    for x in [ elo..ehi ] do
        let covered = Array.exists (fun (lo, hi) -> x >= lo && x <= hi) ranges
        if covered then c <- c + 1

    c

let solve1 filename =
    let sensorData = loadData filename

    let line = 2_000_000

    let beaconsInLine =
        sensorData
        |> Array.filter (snd >> snd >> (=) line)
        |> Array.map snd
        |> Array.distinct
        |> Array.length

    let covered =
        sensorData
        |> Array.choose (sensorLineCoverage line)
        |> countCovered

    covered - beaconsInLine

let solve2 filename = -1
