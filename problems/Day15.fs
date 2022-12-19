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

let isInRange (lo, hi) x = x >= lo && x <= hi

let countCovered ranges =
    let (elo, ehi) = extent ranges
    let mutable c = 0

    for x in [ elo..ehi ] do
        let covered = Array.exists (fun range -> isInRange range x) ranges
        if covered then c <- c + 1

    c

let beaconsInLine line sensorData =
    sensorData
    |> Array.filter (snd >> snd >> (=) line)
    |> Array.map snd
    |> Array.distinct
    |> Array.length

let findUncovered ranges =
    let (lo, hi) = extent ranges

    [ lo..hi ]
    |> List.filter (fun x -> not (Array.exists (fun range -> isInRange range x) ranges))

let solve1 filename =
    let line = 2_000_000
    let sensorData = loadData filename
    let beaconsInLine = beaconsInLine line sensorData

    let covered =
        sensorData
        |> Array.choose (sensorLineCoverage line)
        |> countCovered

    covered - beaconsInLine

let solve2 filename =
    let sensorData = loadData filename

    let line =
        [ 0..4_000_000 ]
        |> List.map (fun line ->
            printfn "%d" line

            let ranges =
                sensorData
                |> Array.choose (sensorLineCoverage line)

            let uncovered = findUncovered ranges
            line, uncovered)
        |> List.find (snd >> List.length >> (=) 1)

    line
