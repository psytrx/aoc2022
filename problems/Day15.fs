module Day15

let loadData filename =
    let data =
        System.IO.File.ReadAllLines(filename)
        |> Array.map (fun line ->
            let sx, sy, bx, by =
                Sscanf.sscanf "Sensor at x=%d, y=%d: closest beacon is at x=%d, y=%d" line

            let r = abs (sx - bx) + abs (sy - by)
            ((sx, sy), r), (bx, by))

    let sensors = data |> Array.map fst |> Array.toList
    let beacons = data |> Array.map snd |> Set.ofArray |> Set.toList
    sensors, beacons

let lineCoverage n sensors =
    sensors
    |> List.choose (fun ((sx, sy), r) ->
        let dy = abs (sy - n)

        if dy > r then
            None
        else
            let dx = r - dy
            Some(sx - dx, sx + dx))

let compressRanges ranges =
    let rec aux acc =
        function
        | [] -> acc
        | hd :: tl ->
            match acc with
            | [] -> aux [ hd ] tl
            | last :: rest ->
                if fst hd <= snd last then
                    let merged = (min (fst hd) (fst last), max (snd hd) (snd last))
                    aux (merged :: rest) tl
                else
                    aux (hd :: acc) tl

    List.rev (aux [] (List.sortBy fst ranges))

let countCoverage ranges =
    ranges |> List.sumBy (fun (lo, hi) -> hi - lo + 1)

let rangeExtent ranges =
    let lo = ranges |> List.map fst |> List.min
    let hi = ranges |> List.map snd |> List.max
    (lo, hi)

let solve1 filename =
    let sensors, beacons = loadData filename
    let line = 2_000_000
    let coverage = lineCoverage line sensors |> compressRanges
    let coveredSum = countCoverage coverage

    let beaconsInRow =
        beacons
        |> List.filter (snd >> (=) line)
        |> List.length

    coveredSum - beaconsInRow

let solve2 filename =
    let sensors, _ = loadData filename

    let (y, coverage) =
        [| 1..4_000_000 |]
        |> Array.Parallel.map (fun line ->
            let coverage = lineCoverage line sensors |> compressRanges
            let (lo, hi) = rangeExtent coverage
            let coveredSum = countCoverage coverage
            let width = hi - lo + 1
            (width - coveredSum, (line, coverage)))
        |> Array.find (fst >> (=) 1)
        |> snd
        |> Util.dump "%A"

    let x = (List.head coverage |> snd) + 1
    bigint x * bigint 4_000_000 + bigint y
