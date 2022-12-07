module Day04

let parseLine s =
    let la, ua, lb, ub = Sscanf.sscanf "%i-%i,%i-%i" s
    (la, ua), (lb, ub)

let contains inside outside =
    match (inside, outside) with
    | ((la, ua), (lb, ub)) -> la >= lb && la <= ub && ua >= lb && ua <= ub

let overlaps a b =
    match (a, b) with
    | (la, ua), (lb, ub) -> ua >= lb && ua <= ub || la >= lb && la <= ub

let solve predicate =
    System.IO.File.ReadAllLines
    >> Array.map parseLine
    >> Array.filter (fun (a, b) -> (predicate a b) || (predicate b a))
    >> Array.length

let solve1 = solve contains
let solve2 = solve overlaps
