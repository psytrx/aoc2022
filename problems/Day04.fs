module Day04

let parseRange (s: string) =
    match s.Split("-") with
    | [| l; u |] -> (System.Convert.ToInt32 l, System.Convert.ToInt32 u)
    | _ -> failwith "invalid range format"

let parsePair (s: string) =
    match s.Split(",") with
    | [| l; r |] -> (parseRange l, parseRange r)
    | _ -> failwith "invalid pair format"

let contains inside outside =
    match (inside, outside) with
    | ((la, ua), (lb, ub)) -> la >= lb && la <= ub && ua >= lb && ua <= ub

let overlaps a b =
    match (a, b) with
    | (la, ua), (lb, ub) -> ua >= lb && ua < ub || la >= lb && la <= ub

let solve predicate =
    System.IO.File.ReadAllLines
    >> Array.map parsePair
    >> Array.filter (fun (a, b) -> (predicate a b) || (predicate b a))
    >> Array.length

let solve1 = solve contains
let solve2 = solve overlaps
