module Day04

let ParseRange (s: string) =
    match s.Split("-") with
    | [| l; u |] -> (System.Convert.ToInt32 l, System.Convert.ToInt32 u)
    | _ -> failwith "invalid range format"

let ParsePair (s: string) =
    match s.Split(",") with
    | [| l; r |] -> (ParseRange l, ParseRange r)
    | _ -> failwith "invalid pair format"

let Contains inside outside =
    match (inside, outside) with
    | ((la, ua), (lb, ub)) -> la >= lb && la <= ub && ua >= lb && ua <= ub

let Overlaps a b =
    match (a, b) with
    | (la, ua), (lb, ub) -> ua >= lb && ua < ub || la >= lb && la <= ub

let Solve predicate =
    System.IO.File.ReadAllLines
    >> Array.map ParsePair
    >> Array.filter (fun (a, b) -> (predicate a b) || (predicate b a))
    >> Array.length

let Solve1 = Solve Contains
let Solve2 = Solve Overlaps
