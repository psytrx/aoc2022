module Day04

let ParseRange (s: string) =
    match s.Split("-") with
    | [| l; u |] ->
        let lower = System.Convert.ToInt32 l
        let upper = System.Convert.ToInt32 u
        (lower, upper)
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


let Solve1 =
    System.IO.File.ReadAllLines
    >> Array.map ParsePair
    >> Array.filter (fun (a, b) -> (Contains a b) || (Contains b a))
    >> Array.length

let Solve2 =
    System.IO.File.ReadAllLines
    >> Array.map ParsePair
    >> Array.filter (fun (a, b) -> (Overlaps a b) || (Overlaps b a))
    >> Array.length
