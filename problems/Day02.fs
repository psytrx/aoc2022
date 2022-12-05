module Day02

type Shape =
    | Rock
    | Paper
    | Scissors

type RoundResult =
    | Win
    | Draw
    | Loss

let GetRoundResult me opp =
    match (me, opp) with
    | (Rock, Scissors) -> Win
    | (Scissors, Paper) -> Win
    | (Paper, Rock) -> Win
    | _ when me = opp -> Draw
    | _ -> Loss

let GetShapeResponse s r =
    match (s, r) with
    | (_, Draw) -> s
    | (Rock, Win) -> Paper
    | (Paper, Win) -> Scissors
    | (Scissors, Win) -> Rock
    | (Rock, Loss) -> Scissors
    | (Paper, Loss) -> Rock
    | (Scissors, Loss) -> Paper

let ParseShape s =
    match s with
    | "A"
    | "X" -> Rock
    | "B"
    | "Y" -> Paper
    | "C"
    | "Z" -> Scissors
    | _ -> failwith "invalid shape"


let ParseRoundResult s =
    match s with
    | "X" -> Loss
    | "Y" -> Draw
    | "Z" -> Win
    | _ -> failwith "invalid shape"

let GetRoundPoints shape result =
    let resultPoints =
        match result with
        | Win -> 6
        | Draw -> 3
        | Loss -> 0

    let shapePoints =
        match shape with
        | Rock -> 1
        | Paper -> 2
        | Scissors -> 3

    resultPoints + shapePoints

let SumPointsForStrategy strategy (data: string []) =
    data
    |> Array.map (fun line ->
        let (opp, me) =
            match line.Split(" ") with
            | [| opp; me |] -> (opp, me)
            | _ -> failwith "invalid input format"

        let (shapeMe, result) = strategy opp me
        GetRoundPoints shapeMe result)
    |> Array.sum

let Solve1 =
    let strat opp me =
        let shapeOpp = ParseShape opp
        let shapeMe = ParseShape me
        let roundRes = GetRoundResult shapeMe shapeOpp
        (shapeMe, roundRes)

    System.IO.File.ReadAllLines
    >> SumPointsForStrategy strat

let Solve2 =
    let strat opp res =
        let shapeOpp = ParseShape opp
        let roundRes = ParseRoundResult res
        let shapeMe = GetShapeResponse shapeOpp roundRes
        (shapeMe, roundRes)

    System.IO.File.ReadAllLines
    >> SumPointsForStrategy strat
