module Day02

type Shape =
    | Rock
    | Paper
    | Scissors

let parseShape s =
    match s with
    | "A"
    | "X" -> Rock
    | "B"
    | "Y" -> Paper
    | "C"
    | "Z" -> Scissors
    | _ -> failwith "invalid shape"

type RoundResult =
    | Win
    | Draw
    | Loss

let roundResult me opp =
    match (me, opp) with
    | (Rock, Scissors) -> Win
    | (Scissors, Paper) -> Win
    | (Paper, Rock) -> Win
    | _ when me = opp -> Draw
    | _ -> Loss

let parseRoundResult s =
    match s with
    | "X" -> Loss
    | "Y" -> Draw
    | "Z" -> Win
    | _ -> failwith "invalid shape"

let shapeResponse s r =
    match (s, r) with
    | (_, Draw) -> s
    | (Rock, Win) -> Paper
    | (Paper, Win) -> Scissors
    | (Scissors, Win) -> Rock
    | (Rock, Loss) -> Scissors
    | (Paper, Loss) -> Rock
    | (Scissors, Loss) -> Paper

let roundPoints shape result =
    match result with
    | Win -> 6
    | Draw -> 3
    | Loss -> 0
    + match shape with
      | Rock -> 1
      | Paper -> 2
      | Scissors -> 3

let sumPointsWithStrat strat (data: string []) =
    data
    |> Array.map (fun line ->
        let (opp, me) =
            match line.Split(" ") with
            | [| opp; me |] -> (opp, me)
            | _ -> failwith "invalid input format"

        let (shapeMe, result) = strat opp me
        roundPoints shapeMe result)
    |> Array.sum

let solve1 =
    let strat opp me =
        let shapeOpp = parseShape opp
        let shapeMe = parseShape me
        let roundRes = roundResult shapeMe shapeOpp
        (shapeMe, roundRes)

    System.IO.File.ReadAllLines
    >> sumPointsWithStrat strat

let solve2 =
    let strat opp res =
        let shapeOpp = parseShape opp
        let roundRes = parseRoundResult res
        let shapeMe = shapeResponse shapeOpp roundRes
        (shapeMe, roundRes)

    System.IO.File.ReadAllLines
    >> sumPointsWithStrat strat
