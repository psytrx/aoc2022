module Day10

type Instruction =
    | Noop
    | AddX of int

let (|Prefix|_|) (prefix: string) (s: string) =
    if s.StartsWith(prefix) then
        Some(s.Substring(prefix.Length))
    else
        None

let parseInstruction line =
    match line with
    | "noop" -> Noop
    | Prefix "addx " rest -> AddX(System.Convert.ToInt32 rest)
    | _ -> failwithf "invalid instruction '%s'" line

let numCycles instr =
    match instr with
    | Noop -> 1
    | AddX _ -> 2

let execute instructions =
    let folder (cycle, signals, x) instr =
        let nextCycle = cycle + numCycles instr

        let doSignal =
            [ cycle + 1 .. nextCycle ]
            |> List.tryFind (fun c -> (20 + c) % 40 = 0)

        let nextSignals =
            match doSignal with
            | Some cycle -> signals @ [ cycle * x ]
            | _ -> signals

        match instr with
        | Noop -> (cycle + numCycles instr, nextSignals, x)
        | AddX v -> (cycle + numCycles instr, nextSignals, x + v)

    instructions |> Array.fold folder (0, [], 1)

let solve1 filename =
    let state =
        filename
        |> System.IO.File.ReadAllLines
        |> Array.map parseInstruction
        |> execute
        |> Util.dump "%A"

    match state with
    | (_, signals, _) -> signals |> List.sum

let solve2 = System.IO.File.ReadAllLines
