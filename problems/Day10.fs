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

let flattenInstructions instructions =
    let rec aux acc instructions =
        match instructions with
        | [] -> acc
        | Noop :: rest -> aux (acc @ [ Noop ]) rest
        | AddX v :: rest -> aux (acc @ [ Noop; AddX v ]) rest

    aux [] instructions

let execute instructions =
    let folder (cycle, signals, x, crt) instr =
        let nextSignals =
            if (20 + cycle) % 40 = 0 then
                signals @ [ cycle * x ]
            else
                signals

        let idx = (cycle - 1) % 40
        let onSprite = idx >= x - 1 && idx <= x + 1
        let pixel = if onSprite then "#" else "."

        let nextCrt = crt + pixel + if cycle % 40 = 0 then "\n" else ""

        match instr with
        | Noop -> (cycle + 1, nextSignals, x, nextCrt)
        | AddX v -> (cycle + 1, nextSignals, x + v, nextCrt)

    instructions |> List.fold folder (1, [], 1, "\n")

let runProgram =
    System.IO.File.ReadAllLines
    >> Array.map parseInstruction
    >> Array.toList
    >> flattenInstructions
    >> execute

let solve1 filename =
    match runProgram filename with
    | (_, signals, _, _) -> signals |> List.sum

let solve2 filename =
    match runProgram filename with
    | (_, _, _, crt) -> crt
