module Day11

type MonkeyNote =
    { mutable Items: uint64 list
      Op: uint64 -> uint64
      Divisor: uint64
      Targets: int * int
      mutable Inspected: uint64 }

let loadNotes filename =
    System.IO.File.ReadAllLines filename
    |> Util.blockWise ""
    |> Seq.map (fun notes ->
        let xs = Seq.toArray notes

        let items =
            let items = Sscanf.sscanf "  Starting items: %s" xs.[1]

            items.Split(", ")
            |> Array.map System.Convert.ToUInt64
            |> Array.toList

        let op =
            match Sscanf.sscanf "  Operation: new = old %c %s" xs.[2] with
            | ('*', "old") -> fun w -> w * w
            | (op, s) ->
                let d = uint64 s

                match op with
                | '+' -> fun w -> w + d
                | '*' -> fun w -> w * d
                | _ -> failwithf "invalid operation: '%s'" xs.[2]

        let divisor =
            let s = Sscanf.sscanf "  Test: divisible by %s" xs.[3]
            uint64 s

        let targets =
            let t = Sscanf.sscanf "    If true: throw to monkey %d" xs.[4]
            let f = Sscanf.sscanf "    If false: throw to monkey %d" xs.[5]
            (t, f)

        { Items = items
          Op = op
          Divisor = divisor
          Targets = targets
          Inspected = uint64 0 })
    |> Seq.toArray

let round unworry (monkeys: MonkeyNote array) =
    for monkey in monkeys do
        for item in monkey.Items do
            let worry = unworry (monkey.Op item)

            let target =
                if worry % monkey.Divisor = 0UL then
                    fst monkey.Targets
                else
                    snd monkey.Targets

            monkeys.[target].Items <- monkeys.[target].Items @ [ worry ]

        monkey.Inspected <-
            monkey.Inspected
            + uint64 (List.length monkey.Items)

        monkey.Items <- []

let multipleRounds n unworry monkeys =
    for _ in [ 1..n ] do
        round unworry monkeys

    monkeys

let monkeyBusiness =
    Array.map (fun m -> m.Inspected)
    >> Array.sortDescending
    >> Array.take 2
    >> Array.reduce (*)

let solve1 =
    loadNotes
    >> multipleRounds 20 (fun w -> w / uint64 3)
    >> monkeyBusiness

let solve2 filename =
    let notes = loadNotes filename

    let modulo =
        notes
        |> Array.fold (fun acc m -> acc * m.Divisor) 1UL

    notes
    |> multipleRounds 10000 (fun w -> w % modulo)
    |> monkeyBusiness
