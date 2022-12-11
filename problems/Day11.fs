module Day11

type MonkeyNote =
    { mutable Items: bigint list
      Op: bigint -> bigint
      Test: bigint -> bool
      Targets: int * int
      mutable Inspected: bigint }

let loadNotes filename =
    System.IO.File.ReadAllLines filename
    |> Util.blockWise ""
    |> Seq.map (fun notes ->
        let xs = Seq.toArray notes

        let items =
            match xs.[1] with
            | Util.Prefix "  Starting items: " rest ->
                rest.Split(", ")
                |> Array.map bigint.Parse
                |> Array.toList
            | _ -> failwithf "invalid starting items: %s" xs.[1]

        let op =
            match Sscanf.sscanf "  Operation: new = old %c %s" xs.[2] with
            | ('*', "old") -> fun w -> w * w
            | (op, s) ->
                let d = bigint.Parse s

                match op with
                | '+' -> fun w -> w + d
                | '*' -> fun w -> w * d
                | _ -> failwithf "invalid operation: '%s'" xs.[2]

        let test =
            match Sscanf.sscanf "  Test: divisible by %s" xs.[3] with
            | s -> (fun w -> w % (bigint.Parse s) = bigint.Zero)

        let targets =
            let t = Sscanf.sscanf "    If true: throw to monkey %d" xs.[4]
            let f = Sscanf.sscanf "    If false: throw to monkey %d" xs.[5]
            (t, f)

        { Items = items
          Op = op
          Test = test
          Targets = targets
          Inspected = bigint 0 })
    |> Seq.toArray

let round unworry (monkeys: MonkeyNote array) =
    for monkey in monkeys do
        for item in monkey.Items do
            let worry = unworry (monkey.Op item)

            let target =
                if monkey.Test worry then
                    fst monkey.Targets
                else
                    snd monkey.Targets

            monkeys.[target].Items <- monkeys.[target].Items @ [ worry ]

        monkey.Inspected <-
            monkey.Inspected
            + bigint (List.length monkey.Items)

        monkey.Items <- []

let multipleRounds n unworry monkeys =
    for _ in [ 1..n ] do
        round unworry monkeys

    monkeys

let solve rounds unworry =
    loadNotes
    >> multipleRounds rounds unworry
    >> Array.map (fun m -> m.Inspected)
    >> Array.sortDescending
    >> Array.take 2
    >> Array.reduce (*)

let solve1 = solve 20 (fun w -> w / bigint 3)

let solve2 filename =
    let modulo = bigint ([ 3; 13; 19; 17; 5; 7; 11; 2 ] |> List.reduce (*))
    filename |> solve 10000 (fun w -> w % modulo)
