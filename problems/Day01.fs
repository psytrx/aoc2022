module Day01

let ParseBags lines =
    seq {
        let mutable bag = []

        for line in lines do
            if line = "" then
                yield bag
                bag <- []
            else
                let c = System.Convert.ToInt32(line)
                bag <- c :: bag

        yield bag
    }

let LoadBags filename =
    System.IO.File.ReadAllLines(filename)
    |> ParseBags
    |> Seq.map List.sum

let Solve1 () = LoadBags "./input/day01.txt" |> Seq.max

let Solve2 () =
    LoadBags "./input/day01.txt"
    |> Seq.sortDescending
    |> Seq.take 3
    |> Seq.sum
