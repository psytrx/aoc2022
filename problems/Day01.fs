module Day01

let parseInput lines =
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

let loadBags =
    System.IO.File.ReadAllLines
    >> parseInput
    >> (Seq.map List.sum)

let solve1 = loadBags >> Seq.max

let solve2 =
    loadBags
    >> Seq.sortDescending
    >> Seq.take 3
    >> Seq.sum
