module Day13

type DataItem =
    | Int of int
    | List of DataItem list

let findClosing s =
    let rec aux i count s =
        match s with
        | Util.Prefix "[" rest -> aux (i + 1) (count + 1) rest
        | Util.Prefix "]" rest ->
            if count = 1 then
                i
            else
                aux (i + 1) (count - 1) rest
        | s -> aux (i + 1) count (s.Substring(1))

    aux 0 0 s

let parsePacket (s: string) =
    let rec aux (s: string) =
        printfn "%s" s

        if s = "" || s = "[]" then
            []
        else if s.StartsWith(",") then
            aux (s.Substring(1))
        else if s.StartsWith("[") then
            let closing = findClosing s
            let inner = s.Substring(1, closing - 1)
            let rest = s.Substring(closing + 1)
            List(aux inner) :: aux rest
        else
            let comma = s.IndexOf(",")

            if comma < 0 then
                [ Int(int s) ]
            else
                let next = s.Substring(0, comma)
                let rest = s.Substring(comma + 1)
                Int(int next) :: aux rest

    List.head (aux s)


let solve1 filename =
    System.IO.File.ReadAllLines(filename)
    |> Util.blockWise ""
    |> Seq.map (fun pair ->
        match pair with
        | [ left; right ] -> parsePacket left, parsePacket right
        | _ -> failwith "invalid packet pair")
    |> Util.dump "%A"

let solve2 filename = -1
