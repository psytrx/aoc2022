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
    let rec parseItems (s: string) =
        if s = "" then
            []
        else if s.StartsWith(",") then
            parseItems (s.Substring(1))
        else if s.StartsWith("[") then
            let closing = findClosing s
            let inner = s.Substring(1, closing - 1)
            let rest = s.Substring(closing + 1)
            List(parseItems inner) :: parseItems rest
        else
            let comma = s.IndexOf(",")

            if comma < 0 then
                [ Int(int s) ]
            else
                let next = s.Substring(0, comma)
                let rest = s.Substring(comma + 1)
                Int(int next) :: parseItems (rest)

    List(parseItems (s.Substring(1, s.Length - 2)))

type Indeterminate =
    | Yes
    | No
    | Unknown

let isInOrder pair =
    let rec aux pair =
        match pair with
        | (Int left, Int right) when left < right -> Yes
        | (Int left, Int right) when left > right -> No
        | (Int left, Int right) when left = right -> Unknown
        | (List [], List []) -> Unknown
        | (List [], List (_ :: _)) -> Yes
        | (List (_ :: _), List []) -> No
        | (List (lHead :: lRest), List (rHead :: rRest)) ->
            match aux (lHead, rHead) with
            | Unknown -> aux (List lRest, List rRest)
            | yesOrNo -> yesOrNo
        | (Int left, List right) -> aux (List [ Int left ], List right)
        | (List left, Int right) -> aux (List left, List [ Int right ])
        | _ -> failwithf "invalid pair: %A" pair

    aux pair

let solve1 filename =
    System.IO.File.ReadAllLines(filename)
    |> Util.blockWise ""
    |> Seq.map (fun pair ->
        match pair with
        | [ left; right ] -> parsePacket left, parsePacket right
        | _ -> failwith "invalid packet pair")
    |> Util.dump "%A"
    |> Seq.mapi (fun i pair -> (i + 1, pair))
    |> Seq.filter ((snd >> isInOrder) >> (=) Yes)
    |> Seq.sumBy fst

let solve2 filename = -1
