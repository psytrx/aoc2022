module Sscanf

// sscanf taken from http://www.fssnip.net/4I/title/sscanf-parsing-with-format-strings

let private check f x =
    if f x then
        x
    else
        failwithf "format failure \"%s\"" x

let private parseDecimal (x: string) =
    System.Decimal.Parse(x, System.Globalization.CultureInfo.InvariantCulture)

let private parsers =
    dict [ 'b', string >> System.Boolean.Parse >> box
           'd', int >> box
           'i', int >> box
           's', box
           'u', uint32 >> box
           'x',
           check (String.forall System.Char.IsLower)
           >> ((+) "0x")
           >> int
           >> box
           'X',
           check (String.forall System.Char.IsUpper)
           >> ((+) "0x")
           >> int
           >> box
           'o', ((+) "0o") >> int >> box
           'e', float >> box // no check for correct format for floats
           'E', float >> box
           'f', float >> box
           'F', float >> box
           'g', float >> box
           'G', float >> box
           'M', parseDecimal >> box
           'c', char >> box ]

// array of all possible formatters, i.e. [|"%b"; "%d"; ...|]
let private separators =
    parsers.Keys
    |> Seq.map (fun c -> "%" + sprintf "%c" c)
    |> Seq.toArray

// Creates a list of formatter characters from a format string,
// for example "(%s,%d)" -> ['s', 'd']
let rec private getFormatters xs =
    match xs with
    | '%' :: '%' :: xr -> getFormatters xr
    | '%' :: x :: xr ->
        if parsers.ContainsKey x then
            x :: getFormatters xr
        else
            failwithf "Unknown formatter %%%c" x
    | x :: xr -> getFormatters xr
    | [] -> []

let sscanf (pf: PrintfFormat<_, _, _, _, 't>) s : 't =
    let formatStr = pf.Value.Replace("%%", "%")
    let constants = formatStr.Split(separators, System.StringSplitOptions.None)

    let regex =
        System.Text.RegularExpressions.Regex(
            "^"
            + System.String.Join(
                "(.*?)",
                constants
                |> Array.map System.Text.RegularExpressions.Regex.Escape
            )
            + "$"
        )

    let formatters =
        pf.Value.ToCharArray() // need original string here (possibly with "%%"s)
        |> Array.toList
        |> getFormatters

    let groups =
        regex.Match(s).Groups
        |> Seq.cast<System.Text.RegularExpressions.Group>
        |> Seq.skip 1

    let matches =
        (groups, formatters)
        ||> Seq.map2 (fun g f -> g.Value |> parsers.[f])
        |> Seq.toArray

    if matches.Length = 1 then
        matches.[0] :?> 't
    else
        Reflection.FSharpValue.MakeTuple(matches, typeof<'t>) :?> 't
