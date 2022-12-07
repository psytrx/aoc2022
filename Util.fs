module Util

let bundleWise size (list: 'a []) =
    let rec aux i acc =
        if i >= list.Length then
            acc
        else
            aux (i + size) (acc @ [ list.[i .. i + size - 1] ])

    aux 0 []

let blockWise sep list =
    seq {
        let mutable acc = []

        for item in list do
            if item = sep then
                yield acc
                acc <- []
            else
                acc <- acc @ [ item ]

        yield acc
    }
