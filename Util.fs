module Util

let bundleWise n (xs: 'a []) =
    let rec aux i acc =
        if i >= xs.Length then
            acc
        else
            aux (i + n) (acc @ [ xs.[i .. i + n - 1] ])

    aux 0 []