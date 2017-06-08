module Utils

open System.Collections.Generic

// Converts seq of key - value pairs to mutable Dictionary
let ofSeq (src:seq<'a * 'b>) = 
    let d = new Dictionary<'a, 'b>()
    for (k,v) in src do
        d.Add(k,v)
    d

// get a seq of key-value pairs for easy iteration with for (k,v) in d do...
let pairs (d:Dictionary<'a, 'b>) =
    seq {
        for kv in d do
            yield (kv.Key, kv.Value)
    }

let add (x, y) (x2, y2) =
    (x + x2, y + y2)

let mul (x, y) (x2, y2) =
    (x * x2, y * y2)

let clone (d:Dictionary<'a, 'b>) =
    new Dictionary<'a, 'b>(d)

let absRange x y =
    if x <= y then
        [x..y]
    else [x .. -1 .. y]