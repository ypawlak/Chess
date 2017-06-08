module Interactive

open System.Collections.Generic
let dict = new Dictionary<string, string>()

dict.Add("1501", "Zara Ali")
dict.Add("1502","Rishita Gupta")
dict.Add("1503","Robin Sahoo")
dict.Add("1504","Gillian Megan")

dict.Item("1504") <- "test"

let coords = new Dictionary<int*int, string>()
coords.Item((1,2)) <- "test"
coords.[(1,2)] <- "test change"
coords
coords.ContainsKey((1,2))

let clone (d:Dictionary<'a, 'b>) =
    new Dictionary<'a, 'b>(d)

let secondCoords = clone coords

let aSquare = (1,2)
let moveVec = (-1, 1)


let moved = Utils.mul aSquare moveVec

let l1 = [1;2;3]
let l2 = [1]
List.zip l1 l2

let add (x, y) (x2, y2) =
    (x + x2, y + y2)

let mul (x, y) (x2, y2) =
    (x * x2, y * y2)

let absRange x y =
    if x <= y then
        [x..y]
    else [x .. -1 .. y]

let getPassedSquaresForFlatMove sourceSq destSq =
    let xSeq, ySeq = 
        match sourceSq, destSq with
        | (x1, y1), (x2, y2) when x1 = x2 -> List.init (abs (y2 - y1) + 1) (fun y -> x1), absRange y1 y2
        | (x1, y1), (x2, y2) when y1 = y2 -> absRange x1 x2, List.init (abs (x2 - x1) + 1) (fun x -> y1)
        | (sx, sy), (dx, dy) -> absRange sx dx, absRange sy dy
    in
    let squaresSeq = List.zip xSeq ySeq
    squaresSeq.Tail

let getSeqs sourceSq destSq =
        match sourceSq, destSq with
        | (x1, y1), (x2, y2) when x1 = x2 -> List.init (abs (y2 - y1) + 1) (fun y -> x1), absRange y1 y2
        | (x1, y1), (x2, y2) when y1 = y2 -> absRange x1 x2, List.init (abs (x2 - x1) + 1) (fun x -> y1)
        | (sx, sy), (dx, dy) -> absRange sx dx, absRange sy dy

let seqs = getSeqs (1,1) (8,1)
let passed = getPassedSquaresForFlatMove (8,1) (4,5)