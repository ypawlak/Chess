module Board
open System.Collections.Generic

let CharsToNums = [
    'A', 1
    'B', 2
    'C', 3
    'D', 4
    'E', 5
    'F', 6
    'G', 7
    'H', 8
    ]

let numsByChars = dict CharsToNums

let NumericValue c = 
    match numsByChars.TryGetValue(c) with
    | true, v -> v
    | _ -> failwith (sprintf "letter '%c' was not in lookup table" c)

let NumsToChars = [
    1, 'A'
    2, 'B'
    3, 'C'
    4, 'D'
    5, 'E'
    6, 'F'
    7, 'G'
    8, 'H'
    ]

let charsByNums = dict NumsToChars

let CharValue d = 
    match charsByNums.TryGetValue(d) with
    | true, v -> v
    | _ -> failwith (sprintf "digit '%d' was not in lookup table" d)

let A = NumericValue 'A'
let B = NumericValue 'B'
let C = NumericValue 'C'
let D = NumericValue 'D'
let E = NumericValue 'E'
let F = NumericValue 'F'
let G = NumericValue 'G'
let H = NumericValue 'H'

let WhiteFiguresStartY = 1
let WhitePawnStartY = 2
let BlackPawnStartY = 7
let BlackFiguresStartY = 8

let getPrettyCoordsPrint (x, y) =
    sprintf "%c%d" (CharValue x) y

let isOccupied (square, board: Dictionary<int*int, Pieces.Piece option>) =
    match board.[square] with
    | Some piece -> true
    | None -> false

let isOccupiedByPieceOfGivenColor (square, color: Pieces.PieceColor, board: Dictionary<int*int, Pieces.Piece option>) =
    match board.[square] with
    | Some { Pieces.Piece.Color = color; Pieces.Piece.Rank = _; } -> true
    | _ -> false

let rec getFreeSquareXCoord (yCoord, colsToCheck, board: Dictionary<int*int, Pieces.Piece option>) =
    match colsToCheck with
    | [] -> invalidOp "All given squares are already occupied."
    | head::tail when isOccupied ((head, yCoord), board ) -> getFreeSquareXCoord (yCoord, tail, board)
    | head::tail -> head

let getInitialXIndex (pRank, yCoord, board: Dictionary<int*int, Pieces.Piece option>) =
    let possibleColumns =
        match pRank with
        | Pieces.Pawn -> [A..H]
        | Pieces.Rook -> [A; H]
        | Pieces.Knigth -> [B; G]
        | Pieces.Bishop -> [C; F]
        | Pieces.Queen -> [D]
        | Pieces.King -> [E]
    in
        getFreeSquareXCoord(yCoord, possibleColumns, board)

let getInitialYIndex piece =
    match piece with 
    | { Pieces.Piece.Color = Pieces.PieceColor.White; Pieces.Piece.Rank = Pieces.Pawn }
        -> WhitePawnStartY
    | { Pieces.Piece.Color = Pieces.PieceColor.White }
        -> WhiteFiguresStartY
    | { Pieces.Piece.Color = Pieces.PieceColor.Black; Pieces.Piece.Rank = Pieces.Pawn }
        -> BlackPawnStartY
    | { Pieces.Piece.Color = Pieces.PieceColor.Black }
        -> BlackFiguresStartY

let putOnInitialPosition (piece, board: Dictionary<int*int, Pieces.Piece option>)  = 
    let y = getInitialYIndex piece in
    let square = getInitialXIndex (piece.Rank, y, board), y in
    board.[square] <- Some piece
    Printf.printfn "Putting piece %A on %A" piece square

let setPiecesForGame (board: Dictionary<int*int, Pieces.Piece option>) = 
    Printf.printfn "%A" (Pieces.Whites @ Pieces.Blacks)
    Pieces.Whites @ Pieces.Blacks
    |> List.iter (fun piece -> putOnInitialPosition (piece, board)) 
    board

let initialGameBoard =
    let board = new Dictionary<int*int, Pieces.Piece option>() in
    for x in A..H do
        for y in 1..8 do
            let square = x, y in 
            board.[square] <- None
    Printf.printfn "%A" board
    setPiecesForGame board

let getPiecesOfGivenColor (color, board: Dictionary<int*int, Pieces.Piece option>) =
    board 
    |> Seq.filter (fun (KeyValue(coords, piece)) -> piece.IsSome && piece.Value.Color = color)
    |> Seq.map (fun (KeyValue(coords, piece)) -> coords)
    |> Seq.toList
