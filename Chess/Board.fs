module Board
open System.Collections.Generic

let A = 1
let B = 2
let C = 3
let D = 4
let E = 5
let F = 6
let G = 7
let H = 8

let WhiteFiguresStartY = 1
let WhitePawnStartY = 2
let BlackPawnStartY = 7
let BlackFiguresStartY = 8

let isOccupied (square, board: Dictionary<int*int, Pieces.Piece option>) =
    match board.[square] with
    | Some piece -> true
    | None -> false

let isOccupiedByPieceOfGivenColor (square, color: Pieces.PieceColor, board: Dictionary<int*int, Pieces.Piece option>) =
    match board.[square] with
    | Some { Pieces.Piece.Color = color; Pieces.Piece.Rank = _; } -> true
    | None -> false

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

