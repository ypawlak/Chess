module Board

let Board : Pieces.Piece option [,] = Array2D.zeroCreate 8 8

let A = 0
let B = 1
let C = 2
let D = 3
let E = 4
let F = 5
let G = 6
let H = 7


let rec getFreeSquareCoords row colsToCheck =
    match colsToCheck with
    | [] -> invalidOp "All given squares are already occupied."
    | H::T when Board.[row, H].IsNone -> H
    | H::T when Board.[row, H].IsSome -> getFreeSquareCoords row T
    | _ -> failwith "Unexpected error: wrong program flow."

let getColumnStartIndex pRank row =
    let possibleColumns =
        match pRank with
        | Pieces.Pawn -> [A..H]
        | Pieces.Rook -> [A; H]
        | Pieces.Knigth -> [B; G]
        | Pieces.Bishop -> [C; F]
        | Pieces.Queen -> [D]
        | Pieces.King -> [E]
    in
        getFreeSquareCoords row possibleColumns

let getRowStartIndex piece =
    match piece with 
    | { Pieces.Piece.Color = Pieces.PieceColor.White; Pieces.Piece.Rank = Pieces.Pawn }
        -> 6
    | { Pieces.Piece.Color = Pieces.PieceColor.White }
        -> 7
    | { Pieces.Piece.Color = Pieces.PieceColor.Black; Pieces.Piece.Rank = Pieces.Pawn }
        -> 1
    | { Pieces.Piece.Color = Pieces.PieceColor.Black }
        -> 0

let getStartPosition piece = 
    let rowInd = getRowStartIndex piece
    (rowInd, getColumnStartIndex piece.Rank rowInd)

let putPieceOnBoard piece (row, col) = 
    Board.[row, col] <- Some piece;
    //printfn "Putting %A piece at coords %A" piece (row, col)

let setPiecesForGame = 
    Pieces.Whites |> List.iter (fun x -> putPieceOnBoard x (getStartPosition x));
    Pieces.Blacks |> List.iter (fun x -> putPieceOnBoard x (getStartPosition x));


