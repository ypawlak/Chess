module Pieces

type PieceRank = 
    | King
    | Queen
    | Rook 
    | Bishop
    | Knigth
    | Pawn

type PieceColor = 
    | Black
    | White

let ranksSet = [Pawn; Knigth; Bishop; Rook; Queen; King]

let pieceRankCount pieceRank =
    match pieceRank with
    | Pawn -> 8
    | Rook | Knigth | Bishop -> 2
    | King | Queen -> 1

let getPiecesOfGivenRank pieceRank = 
    List.init (pieceRankCount pieceRank) (fun index -> pieceRank)

let initialPiecesRanksList = List.concat (ranksSet |> List.map getPiecesOfGivenRank)

type Piece =
    {Rank:PieceRank; Color:PieceColor}
    override this.ToString() = sprintf "%A %A" this.Color this.Rank

let getInitialPiecesForGivenColor color =
    initialPiecesRanksList |> List.map (fun x -> {Rank=x; Color=color})

let Whites = getInitialPiecesForGivenColor White
let Blacks = getInitialPiecesForGivenColor Black

let negate color = 
    match color with
    | White -> Black
    | Black -> White