// Learn more about F# at http://fsharp.org
// See the 'F# Tutorial' project for more help.

[<EntryPoint>]
let main argv = 
    let board = Board.initialGameBoard in
    //let blacks = Board.getPiecesOfGivenColor (Pieces.Black, board)
    //let moves = Moves.getMovesForPiece ((1, Board.WhitePawnStartY), board)
    //Printf.printfn "Moves possible for Pawn at (1, 2): %A" moves
    //Printf.printfn "Black pieces coords: %A" blacks
    let movesForWhites = Moves.getMovesForColor (Pieces.White, board) in
    movesForWhites |> List.iter (Printf.printfn "%O")
    let movesForBlacks = Moves.getMovesForColor (Pieces.Black, board) in
    movesForBlacks |> List.iter (Printf.printfn "%O")
    0 // return an integer exit code
