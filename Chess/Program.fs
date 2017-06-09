// Learn more about F# at http://fsharp.org
// See the 'F# Tutorial' project for more help.

open Moves

let printGameState board =
    Printf.printfn "Chessboard:\n"
    Board.print board
    Printf.printfn "\nWhite moves:\n"
    let movesForWhites = Moves.getMovesForColor (Pieces.White, board) in
    movesForWhites |> List.iter (Printf.printfn "%O")
    Printf.printfn "\nBlack moves:\n"
    let movesForBlacks = Moves.getMovesForColor (Pieces.Black, board) in
    movesForBlacks |> List.iter (Printf.printfn "%O")
    Printf.printfn ""

[<EntryPoint>]
let main argv = 
    let board = Board.initialGameBoard in

    printGameState board
    let testMove =
        {
            StartSquare = (5,2);
            DestinationSquare = (5,3);
            Type = Moves.PawnSimple;
            Moved = board.[(5,2)].Value;
        }
    performMove (testMove, board)
    printGameState board
    let testMove2 =
        {
            StartSquare = (Board.B,8);
            DestinationSquare = (Board.A,6);
            Type = Moves.Knight;
            Moved = board.[(Board.B,8)].Value;
        }
    performMove (testMove2, board)
    printGameState board
    let testMove3 =
        {
            StartSquare = (Board.F,1);
            DestinationSquare = (Board.A,6);
            Type = Moves.Knight;
            Moved = board.[(Board.F,1)].Value;
        }
    performMove (testMove3, board)
    printGameState board
    0 // return an integer exit code
