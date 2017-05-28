// Learn more about F# at http://fsharp.org
// See the 'F# Tutorial' project for more help.

[<EntryPoint>]
let main argv = 
    Board.setPiecesForGame
    printfn "%A" Board.Board
    0 // return an integer exit code
