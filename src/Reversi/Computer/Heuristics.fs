module FableReversi.Reversi.Computer.Heuristics

open FableReversi.Reversi
open FableReversi.Reversi.Runner

let basic (ongoing: OngoingGame) =
    let numBlack = Bitboard.count ongoing.Board.BlackSquares
    let numWhite = Bitboard.count ongoing.Board.WhiteSquares
    let piecesScore = numBlack - numWhite
    let movesScore =
        ongoing.PossibleMoves.Length * if ongoing.Board.NextToMove = Black then 1 else -1

    let score x y =
        let pos = Bitboard.pos x y
        if Bitboard.isSet pos ongoing.Board.BlackSquares then 1
        elif Bitboard.isSet pos ongoing.Board.WhiteSquares then -1
        else 0

    let cornersScore = score 0 0 + score 7 0 + score 0 7 + score 7 7

    float (int piecesScore + 3 * movesScore + 10 * cornersScore)

type HeuristicChoice =
    | Basic
    member this.Name =
        match this with
        | Basic -> "Basic"

let createHeuristic heuristic : Heuristic =
    match heuristic with
    | Basic -> basic
