module FableReversi.Reversi.Computer.FewestReplies

open FableReversi.Reversi
open FableReversi.Reversi.Runner

let numberPossibleMoves board =
    match (Board.toGameInfo board).State with
    | Ongoing og -> og.PossibleMoves.Length
    | _ -> 0

let create() =
    let random = new System.Random()
    {
        ChooseMove = fun ongoingGame ->
            let movesWithFewestReplies =
                ongoingGame.PossibleMoves
                |> List.groupBy (fun pm -> pm.Result |> numberPossibleMoves)
                |> List.maxBy fst
                |> snd
            let choice = random.Next(0, movesWithFewestReplies.Length)
            movesWithFewestReplies.Item choice
    }