module FableReversi.TestComputer.TestMCTS

open Expecto
open FableReversi.Reversi.Computer
open FableReversi.TestComputer.Series

let allTests =
    testList "MCTS player" [
        testCase "random vs. MCTS" (fun _ ->
            let series =
                playSeries
                    Players.Random
                    (Players.MCTS 25)
                    1

            ()
        )

        testCase "BasicHeuristic depth 2 vs. MCTS 25" (fun _ ->
            let series =
                playSeries
                    (Players.Minimax (Heuristics.Basic, 2))
                    (Players.MCTS 25)
                    5

            ()
        )

        ftestCase "BasicHeuristic depth 3 vs. MCTS 200" (fun _ ->
            let series =
                playSeries
                    (Players.Minimax (Heuristics.Basic, 3))
                    (Players.MCTS 200)
                    5

            ()
        )

        testCase "MCTS 200 vs. MCTS 25" (fun _ ->
            let series =
                playSeries
                    (Players.MCTS 200)
                    (Players.MCTS 25)
                    5

            ()
        )
    ]