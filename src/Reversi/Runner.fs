namespace FableReversi.Reversi.Runner

open FableReversi.Reversi

type GameAction =
    | SkipMove
    | PlayMove of PossibleMove

type Heuristic = OngoingGame -> float

type Description =
    {
      Text : string
      SubDescriptions: Description []
    }

type ComputerPlayer =
    {
        ChooseMove : OngoingGame -> PossibleMove
        OpponentSelected : PossibleMove -> unit
        OnMoveSkipped : unit -> unit
        Describe : unit -> Description []
    }

module Actions =
    let skipMove gameInfo =
        match gameInfo.State with
        | OngoingSkipMove b -> { b with NextToMove = b.NextToMove.Opposite }
        | _ -> failwith "Cannot skip move"
