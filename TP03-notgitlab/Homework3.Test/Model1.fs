module Model1
open Analysis
open PetriNet

// ----- Test Model 1 ----------------------------------------------------------------------------------------------- //

type Place =
    | P0
    | P1

type Transition =
    | T0
    | T1
    | T2

let pre =
    Arcs.make [ ((P0, T0), 2)
                ((P0, T2), 2)
                ((P1, T1), 6) ]

let post =
    Arcs.make [ ((P0, T1), 1)
                ((P0, T2), 1)
                ((P1, T0), 1)
                ((P1, T1), 6)
                ((P1, T2), 4) ]

let model = Model.make pre post

let initialMarking =
    Marking.make [ (P0, 3); (P1, 2) ]

let mg = CoverabilityGraph.make model initialMarking

