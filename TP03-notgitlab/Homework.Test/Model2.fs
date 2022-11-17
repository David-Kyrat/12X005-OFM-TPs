module Model2

open PetriNet

// ----- Test Model 2 ----------------------------------------------------------------------------------------------- //

type Place =
    | P0
    | P1
    | P2

type Transition =
    | T0
    | T1
    | T2

let pre =
    Arcs.make [ ((P0, T0), 2)
                ((P1, T1), 1)
                ((P2, T2), 1) ]

let post =
    Arcs.make [ ((P0, T2), 1)
                ((P1, T0), 1)
                ((P2, T1), 3)
                ((P2, T2), 1) ]

let model = Model.make pre post

let initialMarking =
    Marking.make [ (P0, 3) ]
