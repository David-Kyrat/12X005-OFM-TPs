module Model3

open PetriNet

// ----- Test Model 3 ----------------------------------------------------------------------------------------------- //

type Place =
    | P1
    | P2

type Transition =
    | T1
    | T2

let pre =
    Arcs.make [ ((P1, T1), 1)
                ((P2, T2), 1) ]

let post = Arcs.make [ ((P2, T1), 1) ]

let model = Model.make pre post

let initialMarking =
    Marking.make [ (P1, 3) ]
