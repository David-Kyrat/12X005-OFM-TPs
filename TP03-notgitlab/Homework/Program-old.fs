open PetriNet
open Analysis

// ----- Model definition ------------------------------------------------------------------------------------------- //

type Place =
    | P0
    | P1

type Transition =
    | T0
    | T1

let pre =
    Arcs.make [ ((P0, T0), 2)
                ((P1, T1), 1) ]

let post =
    Arcs.make [ ((P0, T1), 1); ((P1, T1), 1) ]
    (*Arcs.make [ ((P0, T1), 2)
                ((P1, T0), 1)
                ((P1, T1), 2) ]*)

let model = Model.make pre post

let initialMarking =
    Marking.make [(P0, 6); (P1, 1)] //[ (P0, 1) ]

// ----- Coverability graph construction ---------------------------------------------------------------------------- //
open MarkingGraph

(*let graph =
    CoverabilityGraph.make model initialMarking*)
let graph = MarkingGraph.make model initialMarking

// ----- Coverability graph representation -------------------------------------------------------------------------- //

printfn "Coverability graph for the model defined in `Program.fs`:"

for edge in graph.Edges do
    printfn $"{edge.Key}"

    for succ in edge.Value do
        printfn $"\t-- {succ.Key} --> {succ.Value}"
