module Homework.Program2

open PetriNet
open other
// ----- Model definition ------------------------------------------------------------------------------------------- //

type Place =
    | P0
    | P1

type Transition =
    | T0
    | T1

let pre =
    Arcs.make [ ((P0, T0), 1)
                ((P1, T1), 1) ]

let post =
    Arcs.make [ ((P0, T1), 2)
                ((P1, T0), 1)
                ((P1, T1), 2) ]

let model = Model.make pre post

let initialMarking =
    Marking.make [ (P0, 1) ]


// ----- Coverability graph construction ---------------------------------------------------------------------------- //

let graph = CoverabilityGraph.make model initialMarking

// ----- Coverability graph representation -------------------------------------------------------------------------- //


/// Print the coverabilityGraph
let printGraph graph =
    printfn "Coverability graph for the model defined in `Program.fs`:"
    for edge in graph.Edges do
        printfn $"{edge.Key}"

        for succ in edge.Value do
            printfn $"\t-- {succ.Key} --> {succ.Value}"

(* let printPathList markingToSearch (pathList: list<Marking<'Place>>) =
     List.toSeq pathList
     |> Seq.map (fun el ->  sprintf " -- %s\n" $"{el}")
     |> Seq.iter (fun el -> printfn $"{el}")
     printfn " -- %s\n" $"{markingToSearch}" *)
     

printGraph graph
printfn "------------------------------------------\n"




