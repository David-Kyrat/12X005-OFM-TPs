open PetriNet
open Analysis
open MarkingGraph


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

(*let peek set =
    set
    |> Set.map (fun x ->
        printfn $"{x}\n"
        x)*)

let printGraph (graph: CoverabilityGraph<'a, 'b>) =
    printfn "Coverability graph for the model defined in `Program.fs`:"
    for edge in graph.Edges do
        printfn $"{edge.Key}"

        for succ in edge.Value do
            printfn $"\t-- {succ.Key} --> {succ.Value}"
 
let printPathList markingToSearch (pathList: list<Marking<'Place>>) =
     List.toSeq pathList
     |> Seq.map (fun el ->  sprintf " -- %s\n" $"{el}")
     |> Seq.iter (fun el -> printfn $"{el}")
     printfn " -- %s\n" $"{markingToSearch}"
            
let peek mapping = mapping |> Map.iter (fun k v -> printfn "{%s, %s}\n" $"{k}" $"{v}")
let mg = CoverabilityGraph.make model initialMarking

let testm0: Marking<Place> = Marking.make [(P0, 1); (P1, 1)]

//MarkingGraph.markings mg |> peek
//mg.Edges |> peek

printGraph mg
printfn "------------------------------------------------\n\n"

//let path = mg.Edges |> CoverabilityGraph.searchPath testm0 initialMarking
//printPathList testm0 path


//mg.Edges |> filterRight (fun _ mark -> mark = m) |> peek

