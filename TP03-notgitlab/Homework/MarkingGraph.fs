module MarkingGraph

(* ======================================================================= 
   ---------- Implementation of MarkingGraph from Homework 2 -------------
   ======================================================================= *)
   
open PetriNet

// ----- MarkingGraph ----------------------------------------------------------------------------------------------- //

/// The marking graph of a Petri net is represented by its root (a marking) and a set of edges implemented as a mapping
/// from markings to mappings from transitions to markings.
type MarkingGraph<'Place, 'Transition when 'Place: comparison and 'Transition: comparison> =
    { Root: Marking<'Place>
      Edges: Map<Marking<'Place>, Map<'Transition, Marking<'Place>>> }

[<RequireQualifiedAccess>]
module MarkingGraph =

    /// Builds the marking graph for a model, given some initial marking as its root.
    let make (model: Model<'Place, 'Transition>) (marking: Marking<'Place>) : MarkingGraph<'Place, 'Transition> =
        let successors marking =
            Model.getFireable model marking
            |> Set.fold
                (fun successors transition -> Map.add transition (Model.fire model marking transition).Value successors)
                Map.empty

        let rec fixpoint markings edges =
            let edges' =
                markings
                |> Set.fold (fun newEdges marking -> Map.add marking (successors marking) newEdges) edges

            let visitedMarkings, allMarkings =
                edges'
                |> Map.fold
                    (fun (visitedMarkings, allMarkings) marking successors ->
                        (Set.add marking visitedMarkings, Set.union (Set.ofSeq (Map.values successors)) allMarkings))
                    (Set.empty, Set.empty)

            let markings' =
                Set.difference allMarkings visitedMarkings

            if Set.isEmpty markings' then
                edges'
            else
                fixpoint markings' edges'

        { Root = marking
          Edges = fixpoint (Set.singleton marking) Map.empty }

    /// Returns the set of all the nodes (markings) in some marking graph.
    let markings (graph: MarkingGraph<'Place, 'Transition>) : Set<Marking<'Place>> =
        graph.Edges
        |> Map.fold
            (fun markings _ successors -> Set.union (Set.ofSeq (Map.values successors)) markings)
            (Set.ofSeq (Map.keys graph.Edges))

    /// Returns the total number of nodes in a marking graph.
    let count (graph: MarkingGraph<'Place, 'Transition>) : int = markings graph |> Set.count

    /// Checks if there exists a node (marking) in a marking graph that satisfies some predicate.
    let exists (predicate: Marking<'Place> -> bool) (graph: MarkingGraph<'Place, 'Transition>) : bool =
        Set.exists predicate (markings graph)

    let filter (predicate: Marking<'Place> -> bool) (graph: MarkingGraph<'Place, 'Transition>) : Set<Marking<'Place>> =
        Set.filter predicate (markings graph)

// ----- Analysis functions ----------------------------------------------------------------------------------------- //

/// Checks if a transition is quasi-alive in a given model for some initial marking.
let isQuasiAlive (model: Model<'Place, 'Transition>) (marking: Marking<'Place>) (transition: 'Transition) : bool =
    (*(marking |> MarkingGraph.make model).Edges
             |> Map.exists (fun key value -> value |> Map.exists (fun trans mark -> trans == transition))*)
    let edges : Map<Marking<'Place>, Map<'Transition, Marking<'Place>>> = (marking |> MarkingGraph.make model).Edges
    edges |> Map.exists(fun _ map ->
                            map |> Map.exists (fun tr _ -> tr.Equals transition))
    //* t is quasi alive For any marking in the graph there exists an edge that is labeled as 'transition'
    //* i.e. there is a key of the Map  Map<'Transition, Marking<'Place>> which is 'transition'
    
/// Returns the set of markings that are deadlocks in a given model when transitions are fired starting from some
/// initial marking.
let deadlocks (model: Model<'Place, 'Transition>) (marking: Marking<'Place>) : Set<Marking<'Place>> =
     marking |> MarkingGraph.make model
             |> MarkingGraph.markings
             |> Set.filter (fun m -> (Model.getFireable model m).IsEmpty)
     
