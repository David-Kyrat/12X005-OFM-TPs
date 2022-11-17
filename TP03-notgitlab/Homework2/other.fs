module other

open PetriNet

// ----- Utility functions ------------------------------------------------------------------------------------------ //

/// Returns a new marking containing 'ω' where necessary if the set of predecessors received as argument contains
/// another marking that is smaller than or equal to the one received as input.
let setOmegas (predecessors: Set<Marking<'Place>>) (marking: Marking<'Place>) : Marking<'Place> =
    // Try to find a smaller marking in the set of predecessors.
    let smallerPredecessor =
        predecessors
        |> Set.filter (fun predecessor -> predecessor <= marking)
        |> Set.toList
        |> List.tryHead

    match smallerPredecessor with
    // If one is found, set the token counts to ω in places where the predecessor is smaller than the input marking.
    | Some marking' ->
        marking
        |> Marking.map (fun place count ->
            if marking'[place] < count then
                Omega
            else
                count)
    | None -> marking

// ----- CoverabilityGraph ------------------------------------------------------------------------------------------ //

/// A coverability graph is represented by its root (an inital marking) and a set of edges implemented as a mapping from
/// markings to mappings from transitions to markings.
type CoverabilityGraph<'Place, 'Transition when 'Place: comparison and 'Transition: comparison> =
    { Root: Marking<'Place>
      Edges: Map<Marking<'Place>, Map<'Transition, Marking<'Place>>> }

[<RequireQualifiedAccess>]
module CoverabilityGraph =

    /// Builds the coverability graph for a model, given some initial marking.
    let make (model: Model<'Place, 'Transition>) (marking: Marking<'Place>) : CoverabilityGraph<'Place, 'Transition> =
        let edges = ref Map.empty

        let rec create_edges_map (edges: 'a ref) marking predecessor =
            printfn $"curr marking = {marking}"
            for t in Model.getFireable model marking do 
                let mark_fir_omeg = setOmegas predecessor (Model.fire model marking t).Value
                let upd_pred = Set.union predecessor <| Set.singleton mark_fir_omeg
                if not <| Map.containsKey mark_fir_omeg edges.Value then 
                    edges.Value <- edges.Value |> Map.add mark_fir_omeg ((Map.empty): Map<'Transition, Marking<'Place>>)
                    // erreur ici dans newmaptrans
            let newmaptrans = edges.Value |> Map.find marking |> Map.add t mark_fir_omeg
                    edges.Value <- edges.Value 
                    |> Map.add marking newmaptrans
                    
                    create_edges_map edges mark_fir_omeg upd_pred
        
        edges.Value <- edges.Value |> Map.add marking Map.empty

        create_edges_map edges marking Set.empty

        printf $"{edges}"
        
        {Root = marking; Edges = edges.Value} // TODO: complete this function to return the coverability graph of a model.

    /// Returns the set of all the markings in some coverability graph.
    let markings (graph: CoverabilityGraph<'Place, 'Transition>) : Set<Marking<'Place>> =
        graph.Edges
        |> Map.fold
            (fun markings _ successors -> Set.union (Set.ofSeq (Map.values successors)) markings)
            (Set.ofSeq (Map.keys graph.Edges))

    /// Returns the total number of markings in a coverability graph.
    let count (graph: CoverabilityGraph<'Place, 'Transition>) : int = markings graph |> Set.count

    /// Checks if there exists a marking in a coverability graph that satisfies some predicate.
    let exists (predicate: Marking<'Place> -> bool) (graph: CoverabilityGraph<'Place, 'Transition>) : bool =
        Set.exists predicate (markings graph)

    /// Returns the set of markings in a coverability graph that satisfy some predicate.
    let filter
        (predicate: Marking<'Place> -> bool)
        (graph: CoverabilityGraph<'Place, 'Transition>)
        : Set<Marking<'Place>> =
        Set.filter predicate (markings graph)
