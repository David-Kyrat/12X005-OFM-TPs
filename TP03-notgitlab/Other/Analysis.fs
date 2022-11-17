module Analysis

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
        let suite_omega (marking : Marking<'Place>) (pred : Map<Marking<'Place>, Set<Marking<'Place>>>) : Map<'Transition, Marking<'Place>> =
            Set.fold
                (fun succ transi -> Map.add transi (setOmegas (pred.[marking].Add(marking)) (Model.fire model marking transi).Value) succ)
                Map.empty
                (Model.getFireable model marking)

        let suite_o_solo (marking : Marking<'Place>) (pred : Map<Marking<'Place>, Set<Marking<'Place>>>) (transition : 'Transition) : Marking<'Place> =
            (suite_omega marking pred).[transition]

        let suite_o (marking : Marking<'Place>) (pred : Map<Marking<'Place>, Set<Marking<'Place>>>) : Set<Marking<'Place>> =
            Set.map (fun trans -> suite_o_solo marking pred trans) (Model.getFireable model marking)
            
        let mutable pred_map = Map.add marking Set.empty Map.empty

        let rec evolu markings edges (map_pred : Map<Marking<'Place>, Set<Marking<'Place>>>) =
            let mark_omega = Set.map (fun marking -> setOmegas map_pred.[marking] marking) markings

            let edges_ret = Set.fold 
                                    (fun edge2 marking -> Map.add marking (suite_omega marking map_pred) edge2) 
                                    edges 
                                    mark_omega
            
            let map_pred_loop = Map.fold 
                                    (fun map marking set -> (Set.fold 
                                                                (fun map suite -> Map.add suite (Set.add marking set) map) 
                                                                map 
                                                                (suite_o marking map_pred))) 
                                    map_pred 
                                    map_pred
             
            let visited_marking, all_marking = Map.fold
                                                    (fun (v_marking, a_marking) marking _ ->
                                                        (Set.add (setOmegas map_pred.[marking] marking) v_marking, 
                                                        Set.union (Set.ofSeq (Map.values (suite_omega marking map_pred))) a_marking))
                                                    (Set.empty, Set.empty)
                                                    edges_ret
            
            let markings_fini = Set.difference all_marking visited_marking
            
            if Set.isEmpty markings_fini then
                edges_ret
            else
                evolu markings_fini edges_ret map_pred_loop

        { Root = marking
          Edges = evolu (Set.singleton marking) Map.empty pred_map} // TODO: complete this function to return the coverability graph of a model.

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
