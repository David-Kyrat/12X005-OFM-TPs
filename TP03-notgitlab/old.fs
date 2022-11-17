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
    (*let make (model: Model<'Place, 'Transition>) (marking: Marking<'Place>) : CoverabilityGraph<'Place, 'Transition> =
        {Root = marking; Edges = Map.empty} // TODO: complete this function to return the coverability graph of a model.*)
    /// Builds the marking graph for a model, given some initial marking as its root.
    let make (model: Model<'Place, 'Transition>) (marking: Marking<'Place>) : CoverabilityGraph<'Place, 'Transition> =
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

    /// Returns the edges (Marking -> (Transition -> Marking)) who's marking on the "right side" (i.e. the values of the Map<'Transition, Marking<'Place>>)
    /// satisfies the given predicate. (the Markings on the "left" side are the predecessors and on the "right side" the successors) 
    (*let filterRight (predicate: 'Transition -> Marking<'Place> -> bool) (edges: Map<Marking<'Place>, Map<'Transition, Marking<'Place>>>) =
        edges |> Map.map (fun k v -> v |> Map.filter (predicate))
              |> Map.filter (fun _ v -> not (Map.isEmpty v))
    
    ///Search for the path going from 'root' to marking 's' (in graph defined by edges 'edges' for root 'root')
    let searchPath (s: Marking<'Place>) (root: Marking<'Place>) (edges: Map<Marking<'Place>, Map<'Transition, Marking<'Place>>>) : list<Marking<'Place>> =
        
        let rec searchPathRec (s: Marking<'Place>) (path: list<Marking<'Place>>) : list<Marking<'Place>> =
            match edges
                   // searches for edges (predecessor -> (transition -> successor)) where successor = s (i.e. searched node)
                  |> filterRight (fun _ marking -> marking = s)  
                  |> Seq.head // take the first result
            with 
                // if we reached the end i.e. the top (because we search "bottom-up") => returns the list (path) of predecessors
                | KeyValue (mi, _) when mi = root -> mi :: path
                // if not store the predecessors and continue the search for the predecessor of 's'
                | KeyValue (mi, _) -> (mi :: path) |> searchPathRec mi
            
        List.empty |> searchPathRec s*)
    
    /// Returns the set of markings in a coverability graph that satisfy some predicate.
    let filter
        (predicate: Marking<'Place> -> bool)
        (graph: CoverabilityGraph<'Place, 'Transition>)
        : Set<Marking<'Place>> =
        Set.filter predicate (markings graph)
