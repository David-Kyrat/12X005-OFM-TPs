module Analysis

open System.Collections.Generic
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

/// Add given key to given dict if it doesn't contain it
let addKIfNex (el:Marking<'Place>) (dict:Dictionary<Marking<'Place>, HashSet<Marking<'Place>>>) =
    if (not (dict.ContainsKey el)) then
        dict.Add (el, new HashSet<Marking<'Place>>()) 

/// Returns the edges (Marking -> (Transition -> Marking)) who's marking on the "right side" (i.e. the values of the Map<'Transition, Marking<'Place>>)
/// satisfies the given predicate for at least one (Transition->Marking) pair. (the Markings on the "left" side are the predecessors and on the "right side" the successors)
let filterRight (predicate: 'Transition -> Marking<'Place> -> bool) (edges: Map<Marking<'Place>, Map<'Transition, Marking<'Place>>>) =
    edges
        |> Map.filter (fun pred succMap -> succMap
                                           |> Map.exists predicate )

[<RequireQualifiedAccess>]
module CoverabilityGraph =

     /// Builds the marking graph for a model, given some initial marking as its root.
    let make (model: Model<'Place, 'Transition>) (marking: Marking<'Place>) : CoverabilityGraph<'Place, 'Transition> =
         
        ///Bottom up approach of searching predecessors by searching a path (a sequence of marking&transition) from 's' to the top (until there's no more predecessors of 's')
        let rec searchPath (s: Marking<'Place>) (edges: Map<Marking<'Place>, Map<'Transition, Marking<'Place>>>) =
            match edges
                    // searches for edges (predecessor -> (transition -> successor)) where successor = s (i.e. searched node)
                    |> filterRight (fun succTrans succMark -> succMark = s)
                    |> Seq.tryHead
            with
               | Some (KeyValue (crtPred, succMap)) -> (searchPath crtPred edges) |> Set.add crtPred 
               | None -> Set.empty                                      
        
        
        let predMap = new Dictionary<Marking<'Place>, HashSet<Marking<'Place>>>()
        predMap |> addKIfNex marking // initialize map of predecessors with the root mapped to the empty set  
        
        let successors (crtMark: Marking<'Place>) =
            Model.getFireable model crtMark
            |> Set.fold
                (fun successors transition ->
                    let newM = (Model.fire model crtMark transition).Value
                    predMap |> addKIfNex newM
                    predMap[newM].Add crtMark |> ignore //we dont care if the addition returned false
                    // if we have an edge (crtMark, newM) then crtMark will be added to the predecessors (pred) set of newM (easy)
                    // now we have to add the predecessors of crtMark to the preds of newM
                    predMap[newM].UnionWith (predMap[crtMark])
                    successors |> Map.add transition (setOmegas (predMap[newM] |> Set.ofSeq) newM))
                Map.empty
        
        /// Since we changed our markings by replacing when necessary with an omega => we need to update predecessor Map.
        /// i.e. What this function does. (predMap is mutable map of (Marking -> mutable set of predecessors of said marking)
        /// (mutable map i.e. Dictionary, mutable set i.e. Hashset)
        let updatePredMapWithOmegas (markings': Set<Marking<'Place>>) edges' =
            markings' |> Seq.iter (fun markKey -> 
                                                  predMap |> addKIfNex markKey //add markKey to predMap if it doesnt exist
                                                  predMap[markKey].UnionWith (searchPath markKey edges'))
            
        let rec fixpoint markings edges =
            let edges' = markings
                            |> Set.fold (fun newEdges marking -> Map.add marking (successors marking) newEdges) edges

            let visitedMarkings, allMarkings =
                edges'
                |> Map.fold
                    (fun (visitedMarkings, allMarkings) marking successors ->
                        (Set.add marking visitedMarkings, Set.union (Set.ofSeq (Map.values successors)) allMarkings))
                    (Set.empty, Set.empty)

            match Set.difference allMarkings visitedMarkings
            with
                | markings' when markings'.IsEmpty -> edges'
                | markings' ->
                               edges' |> updatePredMapWithOmegas markings'
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

    /// Returns the set of markings in a coverability graph that satisfy some predicate.
    let filter
        (predicate: Marking<'Place> -> bool)
        (graph: CoverabilityGraph<'Place, 'Transition>)
        : Set<Marking<'Place>> =
        Set.filter predicate (markings graph)
