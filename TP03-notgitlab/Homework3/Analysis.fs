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
             
        ///Cette fonction permet d'obtenir les successeurs d'un marquage. La fonction setOmegas permet de les obtenir avec omega placés aux points endroits.
        let successors (marking:Marking<'Place>)(predecessors:Map<Marking<'Place>,Set<Marking<'Place>>>): Map<'Transition,Marking<'Place>> =
            //En utilisant getFireable nous obtenons la liste des transitions tirables depuis un certain marking.
            Model.getFireable model marking
            |> Set.fold
            // Ici Fire permet d'obtenir le successeur et l'appel à setOmegas permet de remplacer le successeur par le successeur avec
            // oméga placé aux bons endroits si besoin.
                (fun successors transition ->
                                                let newM = (Model.fire model marking transition).Value
                                                let newPred = predecessors[marking].Add(marking)
                                                successors |>
                                                Map.add transition (newM |> setOmegas newPred))
                //On tire les transitions pour obtenir les successeurs potentiels d'un marquage.
                Map.empty

                
        ///Cette fonction permet d'obtenir le successeur d'un marquage après le tir d'une transition spécifique.
        let single_successors (marking: Marking<'Place>) (predecessors: Map<Marking<'Place>,Set<Marking<'Place>>>) (transition: 'Transition): Marking<'Place>=
            (successors marking predecessors).[transition]
    
    
        /// Cette fonction permet d'obtenir l'ensemble des successeurs d'un certain marquage.
        let all_successors (marking: Marking<'Place>) (predecessors: Map<Marking<'Place>, Set<Marking<'Place>>>): Set<Marking<'Place>> =
            Set.map (fun transition -> single_successors marking predecessors transition) (Model.getFireable model marking)
        
        
        ///On crée un map qui enregistre pour chaque marquage son ensemble de prédecesseurs.
        /// Le map est initialisé avec l'ensemble des prédecesseurs de la racine (ie. l'ensemble vide).
        /// 
        let mutable predecessorsMapGlob=Map.add marking Set.empty Map.empty
        
        ///En m'inspirant de l'implémentation de MarkingGraph du tp2, je crée la fonction récursive qui me permet de créer le graphe de couverture.
        
        let rec fixpoint markings edges (predecessorsMap: Map<Marking<'Place>, Set<Marking<'Place>>>)=
            
            /// On applique la fonction setOmegas aux marquages reçus en entrée.
            let omega_markings = Set.map (fun marking -> setOmegas predecessorsMap[marking] marking) markings
            
            ///On crée les arêtes.
            let edges' = omega_markings
                        |> Set.fold (fun newEdges marking -> Map.add
                                                                marking
                                                                (successors marking predecessorsMap)
                                                                newEdges) edges
            
                     
            ///Pour chaque successeur de chaque marquage, on crée l'ensemble de ses prédecesseurs et un map entre cet ensemble et le successeur.
            /// Puisque les prédecesseurs d'un marquage sont les éléments de la racine au marquage considéré, les prédecesseurs du successeur d'un marquage
            /// sont les prédecesseurs du marquage plus le marquage lui-même.
            let predecessorsMap' = predecessorsMap
                                      |> Map.fold (fun map marking set -> ((all_successors marking predecessorsMap)
                                                                                        |> Set.fold (fun map2 successor ->
                                                                                            Map.add successor (Set.add marking set) map2) map)) predecessorsMap
                                      
            ///On crée l'ensemble des marquages visités et l'ensemble de tous les marquages à visiter.
            let visitedMarkings, allMarkings =
                edges'
                |> Map.fold
                    (fun (visitedMarkings, allMarkings) marking _ ->
                        (Set.add (setOmegas predecessorsMap.[marking] marking) visitedMarkings,
                         Set.union (Set.ofSeq (Map.values (successors marking predecessorsMap))) allMarkings))
                    (Set.empty, Set.empty)
                    
            ///On récupère l'ensemble des marquages encore à visiter en faisant la différence entre les marquages visités et l'ensemble de tous les marquages.
            let markings' = Set.difference allMarkings visitedMarkings
            
            ///Si l'ensemble des marquages à visiter est vide, on a fini et on retourne l'ensemble des arcs. Sinon, on appelle la fonction sur l'ensemble des marquages.
            if Set.isEmpty markings' then
                edges'
            else
                fixpoint markings' edges' predecessorsMap'

        { Root = marking
          Edges = fixpoint (Set.singleton marking) Map.empty predecessorsMapGlob}
   
    
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
    let filter (predicate: Marking<'Place> -> bool) (graph: CoverabilityGraph<'Place, 'Transition>): Set<Marking<'Place>> =
        Set.filter predicate (markings graph)
