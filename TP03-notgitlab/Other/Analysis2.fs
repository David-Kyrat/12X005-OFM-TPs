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
     
        //Je cherche pour un marking tous des prédecesseurs recursivement grâce à edges
        let rec predecessors (marking_init: Marking<'Place>) (edges: Map<Marking<'Place>, Map<'Transition, Marking<'Place>>>) : Set<Marking<'Place>> =
            let marking =
                edges
                |> Map.filter (fun _ successors -> successors |> Map.exists (fun _ marking' -> marking' = marking_init))//Je regarde pour chaque Marking dans egdes si il y en a qui possèdent un successeur (Map<'Transition,Marking<'Place>>) egal au marquage initial. 
                |> Map.keys //Je recupere le Marking de edge et l'extrait grâce a Keys
                |> List.ofSeq //Je le transforme en liste pour pouvoir prendre le premier
                |> List.tryHead
            match marking with
            | Some marking' -> Set.add marking' (predecessors marking' edges)//Si y'a un marquage j'ajoute au set et je cherche les predecesseurs de ce marquage recursivement
            | None -> Set.empty //Si TryHead renvoie None -> fini

        
        let successors (marking: Marking<'Place>)(prede: Map<Marking<'Place>, Set<Marking<'Place>>>): Map<'Transition, Marking<'Place>> =
            Model.getFireable model marking
            |> Set.fold
                (fun successors transition -> Map.add transition (setOmegas (prede.[marking].Add(marking)) (Model.fire model marking transition).Value) successors)
                Map.empty

        //Le set de predecessor de depart que je vais actualiser au fur et a mesure
        
        let prede = Map.add marking Set.empty Map.empty

        
        let rec fixpoint (markings:Set<Marking<'Place>>) (edges: Map<Marking<'Place>, Map<'Transition, Marking<'Place>>>) (prede: Map<Marking<'Place>, Set<Marking<'Place>>>) =
            
            //J'applique la fonction d'accumulation sur markings avec comme etat initial edges pour le mettre a jour et avoir a la fin edges'. 
            //Je passe successors qui provient aussi du Tp2 que j'ai modifié pour prendre en compte les omegas.
            //Elle va retourner les values les values a associer (.Add) a chaque key (marking) de updates_edges
            let edges' = Set.fold (fun updated_edges marking -> Map.add marking (successors marking prede) updated_edges) edges markings 
            
            
            //Jai pas touché cette fonction qui provient du Tp2 elle nous permettra de savoir si il reste des edges a explorer ou non                                           
            let visitedMarkings, allMarkings =
                edges'
                |> Map.fold
                    (fun (visitedMarkings, allMarkings) marking successors ->
                        (Set.add marking visitedMarkings, Set.union (Set.ofSeq (Map.values successors)) allMarkings))
                    (Set.empty, Set.empty)


            //Si ya pas de diff entre allMarkings et ceux qu'on a deja visité (visitedMarking) alors on a tout parcouru et finito
            let Status =
                Set.difference allMarkings visitedMarkings                

            
            if Status = Set.empty then
                //Edges est donc egal a edges' et on a fini
                edges'

            else

                //On met a jour nos predecesseurs. Assez similaire a edges' sauf qu'on va utiliser prede et Status comme state et set.
                //Predecessors nous donne les values a associer (.Add) a chaque key (marking) de updates_prede
                let prede' = Set.fold(fun updated_prede mark -> Map.add mark (predecessors mark edges' ) updated_prede) prede Status      
               //On rappel fixpoint avec edges' et prede' mis a jour
                fixpoint Status edges' prede'

        //J'ai juste rajoute prede comme parametre a fixpoint sinon identique a Tp2
        { Root = marking
          Edges = fixpoint (Set.singleton marking) Map.empty prede }       


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
