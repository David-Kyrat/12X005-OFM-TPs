module PetriNet

open Microsoft.FSharp.Reflection

// ----- Utilities -------------------------------------------------------------------------------------------------- //

/// Returns the set of all the cases in a discriminated union.
let private getAllCases<'T when 'T: comparison> () =
    if not (FSharpType.IsUnion typeof<'T>) then
        failwith "The types for places and transitions must be discriminated unions."

    FSharpType.GetUnionCases typeof<'T>
    |> Seq.map (fun case -> FSharpValue.MakeUnion(case, [||]) :?> 'T)
    |> Set.ofSeq

// ----- ExtendedInt ------------------------------------------------------------------------------------------------ //

/// This type represents the set of integers extended with ω.
type ExtendedInt =
    | Int of int
    | Omega

    /// Adds two integers extended with ω.
    static member (+)(lhs: ExtendedInt, rhs: ExtendedInt) : ExtendedInt =
        match lhs, rhs with
        | Int i, Int j -> Int(i + j)
        | _, _ -> Omega

    /// Subtracts two integers extended with ω.
    static member (-)(lhs: ExtendedInt, rhs: ExtendedInt) : ExtendedInt =
        match lhs, rhs with
        | Int i, Int j -> Int(i - j)
        | _, _ -> Omega

    /// Returns a string representation for an integer extended with ω.
    override this.ToString() : string =
        System.Console.OutputEncoding <- System.Text.Encoding.Unicode

        match this with
        | Int i -> $"{i}"
        | Omega -> "ω"

// ----- Token ------------------------------------------------------------------------------------------------------ //

/// Tokens in a Petri net are simply represented by integer counts (extended with ω).
type Token = ExtendedInt

// ----- Marking ---------------------------------------------------------------------------------------------------- //

/// A marking is a mapping from places to token counts.
type Marking<'Place when 'Place: comparison> =
    private
        { Counts: Map<'Place, Token> }

    /// Checks if a marking is included in another one (i.e. if its token counts for each place are smaller than or
    /// equal to those in the other marking).
    static member op_LessThanOrEqual(lhs: Marking<'Place>, rhs: Marking<'Place>) : bool =
        Map.forall (fun place tokens -> lhs[place] <= tokens) rhs.Counts

    /// Returns the token count associated with a given place in a marking.
    member this.Item(place: 'Place) : Token = Map.find place this.Counts

    /// Returns a string representation for a marking.
    override this.ToString() : string =
        let repr =
            this.Counts
            |> Map.fold (fun str place tokens -> str + $"{place}: {tokens}, ") "{"

        repr[0 .. repr.Length - 3] + "}"

[<RequireQualifiedAccess>]
module Marking =

    /// Returns a new marking built from a sequence of mappings from places to integer token counts.
    let make (mappings: seq<'Place * int>) : Marking<'Place> =
        // A marking is a total map, meaning that for every place in a Petri net, there is a token count in the marking.
        // Every place that isn't initialised with a specific value is given a default token count of zero.
        let totalMap =
            getAllCases<'Place> ()
            |> Set.fold (fun zeroMap place -> Map.add place (Int 0) zeroMap) Map.empty

        let counts =
            mappings
            |> Seq.fold (fun countMap (place, count) -> Map.add place (Int count) countMap) totalMap

        { Counts = counts }

    /// Returns a new marking where the token count associated with a given place has been updated to a new value.
    let where (place: 'Place) (count: Token) (marking: Marking<'Place>) : Marking<'Place> =
        { Counts = Map.add place count marking.Counts }

    /// Builds a new marking whose values are the result of applying the given function to each place and token count
    /// in some marking.
    let map (mapping: 'Place -> Token -> Token) (marking: Marking<'Place>) : Marking<'Place> =
        { Counts = Map.map mapping marking.Counts }

    /// Checks if a marking contains some specific token count in one of its places.
    let contains (value: Token) (marking: Marking<'Place>) : bool =
        marking.Counts
        |> Map.exists (fun _ count -> count = value)

// ----- Arcs ------------------------------------------------------------------------------------------------------- //

/// The pre- and post-condition functions of a Petri net are represented by sets of weighted arcs from places to
/// transitions. They are implemented as mappings from pairs of places and transitions to token counts.
type Arcs<'Place, 'Transition when 'Place: comparison and 'Transition: comparison> =
    private
        { Values: Map<'Place * 'Transition, Token> }

    /// Returns the token count associated with a pair of nodes in a set of arcs.
    member this.Item(arc: 'Place * 'Transition) : Token =
        match Map.tryFind arc this.Values with
        | Some count -> count
        | None -> Int 0

[<RequireQualifiedAccess>]
module Arcs =

    /// Returns a new set of arcs built from a sequence of mappings from pairs of nodes to token counts.
    let make (mappings: seq<('Place * 'Transition) * int>) : Arcs<'Place, 'Transition> =
        let weights =
            mappings
            |> Seq.map (fun ((place, transition), count) -> ((place, transition), Int count))

        { Values = Map(weights) }

// ----- Model ------------------------------------------------------------------------------------------------------ //

/// A Petri net is represented by the types of its places and transitions, as well as its pre- and post-condition
/// functions.
type Model<'Place, 'Transition when 'Place: comparison and 'Transition: comparison> =
    private
        { Places: Set<'Place>
          Transitions: Set<'Transition>
          Pre: Arcs<'Place, 'Transition>
          Post: Arcs<'Place, 'Transition> }

[<RequireQualifiedAccess>]
module Model =

    /// Returns a new model built from pre- and post-condition functions expressed as sets of arcs.
    let make (pre: Arcs<'Place, 'Transition>) (post: Arcs<'Place, 'Transition>) : Model<'Place, 'Transition> =
        { Places = getAllCases<'Place> ()
          Transitions = getAllCases<'Transition> ()
          Pre = pre
          Post = post }

    /// Returns the set of places in a model.
    let places (model: Model<'Place, 'Transition>) : Set<'Place> = model.Places

    /// Returns the set of transitions in a model.
    let transitions (model: Model<'Place, 'Transition>) : Set<'Transition> = model.Transitions

    /// Checks if a transition is fireable from a given marking in a model.
    let isFireable (model: Model<'Place, 'Transition>) (marking: Marking<'Place>) (transition: 'Transition) : bool =
        model.Places
        |> Set.forall (fun place ->
            marking[place]
            - model.Pre[place, transition]
            >= Int 0)

    /// Returns the set of all the transitions that are fireable from a given marking in a model.
    let getFireable (model: Model<'Place, 'Transition>) (marking: Marking<'Place>) : Set<'Transition> =
        model.Transitions
        |> Set.filter (isFireable model marking)

    /// Fires a transition from a given marking in a model. Returns some new marking if the transition is fireable, or
    /// none otherwise.
    let fire
        (model: Model<'Place, 'Transition>)
        (marking: Marking<'Place>)
        (transition: 'Transition)
        : Option<Marking<'Place>> =
        if not (isFireable model marking transition) then
            None
        else
            model.Places
            |> Set.fold
                (fun newMarking place ->
                    let newCount =
                        marking[place]
                        - model.Pre[place, transition]
                        + model.Post[place, transition]

                    Marking.where place newCount newMarking)
                marking
            |> Some
