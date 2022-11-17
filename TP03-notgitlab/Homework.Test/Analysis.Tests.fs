module Analysis.Tests

open PetriNet
open Xunit
open FsUnit.Xunit

// ----- Test coverability graphs ----------------------------------------------------------------------------------- //

let graph1 =
    CoverabilityGraph.make Model1.model Model1.initialMarking

let graph2 =
    CoverabilityGraph.make Model2.model Model2.initialMarking

let graph3 =
    CoverabilityGraph.make Model3.model Model3.initialMarking

// ----- Tests ------------------------------------------------------------------------------------------------------ //

[<Fact>]
let ``The test model 1 should have a coverability graph containing 9 markings`` () =
    graph1
    |> CoverabilityGraph.count
    |> should equal 9

[<Fact>]
let ``The test model 1 should contain some specific markings`` () =
    graph1
    |> CoverabilityGraph.exists (fun marking ->
        marking = (Marking.where Model1.P0 Omega (Marking.make [ (Model1.P1, 6) ])))
    |> should be True

    graph1
    |> CoverabilityGraph.exists (fun marking ->
        marking = Marking.make [ (Model1.P0, 0)
                                 (Model1.P1, 7) ])
    |> should be True

[<Fact>]
let ``The test model 2 should have a coverability graph containing 6 markings`` () =
    graph2
    |> CoverabilityGraph.count
    |> should equal 6

[<Fact>]
let ``The test model 2 should contain some specific markings`` () =
    let testMarking1 =
        Marking.where
            Model2.P0
            Omega
            (Marking.make [ (Model2.P1, 0)
                            (Model2.P2, 3) ])

    let testMarking2 =
        Marking.where Model2.P1 Omega testMarking1

    graph2
    |> CoverabilityGraph.exists (fun marking -> marking = testMarking1)
    |> should be True

    graph2
    |> CoverabilityGraph.exists (fun marking -> marking = testMarking2)
    |> should be True

[<Fact>]
let ``The test model 3 should not contain any markings with ω`` () =
    graph3
    |> CoverabilityGraph.exists (fun marking -> Marking.contains Omega marking)
    |> should be False

[<Fact>]
let ``You get a free point ! And you get a free point ! Everybody gets a free point !`` () = true |> should be True
