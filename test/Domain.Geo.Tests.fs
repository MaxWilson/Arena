module Domain.goes.Tests
open Expecto
open Domain
open Domain.Random
open Domain.Behavior
open Domain.CombatRules
open Domain.Geo
let verify = Swensen.Unquote.Assertions.test
let shouldFail = Swensen.Unquote.Assertions.raises

[<Tests>]
let Tests = testLabel "Unit" <| testList "Geo" [
    testCase "Spot-check coords to index conversions" <| fun () ->
        verify <@ Geo.placeOf (coords(0., 0.)) = (0, 0) @>
        verify <@ Geo.placeOf (coords(1., 0.)) = (2, 0) @>
        verify <@ Geo.placeOf (coords(1., 3.5)) = (2, 7) @>
        verify <@ Geo.placeOf (coords(-1., -1)) = (-2, -2) @>
        verify <@ Geo.placeOf (coords(0, -1)) = (0, -2) @>
        verify <@ Geo.placeOf (coords(0, -0.5)) = (0, -1) @>
    testCase "Spot-check Geo lines" <| fun () ->
        let g = Geo.ofList [
            (1, "Bob"), (0.<yards>, 0.<yards>)
            (2, "Bob"), (0.<yards>, 15.<yards>)
            (3, "Bob"), (12.<yards>, 0.<yards>)
            (4, "Bob"), (9.<yards>, 9.<yards>)
            ]
        let l = g.LineFrom (g.Find (2, "Bob"), g.Find (3, "Bob"))
        verify <@ l.Length = (sqrt (4.5*4.5 + 15.*15.) * 1.<yards>) @> // NOT Euclidean distance! In hex distance half of the 15 goes towards reducing the 12
        let nearBob3 = l.Extend (l.Length - yards 1.)
        verify <@ g.WithinDistance (nearBob3, g.Find(3, "Bob"), yards 1.0) @>
    testCase "Check that Approach wants to move to hex-closest spaces, not cardinal directions" <| fun () ->
        let g = Geo.ofList [
            (1, "Bob"), (2.<yards>, 5.<yards>)
            (2, "Bob"), (2.<yards>, 4.<yards>)
            (3, "Bob"), (2.5<yards>, 0.<yards>)
            ]
        let getCoords = Option.get >> Tuple3.get1
        verify <@ g.TryApproach((3, "Bob"), Person (1, "Bob"), 5) |> getCoords = (3.0<yards>, 4.5<yards>) @>
        ()
    ]
