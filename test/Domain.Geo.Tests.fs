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
    testCase "Spot-check Geo lines" <| fun () ->
        let g = Geo.ofList [
            (1, "Bob"), (0.<yards>, 0.<yards>)
            (2, "Bob"), (0.<yards>, 15.<yards>)
            (3, "Bob"), (12.<yards>, 0.<yards>)
            (4, "Bob"), (9.<yards>, 9.<yards>)
            ]
        let l = g.LineFrom (g.Find (2, "Bob"), g.Find (3, "Bob"))
        verify <@ l.Length = (sqrt (12*12 + 15*15 |> float) * 1.<yards>) @>
        let nearBob3 = l.Extend (l.Length - yards 1.)
        verify <@ g.DistanceBetween (nearBob3, (3, "Bob")) < yards 1.01 @>
        ()
    ]
