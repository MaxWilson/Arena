module UI.Logic.Tests
open Common
open Expecto
open Packrat
open Hedgehog
open Domain
open Domain.Parser
open Domain.Random
open Domain.Random.Parser
open Swensen.Unquote

let findRange bound1 bound2 evaluate =
    let lower, upper = min bound1 bound2, max bound1 bound2
    let (|TooLow|JustRight|TooHigh|) n =
        let v = evaluate n
        if v > upper then TooHigh // if victory percentage is too high, we need to increase n
        elif v < lower then TooLow // if victory percentage is too low, we need to decrease n
        else JustRight
    // first, look for a bound on the *upper* bound
    let rec step1 n =
        match n with
        | TooLow | JustRight -> step1 (n * 2)
        | TooHigh ->
            let range = (1, n)
            notImpl "now we just do a binary search on [1, n] for both upper bound and lower bound"
    step1 1
[<Tests>]
let Tests() = (testLabel "Unit") <| testList "UI" [
    testCase "Spot-check AutoFight.calibrate using mocked evaluation" <| fun () ->
        let vals = [|
            for n in 0..100 do
                if n < 11 then 100
                elif n <= 23 then 90
                elif n = 24 then 100
                elif n < 27 then 60
                elif n <= 30 then 50
                elif n <= 35 then 40
                elif n = 36 || n = 38 then 50
                else 0
            |]
        let getVal n = vals.[n]
        test <@ findRange 50 90 getVal = (11, 30) @>
    testCase "Placeholder for property testing AutoFight.calibrate" <| fun () ->
        // not sure if this is really necessary
        property {
            notImpl "Given a list of times... what property should be true?"
            let! xs = Range.constant 0 1000 |> Gen.int32 |> Gen.list (Range.linear 0 100)
            return List.rev (List.rev xs) = xs
        }
        |> Property.checkBool
    ]
