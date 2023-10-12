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

let findRange = notImpl

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
        test <@ findRange 50 90 getVal = 11 @>
    testCase "Placeholder for property testing AutoFight.calibrate" <| fun () ->
        // not sure if this is really necessary
        property {
            notImpl "Given a list of times... what property should be true?"
            let! xs = Range.constant 0 1000 |> Gen.int32 |> Gen.list (Range.linear 0 100)
            return List.rev (List.rev xs) = xs
        }
        |> Property.checkBool
    ]
