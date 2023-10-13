module UI.Logic.Tests
open Common
open Expecto
open Packrat
open Domain
open Domain.Parser
open Domain.Random
open Domain.Random.Parser
open UI.Components.AutoFight
open Swensen.Unquote

[<Tests>]
let Tests() = (testLabel "Unit") <| testList "UI" [
    testCase "Spot-check AutoFight calibrate using mocked evaluation" <| fun () ->
        let vals = [
            for n in 0..100 do
                if n < 11 then 100
                elif n <= 23 then 90
                elif n = 24 then 100
                elif n < 27 then 60
                elif n <= 30 then 50
                elif n <= 35 then 40
                elif n = 36 || n = 38 then 50
                else 0
            ]
        let getVal (vals: _ list) n = vals.[n]
        let eval vals n = async {
            let v = getVal vals n
            return
                if v > 90 then TooLow // if victory percentage is too high, we need to increase n
                elif v < 50 then TooHigh // if victory percentage is too low, we need to decrease n
                else JustRight
            }
        test <@ (findRange (eval vals) None |> Async.RunSynchronously) = (11, 30) @>
        test <@ (findRange (eval vals) (Some 20) |> Async.RunSynchronously) = (11, 20) @>
        test <@ (findRange (eval vals) (Some 5) |> Async.RunSynchronously) = (5, 5) @>
        test <@ (findRange (eval [100; 100; 80; 70; 30; 0]) None |> Async.RunSynchronously) = (2, 3) @>

    ]
