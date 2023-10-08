#load @"c:\code\Arena\src\Core\Common.fs"
#load @"c:\code\Arena\src\Core\CQRS.fs"
#load @"c:\code\Arena\src\Core\Coroutine.fs"
#load @"c:\code\Arena\src\Core\Packrat.fs"
#load @"c:\code\Arena\src\Domain\Random.fs"
#load @"c:\code\Arena\src\Domain\Data.fs"
#load @"c:\code\Arena\src\Domain\Geo2d.fs"
#load @"c:\code\Arena\src\Domain\Behavior.fs"
#load @"c:\code\Arena\src\Domain\CombatRules.fs"
open Coroutine
open Domain
open Domain.Random
open Domain.Behavior
open Domain.CombatRules
open Domain.Geo
module Test =
        let teamOf m coords =
            [{  members = m
                center = coords
                radius = Some 0.<yards>
                }]
        // we don't care who's in the combat as long as there's someone on both sides and they're in range of each other
        let combatAt pos = createCombat (["Bob", Creature.create "Bob"] |> Map.ofList) (teamOf [1, "Bob"] (0.<yards>, 0.<yards>)) (teamOf [1, "Bob"] pos) |> fun c -> c.combat

        let bob = (1, "Bob")
        let toCtx combat = { me = bob; combat = combat }

        let (AwaitingAction(action, bhv)) = run justAttack ((), toCtx (combatAt (yards 0., yards 5.)))
        action
        let (AwaitingAction(action, bhv)) = run bhv ((), toCtx (combatAt (yards 0., yards 1.)))
        action // this should be Attack but is Move the first time we run it! Repros the issue!

let bhv : ActionBehavior = behavior {
    printfn "Starting bhv" // should not print immediately
    return ()
    }
let b = behavior in
    b.Run(b.Delay(fun () -> b.Return ()))
