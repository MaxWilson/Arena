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

let justAttack : ActionBehavior = behavior {
    printfn "Started justAttack"
    let rec loop targetId = behavior {
        printfn "Just re-entered justAttack loop"
        let! target = query(fun ctx ->
            match targetId with
            | Some targetId ->
                let target = ctx.combat.combatants[targetId]
                if target.isnt [Unconscious; Dead] then Some target
                else tryFindTarget ctx.combat ctx.me_
            | None -> tryFindTarget ctx.combat ctx.me_
            )
        printfn "Queried and got: %A" target.IsSome
        match target with
        | None ->
            printfn "Couldn't find a target"
            return ()
        | Some target ->
            printfn "Still have a target"
            let! ctx = query id
            let! response = (moveToward target.Id)((), ctx)
            match response with
            | Ready() ->
                printfn "Didn't need to move, continuing"
                let! rs = query(fun ctx ->
                    match ctx.me_ with Resourcing.ConsumeRapidStrike c when c.stats.UseRapidStrike -> true | _ -> false
                    )
                let! feedback, ctx = attack({ AttackDetails.create(target.Id) with rapidStrike = rs })
                return! loop (Some target.Id)
            | Resume(keepMoving) ->
                printfn "Needed to move, but not done moving yet..." // we could use keepMoving but why bother? It's not a multi-step kind of behavior.
                return! loop (Some target.Id)
        }
    printfn "About to enter justAttack loop for the first time"
    return! loop None
    }

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
run bhv ((), toCtx (combatAt (yards 0., yards 1.)))
let (AwaitingAction(action, bhv)) = run bhv ((), toCtx (combatAt (yards 0., yards 1.)))
action // this should be Attack but is Move the first time we run it! Repros the issue!

let behavior = BehaviorBuilder()



let walk : ActionBehavior = behavior {
    printfn "***Starting walk" // should not print immediately
    let! feedback, ctx = ReturnAction(Move(Place (yards 0., yards 0.)))
    return ()
    }
let rec loop: ActionBehavior = behavior {
    printfn "***Running loop"
    let rec innerLoop walk = behavior {
        let! ctx = query(id)
        let! result = run walk ((), ctx)
        match result with
        | Resume(walking) ->
            // if we get here then walking is still ongoing, but how do I actually resume it?
            return! innerLoop walking
        | Ready () -> return! loop // could do something else instead
        }
    return! innerLoop walk
    }
let (AwaitingAction(action, b1)) = run loop ((), toCtx (combatAt (yards 0., yards 1.)))
let (AwaitingAction(action, b2)) = run b1 ((), toCtx (combatAt (yards 0., yards 1.)))
let (AwaitingAction(action, b3)) = run b2 ((), toCtx (combatAt (yards 0., yards 1.)))

let (AwaitingAction(action, b1)) = run walk ((), toCtx (combatAt (yards 0., yards 1.)))
let (AwaitingAction(action, b2)) = run b1 ((), toCtx (combatAt (yards 0., yards 1.)))
let (AwaitingAction(action, b3)) = run b2 ((), toCtx (combatAt (yards 0., yards 1.)))
2+2