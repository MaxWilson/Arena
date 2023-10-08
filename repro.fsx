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
            let! response = RunChildRequest (moveToward target.Id)
            match response with
            | Finished() ->
                printfn "Didn't need to move, continuing"
                let! rs = query(fun ctx ->
                    match ctx.me_ with Resourcing.ConsumeRapidStrike c when c.stats.UseRapidStrike -> true | _ -> false
                    )
                let! feedback, ctx = attack({ AttackDetails.create(target.Id) with rapidStrike = rs })
                return! loop (Some target.Id)
            | AwaitingAction(action, followup) ->
                printfn "Need to move, let's do that and then continue..."
                let! feedback, ctx = ReturnAction action
                printfn "Finished the move, continuing..."
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

let bhv : ActionBehavior = behavior {
    printfn "Starting bhv" // should not print immediately
    return ()
    }
let b = behavior in
    b.Run(b.Delay(fun () -> b.Return ()))
