module Domain.Behavior.Tests
open Expecto
open Coroutine
open Domain
open Domain.Random
open Domain.Behavior
open Domain.CombatRules
open Domain.Geo
let verify = Swensen.Unquote.Assertions.test

#nowarn "40" // we're not doing anything weird in the behavior ctors like calling arguments that are functions, or anything like that, so the warning is acknowledged but not a problem.

[<Tests>]
let Tests = testLabel "Unit" <| testList "Behavior" [
    testCase "Sanity check behavior composition" <| fun () ->
        // A simple behavior: always returns its context + 1. This very predictable behavior lets us
        // sanity-check that we're getting the ctx we expect.
        let rec incrementBehavior: Behavior<int,unit,int,_> = behavior {
            let! ctx = QueryRequest id
            let! feedback, ctx' = ReturnAction(ctx + 1)
            return! incrementBehavior
            }
        // wrapper around a simple behavior that just keeps it running forever unless it gets a signal to stop
        let rec triggerlistener stopTrigger wrapped: Behavior<int,unit,int,string> = behavior {
            let! ctx = QueryRequest id
            match stopTrigger ctx with
            | Some result ->
                return result
            | None ->
                match! run wrapped ((), ctx) with
                | Ready result -> return result
                | Resume followup ->
                    return! triggerlistener stopTrigger followup
            }

        let mutable bhv = triggerlistener (function 42 -> Some "OK" | _ -> None) incrementBehavior
        let getAction = function AwaitingAction(action, nextBehavior) -> action, nextBehavior | v -> matchfail v
        let run n =
            let action, followup = run bhv ((), n) |> getAction
            bhv <- followup
            action
        let runFinal n =
            match Coroutine.run bhv ((), n) with
            | Finished result -> result
            | v -> matchfail v
        verify <@ run 1 = 2 @>
        verify <@ run 2 = 3 @>
        verify <@ run 10 = 11 @>
        verify <@ run 6 = 7 @>
        verify <@ runFinal 42 = "OK" @>
    testCase "Cowardly should flee when damaged" <| fun () ->
        let rec fakeFlee: ActionBehavior = behavior {
            let! (feedback: unit), (ctx: ActionContext) = ReturnAction(Move(Place(10000.<yards>,0.<yards>)))
            return! fakeFlee
            }
        let teamOf m coords =
            [{  members = m
                center = coords
                radius = Some 0.<yards>
                }]
        // we don't care who's in the combat as long as there's someone on both sides and they're in range of each other
        let combat = createCombat (["Bob", Stats.create "Bob"] |> Map.ofList) (teamOf [1, "Bob"] (0.<yards>, 0.<yards>)) (teamOf [1, "Bob"] (0.<yards>, 1.<yards>)) |> fun c -> c.combat

        let bob = combat.combatants.Keys |> Seq.head
        let toCtx combat = { me = bob; combat = combat }

        let mutable BobsBhv = cowardly justAttack fakeFlee // todo: move this mutable field onto Bob himself
        let doCheckActionFor expectedAction combat = // note that this updates BobsBhv
            match run BobsBhv ((), toCtx combat) with
            | Finished () -> failwith "Bob should always be either attacking or fleeing until his opponent dies, which shouldn't happen in this test."
            | AwaitingAction(action, nextBehavior) ->
                BobsBhv <- nextBehavior
                // we don't actually DO the action in this test but we verify that it's an attack on the other Bob
                verify <@ action = expectedAction @>
        for _ in 1..10 do
            doCheckActionFor (Attack(AttackDetails.create(2, "Bob"))) combat // each time we get a new behavior, which should be an attack
        let woundedBob =
            { combat
                with combatants = combat.combatants |> Map.change bob (function
                    | Some bob -> Some { bob with injuryTaken = bob.stats.HP_ - 1 }
                    | None -> failwith "Bob should be in the combat"
                )
            }
        // wounding Bob forces him to flee until healed
        doCheckActionFor (Move(Place(10000.<yards>,0.<yards>))) woundedBob
        // but when he's healed, he should resume attacking
        doCheckActionFor (Attack(AttackDetails.create(2, "Bob"))) combat // each time we get a new behavior, which should be an attack
    testCase "justAttack should move if needed" <| fun () ->
        let teamOf m coords =
            [{  members = m
                center = coords
                radius = Some 0.<yards>
                }]
        // we don't care who's in the combat as long as there's someone on both sides and they're in range of each other
        let combatAt pos =
            let c = createCombat (["Bob", Stats.create "Bob"] |> Map.ofList) (teamOf [1, "Bob"] (0.<yards>, 0.<yards>)) (teamOf [1, "Bob"] pos) |> fun c -> c.combat
            let actual = c.geo.Find (2, "Bob")
            verify <@ pos = actual @>
            c
        let bob = (1, "Bob")
        let toCtx combat = { me = bob; combat = combat }

        let mutable BobsBhv = justAttack // todo: move this mutable field onto Bob himself
        let doCheckActionFor expected combat = // note that this updates BobsBhv
            match run BobsBhv ((), toCtx combat) with
            | Finished () -> failwith "Bob should always be either attacking or fleeing until his opponent dies, which shouldn't happen in this test."
            | AwaitingAction(action, nextBehavior) ->
                BobsBhv <- nextBehavior
                // we don't actually DO the action in this test but we verify that it's an attack on the other Bob
                verify <@ expected = action @>
        doCheckActionFor (Attack (AttackDetails.create(2, "Bob"))) (combatAt (coords (0., 1.))) // if we start in reach we should attack
        doCheckActionFor (Move(Person (2, "Bob"))) (combatAt (coords (5., 5.))) // if we're out of reach next turn we should move
        doCheckActionFor (Attack (AttackDetails.create(2, "Bob"))) (combatAt (coords (0., 1.))) // but when we're in reach we should just attack
    ]