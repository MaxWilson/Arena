module Domain.Behavior.Tests
open Expecto
open Coroutine
open Domain
open Domain.Random
open Domain.Behavior
open Domain.CombatRules
let verify = Swensen.Unquote.Assertions.test

#nowarn "40" // we're not doing anything weird in the behavior ctors like calling arguments that are functions, or anything like that, so the warning is acknowledged but not a problem.

[<Tests>]
let Tests = testLabel "Unit" <| testList "Behavior" [
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
        let combat = createCombat (["Bob", Creature.create "Bob"] |> Map.ofList) (teamOf [1, "Bob"] (0.<yards>, 0.<yards>)) (teamOf [1, "Bob"] (0.<yards>, 1.<yards>)) |> fun c -> c.combat

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
        let combatAt pos = createCombat (["Bob", Creature.create "Bob"] |> Map.ofList) (teamOf [1, "Bob"] (0.<yards>, 0.<yards>)) (teamOf [1, "Bob"] pos) |> fun c -> c.combat

        let bob = (1, "Bob")
        let toCtx combat = { me = bob; combat = combat }

        let mutable BobsBhv = justAttack // todo: move this mutable field onto Bob himself
        let doCheckActionFor expected combat = // note that this updates BobsBhv
            match run BobsBhv ((), toCtx combat) with
            | Finished () -> failwith "Bob should always be either attacking or fleeing until his opponent dies, which shouldn't happen in this test."
            | AwaitingAction(action, nextBehavior) ->
                BobsBhv <- nextBehavior
                // we don't actually DO the action in this test but we verify that it's an attack on the other Bob
                verify <@ action = expected @>
        doCheckActionFor (Attack (AttackDetails.create(2, "Bob"))) (combatAt (0.<yards>, 0.3<yards>)) // when we're in reach we should just attack
        doCheckActionFor (Move(Person (2, "Bob"))) (combatAt (yards 0., yards 2.)) // the first time we should get a move
        doCheckActionFor (Attack (AttackDetails.create(2, "Bob"))) ((combatAt (yards 0., yards 2.)) |> CombatAtom.updateCombat (MoveTo((2, "Bob"), (yards 0., yards 1.), 0, ""))) // but when we're in reach we should just attack
    ]