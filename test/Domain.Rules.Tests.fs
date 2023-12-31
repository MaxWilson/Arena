module Domain.Rules.Tests
open Expecto
open Domain
open Domain.Random
open Domain.Behavior
open Domain.CombatRules
let verify = Swensen.Unquote.Assertions.test
let shouldFail = Swensen.Unquote.Assertions.raises

// Slightly simpler version of DefenseDetails to make test output easier to read
type DefenseResult = { defense: DefenseType; targetRetreated: bool }
    with static member create (targetNumber, input: DefenseDetails) = targetNumber, { defense = input.defense; targetRetreated = input.retreating.IsSome }

[<Tests>]
let Tests = testLabel "Unit" <| testList "Rules" [
    testCase "Spot check damage computations" <| fun () ->
        verify <@ swingDamage 25 +2 = RandomThrow.create(5,6,+1) @>
        verify <@ swingDamage 6 +2 = RandomThrow.create(1,6,-1) @>
        verify <@ thrustDamage 3 0 = RandomThrow.create(1,6,-5) @>
        verify <@ thrustDamage 37 +3 = RandomThrow.create(4,6,+3) @>
        let baseDamage st =
            thrustDamage st 0, swingDamage st 0
        verify <@ baseDamage 16 = (RandomThrow.create(1,6,+1), RandomThrow.create(2,6,+2)) @>
        verify <@ baseDamage 45 = (RandomThrow.create(5,6), RandomThrow.create(7,6,+1)) @>
        verify <@ baseDamage 70 = (RandomThrow.create(8,6), RandomThrow.create(10,6)) @>
        verify <@ baseDamage 75 = (RandomThrow.create(8,6,+2), RandomThrow.create(10,6,+2)) @>
        verify <@ baseDamage 100 = (RandomThrow.create(11,6), RandomThrow.create(13,6)) @>

    testCase "Spot check defense choice" <| fun () ->
        let previousAttacker = Combatant.fresh(2, "Ogre 1", 1, Stats.create "Ogre 1")
        let attacker = Combatant.fresh(2, "Ogre 2", 2, Stats.create "Ogre 2")
        let gunman = Combatant.fresh(2, "Gunman", 3, { Stats.create "Gunman" with CannotBeParried = true })
        let nowhere = coords (0., 0.)
        let geo = Geo.ofList [(1, "test1"), coords(0., 0.); previousAttacker.Id, coords (1., 0.); attacker.Id, coords(0., 1.); gunman.Id, coords(1., 1.)]
        let chooseDefense = chooseDefense geo
        let create dodge parry block retreatUsed =
            let stats = { Stats.create("test") with Dodge = Some dodge; Parry = Some parry; Block = Some block }
            { Combatant.fresh(1, "test1", 1, stats) with retreating = if retreatUsed then Some (previousAttacker.Id, nowhere) else None }
        let chooseDefenseWith f dodge parry block retreat parriesUsed =
            let combatant = create dodge parry block retreat
            let combatant = { combatant with parriesUsed = parriesUsed; stats = f combatant.stats }
            chooseDefense attacker combatant |> DefenseResult.create
        let chooseDefenseWithExtraParry dodge parry block retreat extraParries parriesUsed =
            chooseDefenseWith (fun stats -> { stats with ExtraParry = Some extraParries }) dodge parry block retreat parriesUsed
        let chooseDefenseWithExtraParryF f dodge parry block retreat extraParries parriesUsed =
            chooseDefenseWith (fun stats -> { stats with ExtraParry = Some extraParries } |> f) dodge parry block retreat parriesUsed
        let chooseDefenseUnderConditions dodge parry block retreat damage conditions =
            let combatant = { create dodge parry block retreat with statusMods = conditions; injuryTaken = damage }
            chooseDefense attacker combatant |> DefenseResult.create
        let chooseDefenseWithPriorRetreat dodge parry block previousRetreat =
            let combatant = { create dodge parry block false with retreating = Some (previousRetreat, nowhere) }
            chooseDefense attacker combatant |> DefenseResult.create
        let chooseDefenseFromGunman dodge parry block retreat =
            let combatant = create dodge parry block retreat
            chooseDefense gunman combatant |> DefenseResult.create
        let chooseDefense dodge parry block retreat =
            let combatant = create dodge parry block retreat
            chooseDefense attacker combatant |> DefenseResult.create
        verify <@ chooseDefense 10 0 0 true = (10, { defense = Dodge; targetRetreated = false })  @>
        verify <@ chooseDefense 10 13 9 true = (13, { defense = Parry; targetRetreated = false }) @>
        verify <@ chooseDefenseFromGunman 10 13 9 true = (10, { defense = Dodge; targetRetreated = false }) @>
        verify <@ chooseDefense 10 9 14 true = (14, { defense = Block; targetRetreated = false }) @>
        verify <@ chooseDefense 10 10 10 false = (13, { defense = Dodge; targetRetreated = true })  @>
        verify <@ chooseDefense 10 13 9 false = (14, { defense = Parry; targetRetreated = true }) @>
        verify <@ chooseDefense 10 9 14 false = (15, { defense = Block; targetRetreated = true }) @>
        verify <@ chooseDefenseUnderConditions 10 0 0 false 7 [] = (8, { defense = Dodge; targetRetreated = true })  @>
        verify <@ chooseDefenseUnderConditions 10 10 10 false 7 [] = (11, { defense = Parry; targetRetreated = true })  @>
        verify <@ chooseDefenseUnderConditions 10 0 0 true 0 [Stunned] = (6, { defense = Dodge; targetRetreated = false })  @>
        verify <@ chooseDefenseUnderConditions 10 0 0 false 0 [Stunned] = (6, { defense = Dodge; targetRetreated = false })  @>
        verify <@ chooseDefenseUnderConditions 10 0 0 true 7 [Stunned] = (1, { defense = Dodge; targetRetreated = false })  @>
        verify <@ chooseDefenseUnderConditions 10 0 0 false 7 [Stunned] = (1, { defense = Dodge; targetRetreated = false })  @>
        verify <@ chooseDefenseUnderConditions 10 0 0 false 0 [Prone] = (10, { defense = Dodge; targetRetreated = true })  @>
        verify <@ chooseDefenseUnderConditions 10 0 0 true 0 [Stunned;Prone] = (3, { defense = Dodge; targetRetreated = false })  @>
        verify <@ chooseDefenseWithExtraParry 10 13 0 false 5 0 = (14, { defense = Parry; targetRetreated = true })  @>
        verify <@ chooseDefenseWithExtraParry 10 13 0 true 5 0 = (13, { defense = Parry; targetRetreated = false })  @>
        verify <@ chooseDefenseWithExtraParry 10 13 0 true 5 1 = (13, { defense = Parry; targetRetreated = false })  @>
        verify <@ chooseDefenseWithExtraParry 10 13 0 true 5 5 = (13, { defense = Parry; targetRetreated = false })  @>
        let chooseDefenseWithFencingParry = chooseDefenseWithExtraParryF (fun s -> { s with FencingParry = true })
        verify <@ chooseDefenseWithFencingParry 10 13 0 true 5 6 = (11, { defense = Parry; targetRetreated = false })  @>
        verify <@ chooseDefenseWithExtraParry 10 13 0 true 5 6 = (10, { defense = Dodge; targetRetreated = false })  @>
        verify <@ chooseDefenseWithExtraParry 10 13 0 true 5 7 = (10, { defense = Dodge; targetRetreated = false })  @>
        verify <@ chooseDefenseWithExtraParry 0 13 0 true 5 7 = (9, { defense = Parry; targetRetreated = false })  @>
        verify <@ chooseDefenseWithExtraParry 0 17 14 true 0 3 = (14, { defense = Block; targetRetreated = false })  @>
        let chooseDefenseWeaponMasterFencing args = chooseDefenseWithExtraParryF (fun s -> { s with WeaponMaster = true; FencingParry = true }) args
        let chooseDefenseWeaponMasterBroadsword args = chooseDefenseWithExtraParryF (fun s -> { s with WeaponMaster = true }) args
        verify <@ chooseDefenseWithExtraParry 10 17 0 true 0 3 = (10, { defense = Dodge; targetRetreated = false })  @>
        verify <@ chooseDefenseWithFencingParry 10 17 0 true 0 3 = (11, { defense = Parry; targetRetreated = false })  @>
        verify <@ chooseDefenseWeaponMasterBroadsword 10 17 0 true 0 3 = (11, { defense = Parry; targetRetreated = false })  @>
        verify <@ chooseDefenseWeaponMasterFencing 10 17 0 true 0 3 = (14, { defense = Parry; targetRetreated = false })  @>
        verify <@ chooseDefenseWeaponMasterFencing 10 17 0 false 0 3 = (17, { defense = Parry; targetRetreated = true })  @>
        // prove that retreat works across multiple defenses
        verify <@ chooseDefenseWithPriorRetreat 10 0 0 previousAttacker.Id = (10, { defense = Dodge; targetRetreated = false })  @>
        verify <@ chooseDefenseWithPriorRetreat 10 0 0 attacker.Id = (13, { defense = Dodge; targetRetreated = true })  @>

    testCase "Spot check target prioritization" <| fun () ->
        let attacker =
            Combatant.fresh(1, "Andy", 1, Stats.create "Knight")
        let mutable counter = 2
        let create name injury conditions =
            counter <- counter + 1
            { Combatant.fresh(2, name, counter, Stats.create "Target") with injuryTaken = injury; statusMods = conditions }
        // we put stunned and prone targets at high priority.
        // prefer targets that are stunned but not yet at -HP,
        // then targets that are prone but not yet at -HP,
        // then targets that are below 1/3 HP but not yet at 0 HP,
        // then targets at or below 0 HP
        // then anyone still alive (ordered by statblock name and number because why not, and it makes readouts more predictable)
        let combat =
            [
                attacker
                create "Perfectly Fine Guy" 0 []
                create "Dead Guy" 100 [Dead]
                create "Stunned Guy" 3 [Stunned]
                create "Prone Guy" 3 [Prone]
                create "Hurt Guy" 8 []
                create "Stunned Dying Guy" 23 [Stunned]
                create "Badly Hurt Guy" 13 []
                create "Dying Guy" 22 [Unconscious]
                ]
            |> fun guys -> { Combat.fresh with combatants = guys |> List.map (fun c -> c.Id, c) |> Map.ofList; geo = Geo.ofList [for ix, c in guys |> List.mapi Tuple2.create -> c.Id, coords (float ix*2., 0.)] }
        let priority = prioritizeTargets combat attacker |> List.ofSeq |> List.map (fun c -> c.personalName)
        verify <@ priority
                    = ["Stunned Guy"; "Prone Guy"; "Hurt Guy"; "Perfectly Fine Guy"; "Stunned Dying Guy"; "Badly Hurt Guy"; ] @>
    testCase "Spot check death check thresholds" <| fun () ->
        let getThresholds fullHP startFrom =
            let mutable thresholds = []
            failedDeathcheck (fun threshold -> thresholds <- thresholds@[threshold]; true) fullHP startFrom (fullHP * -5 + 1)
            |> ignore
            thresholds
        verify <@ getThresholds 10 0 = [-10; -20; -30; -40] @>
        verify <@ getThresholds 10 10 = [-10; -20; -30; -40] @>
        verify <@ getThresholds 10 -9 = [-10; -20; -30; -40] @>
        verify <@ getThresholds 10 -10 = [-20; -30; -40] @>
        verify <@ getThresholds 14 -10 = [-14; -28; -42; -56] @>
    testCase "Check for auto-berserk" <| fun () ->
        let parse input =
            match Packrat.ParseArgs.Init input with
            | Domain.Parser.Creature (v, Packrat.End) -> v
            | v -> shouldntHappen()
        let db = [parse "Minotaur: ST 23 Berserk Auto"] |> List.map (fun c -> c.name, c) |> Map.ofList
        let c = createCombat db (Setup.fresh [(1, "Minotaur")]) (Setup.fresh [(1, "Minotaur")]) |> fun c -> c.combat
        verify <@ c.combatants.Values |> List.ofSeq |> List.every (fun c -> c.is Berserk) @>

    testCase "Resourcing spot-checks" <| fun() ->
        let inigo =
            Combatant.fresh(1, "Inigo Montoya", 1, { Stats.create "Inigo" with UseRapidStrike = true; ExtraAttack = Some 1 })
            |> Combatant.newTurn
        let after = match inigo with | Resourcing.ConsumeRapidStrike c -> c | v -> matchfail v
        verify <@ after.rapidStrikeBudget = Some 1 @>
        verify <@ after.attackBudget = 1 @>
        verify <@ after.maneuverBudget = 0 @>
        let after = match after with | Resourcing.ConsumeRapidStrike c -> c | v -> matchfail v
        verify <@ after.rapidStrikeBudget = Some 0 @>
        verify <@ after.attackBudget = 1 @>
        verify <@ after.maneuverBudget = 0 @>
        Swensen.Unquote.Assertions.raises<System.InvalidOperationException> <@ match after with | Resourcing.ConsumeRapidStrike c -> c | v -> matchfail v @>
        let after = match after with | Resourcing.ConsumeAttack c -> c | v -> matchfail v
        verify <@ after.rapidStrikeBudget = Some 0 @>
        verify <@ after.attackBudget = 0 @>
        verify <@ after.maneuverBudget = 0 @>

    testCase "TryApproach should never ask for an invalid movement--if it returns a result, actually moving there should always work" <| fun() ->
        let alice = Combatant.fresh(1, "Alice", 1, Stats.create "Alice")
        let bob = Combatant.fresh(1, "Bob", 1, Stats.create "Bob")
        let cqrs =
            let combat =
                {   Combat.fresh with
                        combatants = [alice; bob] |> List.map (fun c -> c.Id, c) |> Map.ofList
                        geo = Geo.ofList [alice.Id, coords (0., 0.); bob.Id, coords (0., 2.)]
                }
            CQRS.CQRS.create(combat, CombatAtom.updateCombat)
        let (goalPos, dist, cost) = cqrs.State.geo |> Geo.tryApproach (bob.Id, Place (coords (2., 0.)), 4) |> Option.get
        cqrs.Execute (MoveTo(bob.Id, cqrs.State.geo.lookup[bob.Id], goalPos, cost, ""))
        let (coords, dist, cost) = cqrs.State.geo |> Geo.tryApproach (alice.Id, Place (coords (2., 0.)), 4) |> Option.get
        cqrs.Execute (MoveTo(bob.Id, cqrs.State.geo.lookup[alice.Id], goalPos, cost, ""))
        ()
    ]
