module Domain.Behavior

open Coroutine
open Domain.Geo

let behavior = BehaviorBuilder()

let prioritizeTargets (combat: Combat) (attacker: Combatant) =
    let betweenInclusive (min, max) x = min <= x && x <= max
    let potentialTargets =
        combat.combatants.Values
        |> Seq.filter (fun c -> c.team <> attacker.team)
        |> Seq.filter (fun c -> c.isAny[Dead;Unconscious] |> not)
        // We don't want to overkill damage, so we put targets that might fall unconscious by themselves
        // fairly low in priority, although we also want to damage vulnerable targets while they're vulnerable
        // so we put stunned and prone targets at high priority.
        // prefer targets that are stunned but not yet at -HP,
        // then targets that are prone but not yet at -HP,
        // then targets that are below 1/3 HP but not yet at 0 HP,
        // then targets at or below 0 HP
        // then anyone still alive (ordered by distance)
        |> Seq.sortBy(fun c ->
            combat.geo.WithinDistance(attacker.Id, c.Id, 1.<yards>), // prefer targets within reach
            ((c.is Stunned)
                && c.CurrentHP_ > -c.stats.HP_) |> not,
            ((c.is Prone)
                && c.CurrentHP_ > -c.stats.HP_) |> not,
            betweenInclusive (0, (c.stats.HP_ + 1) / 3) c.CurrentHP_ |> not,
            c.CurrentHP_ <= 0 && not c.stats.SupernaturalDurability,
            combat.geo.EuclideanSquared(attacker.Id, c.Id) |> Ops.round, // all else being equal, pick closer targets
            c.number)
    potentialTargets

let tryFindTarget (combat: Combat) (attacker: Combatant) =
    prioritizeTargets combat attacker |> Seq.tryHead

let query(f: ActionContext -> _) = QueryRequest f
let attack details = ReturnAction (Attack details)

let nullBehavior = (behavior { return () })

// move toward is a finite behavior, stops when you get within 1 yard of the target
let rec moveToward (targetId: CombatantId): ActionBehavior = behavior {
    let! geo, inReach = query(fun ctx -> ctx.geo, ctx.geo.WithinDistance(ctx.me, targetId, 1.0<yards>))
    if inReach then // TODO: enforce distance in action resolution, and allow Behavior to preview enforcement just like with ConsumeAttack. For now we just want to prevent infinite loops in the behavior.
        return () // done! We're in range, can do something else now.
    else
        let! feedback, ctx = ReturnAction(Move(Person targetId))
        return! moveToward targetId
    }

let justAttack : ActionBehavior = behavior {
    let rec loop targetId keepMoving = behavior {
        let! target, changed = query(fun ctx ->
            match targetId with
            | Some targetId ->
                let target = ctx.combat.combatants[targetId]
                if target.isnt [Unconscious; Dead] then Some target, false
                else tryFindTarget ctx.combat ctx.me_, true
            | None -> tryFindTarget ctx.combat ctx.me_, true
            )
        match target with
        | None -> return ()
        | Some target ->
            let! ctx = query id
            match! run (if changed then moveToward target.Id else keepMoving) ((), ctx) with
            | Ready() ->
                let! rs = query(fun ctx ->
                    match ctx.me_ with Resourcing.ConsumeRapidStrike c when c.stats.UseRapidStrike -> true | _ -> false
                    )
                let! feedback, ctx = attack({ AttackDetails.create(target.Id) with rapidStrike = rs })
                return! loop (Some target.Id) (moveToward target.Id) // give it moveToward because the target might move again
            | Resume(keepMoving) ->
                return! loop (Some target.Id) keepMoving
        }
    return! loop None nullBehavior
    }

// note: we're not doing nowarn 40 here because throwing a notImpl exception is kind of weird, and if justFlee were a recursive object instead
// of a recursive function, we'd get a runtime exception because of that. In the long run that won't be a problem but for now the warning is relevant.
let rec justFlee() : ActionBehavior = behavior {
    let pos = query(fun ctx -> notImpl "Combatant needs a position first before we can implement flee")
    let! feedback, ctx = ReturnAction(Move(notImpl pos))
    return! justFlee() // just move from now until the end of time
    }

let cowardly bhv flee : ActionBehavior = behavior {
    let rec loop bhv flee = behavior {
        let! ctx = query id
        let brave = let me = ctx.me_ in me.CurrentHP_ > me.stats.HP_ / 3
        let! result = (if brave then bhv else flee)((), ctx)
        match result with
        | Resume followup ->
            // update whichever behavior we used
            let bhv, flee = if brave then followup, flee else bhv, followup
            return! loop bhv flee // we track state for both bhv and flee separately,
            // so we can un-flee if we get healed. Otherwise we would just return! flee and permanently go into flee mode.
        | Ready () -> return ()
        }
    return! loop bhv flee
    }