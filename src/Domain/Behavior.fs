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
        // then anyone still alive (ordered by statblock name and number because why not, and it makes readouts more predictable)
        |> Seq.sortBy(fun c ->
            ((c.is Stunned)
                && c.CurrentHP_ > -c.stats.HP_) |> not,
            ((c.is Prone)
                && c.CurrentHP_ > -c.stats.HP_) |> not,
            betweenInclusive (0, (c.stats.HP_ + 1) / 3) c.CurrentHP_ |> not,
            c.CurrentHP_ <= 0 && not c.stats.SupernaturalDurability,
            c.number)
    potentialTargets

let tryFindTarget (combat: Combat) (attacker: Combatant) =
    prioritizeTargets combat attacker |> Seq.tryHead

let query(f: ActionContext -> _) = QueryRequest f
let attack details = ReturnAction (Attack details)
// move toward is a finite behavior, stops when you get within 1 yard of the target
let rec moveToward targetId: ActionBehavior = behavior {
    let! geo, dist = query(fun ctx -> ctx.geo, ctx.geo.DistanceBetween(ctx.me, targetId))
    if dist <= 1.0<yards> then
        return () // done! We're in range, can do something else now.
    else
        let! feedback, ctx = ReturnAction(Move(Person targetId))
        return! moveToward targetId
    }

let justAttack : ActionBehavior = behavior {
    let rec loop targetId = behavior {
        let! target = query(fun ctx ->
            match targetId with
            | Some targetId ->
                let target = ctx.combat.combatants[targetId]
                if target.isnt [Unconscious; Dead] then Some target
                else tryFindTarget ctx.combat ctx.me_
            | None -> tryFindTarget ctx.combat ctx.me_
            )
        match target with
        | None -> return ()
        | Some target ->
            match! RunChildRequest (moveToward target.Id) with
            | Finished() ->
                let! rs = query(fun ctx ->
                    match ctx.me_ with Resourcing.ConsumeRapidStrike c when c.stats.UseRapidStrike -> true | _ -> false
                    )
                let! feedback, ctx = attack({ AttackDetails.create(target.Id) with rapidStrike = rs })
                return! loop (Some target.Id)
            | AwaitingAction(action, followup) ->
                let! feedback, ctx = ReturnAction action
                return! loop (Some target.Id)
        }
    return! loop None
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
        let! brave = query(fun ctx -> let me = ctx.me_ in me.CurrentHP_ > me.stats.HP_ / 3)

        let! result = RunChildRequest (if brave then bhv else flee)
        match result with
        | Finished result -> return result
        | AwaitingAction(action, followup) ->
            // update whichever behavior we used
            let bhv, flee = if brave then followup, flee else bhv, followup
            return! loop bhv flee // we track state for both bhv and flee separately,
            // so we can un-flee if we get healed. Otherwise we would just return! flee and permanently go into flee mode.
        }
    return! loop bhv flee
    }