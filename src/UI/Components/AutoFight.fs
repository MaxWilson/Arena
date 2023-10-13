module UI.Components.AutoFight
open Domain
open Domain.CombatRules

type Page =
    | Home
    | Editing of name:string
type Awaitable<'inProgress, 't> =
    | NotStarted
    | InProgress of 'inProgress
    | Completed of 't
type Model = {
    page: Page
    fightSetup: FightSetup
    database : MonsterDatabase
    execution: Awaitable<string option, FightSetup * FightResult>
    }
type Side = SideA | SideB
type Msg =
    | ChangeFightSetup of (FightSetup -> FightSetup)
    | Clear of Side
    | Upsert of Creature
    | SetPage of Page
    | Fighting of Awaitable<string option, FightSetup * FightResult>

open Common
open Fable.Core
open Feliz
open Elmish
open Fable.Core.JsInterop
open Domain
open Domain.CombatRules
open Domain.Random
open Domain.Random.Parser
open Feliz.Listeners

[<Emit("$0.scrollIntoView({block: 'nearest', inline: 'nearest'})")>]
let scrollIntoView (element: Browser.Types.Node) = jsNative
[<Emit("$0.scrollIntoView({block: 'start', inline: 'nearest'})")>]
let scrollSectionIntoView (element: Browser.Types.Node) = jsNative

let update msg model =
    match msg with
    | ChangeFightSetup f -> { model with fightSetup = f model.fightSetup }
    | SetPage page -> { model with page = page }
    | Clear side ->
        let clearSide = function
            | SideA -> { model.fightSetup with sideA = [] }
            | SideB -> { model.fightSetup with sideB = Team.freshCalibrated() }
        { model with fightSetup = clearSide side }
    | Upsert creature ->
        if creature.name |> String.isntWhitespace then
            let db = MonsterDatabase.add creature model.database
            UI.LocalStorage.Catalog.write db.catalog
            { model with database = db }
        else model
    | Fighting v -> { model with execution = v }

let init () =
    let dev = false
    let updateWithDefaults catalog =
        if dev then
            // during development, we want to be able to overwrite user defaults so they get e.g. Berserk minotaurs when we add a Berserk field
            let mutable output = catalog
            let defaults = Domain.Defaults.database()
            for k in defaults.Keys do
                output <- output |> Map.add k defaults[k]
            output
        else catalog
    let db =
        { catalog = UI.LocalStorage.Catalog.read() |> updateWithDefaults }
    let fight = {
        sideA = [2, "Stone Golem"; 1, "Peshkali"] |> Team.fresh
        sideB = Opposition.calibrated (Some "Orc", None, None, TPK) Team.randomInitialPosition
        }
    { page = Home; fightSetup = fight; database = db; execution = NotStarted }

let specificFight db team1 team2 = async {
    let cqrs = CQRS.CQRS.create((createCombat db team1 team2), CombatAtom.update)
    let victors = fight cqrs
    return (cqrs.LogWithMessages() |> loggedOnly), victors
    }

type DifficultyGuidance = TooLow | TooHigh | JustRight

// find an inclusive range [lowest, highest] range of values that satisfy the given asynchronous evaluation function
let findRange evaluate (hardCap: _ option) = async {
    // first, look for a bound on the *upper* bound
    let rec step1 n = async {
        let capped = (match hardCap with Some hardCap -> min hardCap n | None -> n)
        match! evaluate capped with
        | TooLow when hardCap.IsSome && capped = hardCap.Value -> return (hardCap.Value, hardCap.Value) // special case: if we can't go any higher and it's still too low, just return the hardcap.
        | (TooLow | JustRight) when (hardCap.IsNone || n <= hardCap.Value) -> return! step1 (if n = 1 then 2 else n * 3 / 2) // about a 50% increase each time
        | TooHigh | _ ->
            // then, use binary search to seek both the lower and upper bound
            let rec binarySearch eval (minInclusive, maxInclusive) = async {
                let mid = (minInclusive + maxInclusive) / 2
                if minInclusive >= maxInclusive then return minInclusive // if we've narrowed it down to a single value, accept it as the best we can do
                else
                    match! eval mid with
                    | TooLow when minInclusive + 1 = maxInclusive -> return maxInclusive // special case when min and max are adjacent--no more recursion
                    | TooHigh when minInclusive + 1 = maxInclusive -> return minInclusive // special case when min and max are adjacent--no more recursion
                    | TooLow -> return! binarySearch eval (mid, maxInclusive)
                    | TooHigh -> return! binarySearch eval (minInclusive, mid)
                    | JustRight -> return mid
                }
            let rec findLowerBound n = async {
                let! evalN = evaluate n
                let! evalJustBelow = evaluate (n-1)
                return
                    match evalN, evalJustBelow with
                    | JustRight, TooLow -> JustRight
                    | TooLow, _ -> TooLow
                    | TooHigh, _ -> TooHigh
                    | JustRight, JustRight -> TooHigh // go lower until you find a JustRight/TooLow pair.
                    | JustRight, TooHigh -> TooHigh // Note: JustRight/TooHigh doesn't make sense but could happen anyway if eval is noisy.
                }
            let rec findUpperBound n = async {
                let! evalN = evaluate n
                let! evalJustAbove = evaluate (n+1)
                return
                    match evalN, evalJustAbove with
                    | JustRight, TooHigh -> JustRight
                    | TooLow, _ -> TooLow
                    | TooHigh, _ -> TooHigh
                    | JustRight, JustRight -> TooLow // go higher until you find a JustRight/TooHigh pair.
                    | JustRight, TooLow -> TooLow // Note: JustRight/TooLow doesn't make sense but could happen anyway if eval is noisy.
                }
            match hardCap with
            | Some hardCap when n > hardCap ->
                let range = (1, hardCap)
                let upper = hardCap
                let! lower = binarySearch findLowerBound range
                return lower, upper
            | _ ->
                let range = (1, n)
                let! lower = binarySearch findLowerBound range
                let! upper = binarySearch findUpperBound range
                return lower, upper
        }
    return! step1 1
    }

let calibrate inform (db: Map<_,Creature>) (team1: TeamSetup) (center: Coords, radius: Distance option, enemyType, minbound, maxbound, defeatCriteria) = async {
    let enemyStats = db[enemyType]
    let runForN n run = async {
        inform $"Evaluating vs. {enemyStats.Quantify n} ({run}/10)"
        do! Async.Sleep 0 // yield the JS runtime  in case UI updates need to be processed
        let combat = createCombat db team1 [{ members = [n, enemyType ]; center = center; radius = radius }] // instantiate. TODO: instantiate at specific positions, as soon as monsters have positions.
        let cqrs = CQRS.CQRS.create(combat, CombatAtom.update)
        return cqrs, fight cqrs
        }
    let get n = async {
        inform $"Evaluating vs. {enemyStats.Quantify n}"
        let! runs = async {
            let mutable finished = []
            for run in 1..10 do
                let! r = runForN n run
                finished <- r :: finished
            return finished
            }
        let victoryMetric : CQRS.CQRS<_, AugmentedCombat> * {| victors: int list |} -> int =
            match defeatCriteria with
            | TPK -> function (_, v) when v.victors = [1] -> 1 | otherwise -> 0
            | OneCasualty ->
                fun (cqrs, v) ->
                    // in this case, TeamA is very casualty-averse. Defeat is taking even one casualty (dead or unconscious).
                    if cqrs.State.combat.combatants.Values |> Seq.exists (fun c ->
                        c.team = 1 && c.isAny[Dead; Unconscious]) then
                        0
                    else 1
            | HalfCasualties ->
                fun (cqrs, v) ->
                    // in this case, TeamA is somewhat casualty-averse. Defeat is a pyrrhic victory where at least half the team dies.
                    let friendlies = cqrs.State.combat.combatants.Values |> Seq.filter (fun c -> c.team = 1)
                    let deadFriendlies = friendlies |> Seq.filter (fun c -> c.isAny[Dead; Unconscious])
                    if deadFriendlies |> Seq.length >= ((friendlies |> Seq.length) / 2) then
                        0
                    else 1

        let victories = runs |> List.sumBy victoryMetric
        return victories
        }

    let mutable resultsCache = Map.empty
    let eval n = async {
        if resultsCache.ContainsKey n then return resultsCache[n]
        else
            let! victories= get n
            let v =
                if float victories / 100. < minbound then TooHigh // make it easier, and hard cap at 100 monsters to prevent nontermination
                elif float victories / 100. > maxbound then TooLow // make it harder
                else JustRight
            resultsCache <- resultsCache |> Map.add n v
            return resultsCache[n]
        }
    match! findRange eval (Some 100) with
    | min, max ->
        let fightFor n =
            let combat = createCombat db team1 [{ members = [n, enemyType ]; center = center; radius = radius }] // instantiate. TODO: instantiate at specific positions, as soon as monsters have positions.
            let cqrs = CQRS.CQRS.create(combat, CombatAtom.update)
            fight cqrs |> ignore
            let sampleFight = cqrs.LogWithMessages()
            loggedOnly sampleFight
        return Some min, Some max, Some (fightFor max)
    }

let beginFights (model: Model) dispatch =
    match model.execution with
    | InProgress _ ->
        ()
    | _ ->
        let g = System.Guid.NewGuid()
        Fighting (InProgress None) |> dispatch
        async {
            do! Async.Sleep 0 // yield the JS runtime  in case UI updates need to be processed
            try
                match model.fightSetup.sideB with
                | _ when (model.fightSetup.sideA |> List.every (fun group -> group.members |> List.sumBy fst = 0)) ->
                    Fighting NotStarted |> dispatch
                    informUserOfError "You have to pick monsters first"
                | Calibrate({ members = None, _, _, _ }) ->
                    Fighting NotStarted |> dispatch
                    informUserOfError "You have to pick monsters first"
                | Calibrate({ members = (Some name, min, max, defeatCriteria) } as sideB) ->
                    let min = (defaultArg min 50 |> float) / 100.
                    let max = (defaultArg max 90 |> float) / 100.
                    match! calibrate (Some >> InProgress >> Fighting >> dispatch) model.database.catalog
                            model.fightSetup.sideA
                            (sideB.center, sideB.radius, name, float min / 100., float max / 100., defeatCriteria) with
                    | minQuantity, maxQuantity, Some sampleMaxFight ->
                        Completed(model.fightSetup, CalibratedResult(minQuantity, maxQuantity, sampleMaxFight)) |> Fighting |> dispatch
                    | v ->
                        Fighting NotStarted |> dispatch
                        informUserOfError "Failed to find a number of monsters that would satisfy those constraints. Try a wider range like 20% to 100%"
                | Specific(sideB) ->
                    let! fightResult = specificFight model.database.catalog model.fightSetup.sideA sideB
                    (model.fightSetup, SpecificResult fightResult)
                        |> Completed
                        |> Fighting
                        |> dispatch
            with err ->
                NotStarted |> Fighting |> dispatch // reset the UI back to a usable state, which will be shown only after the error is dismissed
                raise err // cannot reraise() for some reason but hopefully that doesn't matter because JavaScript doesn't give stack traces anyway in this case, even if the try/catch is deleted. Use the browser debugger to get the stack trace if needed.
            } |> Async.StartAsPromise |> ignore
