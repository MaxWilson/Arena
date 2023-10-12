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
                            (sideB.center, sideB.radius, name, min, max, defeatCriteria) with
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
