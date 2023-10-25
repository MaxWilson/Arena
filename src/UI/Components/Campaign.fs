module UI.Components.Campaign
open Domain.Data
open Domain.Campaign

module Persist =
    open UI.LocalStorage
    open Domain
    let key = "Roster"
    let cacheRead, cacheInvalidate = Cache.create()
    let read (): Roster =
        cacheRead (thunk2 read key (thunk []))
    let write (v: Roster) =
        write key v
        cacheInvalidate()
type Mode = ChooseParty | ChooseOpposition | SetupEncounter | Adventure | Treasure | Commit
type Model = {
    mode: Mode
    roster: Roster
    monsters: Domain.Data.MonsterDatabase
    currentEncounterSetup: Setup
    }
    with static member fresh = { mode = ChooseParty; roster = Persist.read(); monsters = { catalog = Domain.Defaults.database() }; currentEncounterSetup = [] }

type Msg =
    | ChangeRoster of (Roster -> Roster)
    | ChangeSetup of (Setup -> Setup)
    | SetMode of Mode
let update msg model =
    match msg with
    | ChangeRoster f ->
        { model with roster = model.roster |> f }
    | ChangeSetup f ->
        { model with currentEncounterSetup = model.currentEncounterSetup |> f }
    | SetMode mode ->
        { model with mode = mode }
let init _ =
    Model.fresh

