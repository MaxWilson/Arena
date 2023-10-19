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

type Model = {
    roster: Roster
    currentEncounterSetup: Domain.Campaign.Setup
    }
    with static member fresh = { roster = []; currentEncounterSetup = Domain.Campaign.Setup [] }
type Msg =
    | ChangeRoster of (Roster -> Roster)
let update msg model =
    match msg with
    | ChangeRoster f ->
        { model with roster = model.roster |> f }
let init _ =
    Model.fresh


