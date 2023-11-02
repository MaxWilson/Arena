module UI.Components.ChargenView
open Fable.Core
open Domain.Data
open Domain.Campaign
open Domain.Random
open Feliz
open Feliz.Listeners

module private Impl =
    open Domain.Character
    type Profession = Swashbuckler | Knight | Druid | Cleric | Wizard
        with static member All = [Swashbuckler; Knight; Druid; Cleric; Wizard]
    type CharacterSheet2 = {
        rp: RoleplayingData
        profession: Profession
        }
        with static member fresh = { rp = Generate.randomly(); profession = chooseRandom Profession.All }
    type Model = {
        character: CharacterSheet2
        }
    let update msg model = model
    let init _ = { character = CharacterSheet2.fresh }
open Impl
[<ReactComponent>]
let View(settings, header) =
    let state, dispatch = React.useElmishSimple init update
    Html.div [
        header
        Html.div "Chargen placeholder"
        ]
