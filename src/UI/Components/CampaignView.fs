module UI.Components.CampaignView
open Fable.Core
open Feliz
open UI.Components.Campaign
open Domain.Data
open Domain.Campaign

[<JSX.Component>]
let Campaign() =
    let (state: Model), dispatch = React.useElmishSimple init update
    let parse = Packrat.parser Domain.Parser.(|Creature|_|)
    let n, setN = React.useState 1
    let render = (fun r -> JSX.jsx """<li><b>{r.personalName}</b></li>""")
    let addToRoster (lst: Roster) =
        setN (n+1)
        lst@[CharacterSheet.create($"Bob {n}", (parse $"Bob: ST {10 + rand 8}"))]
    let onClick = (fun _ -> addToRoster |> ChangeRoster |> dispatch)
    let hello = JSX.jsx "<fragment>hello</fragment>"
    JSX.jsx $"""
        <div>Hello placeholder
            <div>Roster: <ul>{state.roster |> List.map render}</ul></div>
            <button onClick={onClick}>Add</button>
        </div>
        """
