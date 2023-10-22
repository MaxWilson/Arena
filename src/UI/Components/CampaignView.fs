module UI.Components.CampaignView
open Fable.Core
open Feliz
open UI.Components.Campaign
open Domain.Data
open Domain.Campaign

// [<ReactComponent>]
// let MonsterPicker (model:Model) =
//     let namePrefix, setNamePrefix = React.useState ""
//     Html.div [
//         Html.input [prop.placeholder "Monster name"; prop.valueOrDefault namePrefix; prop.onChange setNamePrefix]
//         classP' "newButton" Html.button [prop.text "New"; prop.onClick(thunk1 dispatch (SetPage (Editing "")))]
//         classP' "clearButton" Html.button [prop.text "Clear"; prop.onClick(fun _ -> setNamePrefix ""; dispatch (Clear side))]
//         let (Setup setup) = model.currentEncounterSetup
//         for teamNumber, group in setup do
//             match group with
//             | Individual _ -> notImpl()
//             | Group group ->
//                 for quantity, name in group.members do
//                     Html.div [
//                         Html.button [prop.text "+"; prop.onClick (fun _ -> dispatch (ChangeFightSetup (changeQuantity (ix, name) +1)))]
//                         Html.button [prop.text "-"; prop.onClick (fun _ -> dispatch (ChangeFightSetup (changeQuantity (ix, name) -1)))]
//                         editLink (Some quantity) name (fun q -> dispatch (ChangeFightSetup (setQuantity (ix, name) q)))
//                         ]
//         if namePrefix.Length > 0 || noMonstersSelectedYet then
//             let matchingNames = db.catalog.Keys |> Seq.filter (fun name -> name.StartsWith(namePrefix, System.StringComparison.InvariantCultureIgnoreCase)) |> List.ofSeq
//             for name in matchingNames |> List.take (min 10 matchingNames.Length) do
//                 Html.div [
//                     Html.button [prop.text clickLabel; prop.onClick(fun _ -> onClick name)]
//                     Html.text name
//                     ]
//         ]


[<JSX.Component>]
let SideView teamNumber dispatch =
    JSX.jsx """
    <div>

    </div>
    """

[<JSX.Component>]
let PartyPicker teamNumber dispatch =
    JSX.jsx """
    <div>
        <b>Party picker</b>
        <div>
            <input type="text" placeholder="Filter by name" />
            <button>New PC</button>
            <button>New monster</button>
            <button>Clear</button>
        </div>
    </div>
    """

[<JSX.Component>]
let Campaign() =
    let (state: Model), dispatch = React.useElmishSimple init update
    let n, setN = React.useState 1
    match state.mode with
    | PartyPicking ->
        PartyPicker 0 dispatch
    | _ ->
        let parse = Packrat.parser Domain.Parser.(|Creature|_|)
        let render = (fun r -> JSX.jsx """<li><b>{r.personalName}</b></li>""")
        let addToRoster (lst: Roster) =
            setN (n+1)
            lst@[CharacterSheet.create($"Bob {n}", (parse $"Bob: ST {10 + rand 8}"))]
        let onClick = (fun _ -> addToRoster |> ChangeRoster |> dispatch)
        JSX.jsx $"""
            <fragment>Campaign
                {PartyPicker 0 dispatch}
                <div>Roster:</div>
                <ul>{state.roster |> List.map render}</ul>
                <button onClick={onClick}>Add</button>
            </fragment>
            """
