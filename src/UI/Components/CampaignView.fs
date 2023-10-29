module UI.Components.CampaignView
open Fable.Core
open UI.Components.Campaign
open Domain.Data
open Domain.Campaign
open Domain.Random
open Feliz
open Feliz.Listeners

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

[<ReactComponent>]
let PartyPicker (model:Model) teamNumber dispatch =
    let party, setParty = React.useState []
    let filter, setFilter = React.useStateWithDependencies (thunk "") model.roster
    let (draft: string option), setDraft = React.useState None
    let isCharacter id = function
        | Individual char when char.id = id -> true
        | _ -> false
    let partyDisplay = Html.div [

        let partyRows = [
            for r in model.roster do
                let id = $"chk_{r.id}"
                let isChecked = party |> List.exists (isCharacter r.id)
                if isChecked || r.rp.personalName.StartsWith(filter, System.StringComparison.InvariantCultureIgnoreCase) then
                    let toggle nowChecked =
                        if nowChecked then
                            setParty (party @ [Individual r])
                        else setParty (party |> List.filter (isCharacter r.id >> not))
                    classP' "partyRow" Html.tr [
                        prop.key id
                        prop.children (List.map (fun (d:ReactElement) -> Html.td d) [
                            Html.input [prop.type'.checkbox; prop.id id; prop.isChecked isChecked; prop.onChange toggle]
                            Html.label [prop.htmlFor id; prop.text r.rp.personalName]
                            classP' "chooseParty" Html.textarea [prop.valueOrDefault r.draft; prop.disabled true]
                            ])
                        ]
                ]
        class' "partyDisplay" Html.table [
            Html.tbody partyRows
            ]
        if party.Length > 0 then
            Html.div [
                let start _ =
                    let group = {
                        members = (teamNumber, party)
                        radius = None
                        center = coords(0., 0.) // we'll change this later, during Setup mode
                        }
                    dispatch (ChangeSetup (fun setup -> setup @ [group]))
                    dispatch (SetMode ChooseOpposition)
                Html.button [prop.text "Start"; prop.onClick start]
                ]
        ]
    // React says you have to call the same hooks no matter what is rendered, so we ensure this
    let withListener listeners body =
        React.useListener.onKeyDown(fun ev ->
            match listeners with
            | None -> ()
            | Some (save, cancel) ->
                if ev.key = "Escape" then ev.preventDefault(); cancel()
                elif ev.key = "s" && ev.ctrlKey then ev.preventDefault(); save())
        body
    match draft with
    | Some draft ->
        let char =
            match Packrat.ParseArgs.Init draft with
            | Domain.Character.Parser.CharacterSheet (char, Packrat.End) -> Some char
            | _ -> None
        let submit() =
            match char with
            | Some char ->
                dispatch (ChangeRoster (fun roster -> roster @ [char]))
                setDraft None
            | None -> ()
        let cancel _ = setDraft None
        Html.div [
            if char.IsNone then
                prop.className "error"
            prop.children [
                classP' "chargen" Html.textarea [prop.valueOrDefault draft; prop.onChange (Some >> setDraft); prop.onKeyDown(fun e -> if e.ctrlKey && e.key = "Enter" then submit())]
                Html.div [
                    Html.button [prop.text "Regenerate"; prop.onClick (newPC >> Some >> setDraft)]
                    Html.button [prop.text "Cancel"; prop.onClick cancel]
                    Html.button [prop.text "Save"; prop.onClick (thunk1 submit ()); if char.IsNone then prop.disabled true]
                    ]
                ]
            ]
        |> withListener (Some(submit, cancel))
    | None ->
        // we're tentatively using JSX for layout-heavy stuff, mostly just to see if it will work but also to see if it's easier to read
        let filterInput = Html.input [prop.placeholder "Filter by name"; prop.valueOrDefault filter; prop.onChange (setFilter)]
        let clearButton = Html.button [prop.text "Clear"; prop.onClick (fun _ -> setFilter ""; if filter = "" then setParty [])] // double click to clear filter AND party
        JSX.jsx $"""
        <div>
            <b>Party picker</b>
            <div>
                {filterInput}
                <button onClick={newPC >> Some >> setDraft}>New PC</button>
                <button>New monster</button>
                {clearButton}
                {partyDisplay}
            </div>
        </div>
        """
        |> React.ofJsx
        |> withListener None

[<ReactComponent>]
let Campaign(header: ReactElement) =
    let (state: Model), dispatch = React.useElmishSimple init update
    let n, setN = React.useState 1
    class' "campaign" Html.div [
        header
        Html.h4 [Html.i "(Under construction)"]
        match state.mode with
        | ChooseParty ->
            PartyPicker state 0 dispatch
        | _ ->
            JSX.jsx $"""
                <div>Campaign
                    TODO: {state.mode.ToString()} mode
                </div>
                """
            |> React.ofJsx
        ]
