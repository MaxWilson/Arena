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
let PartyPicker (model:Model) teamNumber dispatch =
    let party, setParty = React.useState []
    let (draft: string option), setDraft = React.useState None
    let isCharacter id = function
        | Individual char when char.id = id -> true
        | _ -> false
    let partyDisplay = [
        for r in model.roster do
            let id = $"chk_{r.id}"
            let isChecked = party |> List.exists (isCharacter r.id)
            let toggle nowChecked =
                if nowChecked then
                    setParty (party @ [Individual r])
                else setParty (party |> List.filter (isCharacter r.id >> not))
            Html.div [
                Html.input [prop.type'.checkbox; prop.id id; prop.isChecked isChecked; prop.onChange toggle]
                Html.label [prop.htmlFor id; prop.text r.personalName]
                Html.input [prop.valueOrDefault r.draft; prop.disabled true]
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
    match draft with
    | Some draft ->
        Html.form [
            let char =
                match Packrat.ParseArgs.Init draft with
                | Domain.Parser.Character (char, Packrat.End) -> Some char
                | _ -> None
            let submit() =
                match char with
                | Some char ->
                    let char = CharacterSheet.create char.name char
                    dispatch (ChangeRoster (fun roster -> roster @ [char]))
                    setDraft None
                | None -> ()
            prop.onSubmit (fun ev ->
                submit()
                ev.preventDefault()
                )
            if char.IsNone then
                prop.className "error"
            prop.children [
                Html.textarea [prop.valueOrDefault draft; prop.onChange (Some >> setDraft); prop.onKeyDown(fun e -> if e.ctrlKey && e.key = "Enter" then submit())]
                Html.div [
                    Html.button [prop.text "Cancel"; prop.onClick (fun _ -> setDraft None)]
                    Html.button [prop.type'.submit; prop.text "Save"; if char.IsNone then prop.disabled true]
                    ]
                ]
            ]
    | None ->
        let randomName() = ["Bob"; "Lea"; "Lyron"; "Mortimer"; "Sally"; "Samantha"; "Sven"; "Tyrone"] |> chooseRandom
        let newPC setDraft _ = setDraft (Some $"{randomName()}: ST 10")

        Html.div [
            Html.b "Party picker"
            Html.div [
                Html.input [prop.type'.text; prop.placeholder "Filter by name"]
                Html.button [prop.onClick (newPC setDraft); prop.text "New PC"]
                Html.button [prop.text "New monster"]
                Html.button [prop.text "Clear"]
                yield! partyDisplay
                ]
            ]

[<JSX.Component>]
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
            let parse = Packrat.parser Domain.Parser.(|Creature|_|)
            let render = (fun r -> JSX.jsx """<li><b>{r.personalName}</b></li>""")
            let addToRoster (lst: Roster) =
                setN (n+1)
                lst@[CharacterSheet.create $"Bob {n}" (parse $"Bob: ST {10 + rand 8}")]
            let onClick = (fun _ -> addToRoster |> ChangeRoster |> dispatch)
            JSX.jsx $"""
                <fragment>Campaign
                    <div>Roster:</div>
                    <ul>{state.roster |> List.map render}</ul>
                    TODO: {state.mode.ToString()} mode
                </fragment>
                """
            |> React.ofJsx
        ]
