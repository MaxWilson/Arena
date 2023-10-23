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
                Html.label [prop.for' id; prop.text r.personalName]
                Html.textarea [prop.valueOrDefault r.draft; prop.disabled true]
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
        |> React.toJsx
    | None ->
    // we're tentatively using JSX for layout-heavy stuff, mostly just to see if it will work but also to see if it's easier to read
        let randomName() = ["Bob"; "Lea"; "Lyron"; "Mortimer"; "Sally"; "Samantha"; "Sven"; "Tyrone"] |> chooseRandom
        let newPC _ = setDraft (Some $"{randomName()}: ST 10")
        JSX.jsx """
        <div>
            <b>Party picker</b>
            <div>
                <input type="text" placeholder="Filter by name" />
                <button onClick={newPC}>New PC</button>
                <button>New monster</button>
                <button>Clear</button>
                {partyDisplay}
            </div>
        </div>
        """

[<JSX.Component>]
let Campaign() =
    let (state: Model), dispatch = React.useElmishSimple init update
    let n, setN = React.useState 1
    class' "campaign" Html.div [
        match state.mode with
        | PartyPicking ->
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
                    {PartyPicker state 0 dispatch}
                    <div>Roster:</div>
                    <ul>{state.roster |> List.map render}</ul>
                    <button onClick={onClick}>Add</button>
                </fragment>
                """
        |> React.ofJsx
        ]
