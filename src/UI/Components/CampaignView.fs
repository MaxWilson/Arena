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
let PartyPicker (model:Model, teamNumber, title, back, next, dispatch) =
    let party, setParty = React.useState (model.currentEncounterSetup |> Setup.enumerateMembers |> List.choose (function (_, _, otherTeamNumber, Individual char ) when otherTeamNumber = teamNumber -> Some (Individual char) | _ -> None))
    let filter, setFilter = React.useStateWithDependencies (thunk "") model.roster
    let (draft: string option), setDraft = React.useState None
    let isCharacter id = function
        | Individual char when char.id = id -> true
        | _ -> false
    let toggle charSheet nowChecked =
        if not nowChecked then
            setParty (party @ [Individual charSheet])
        else setParty (party |> List.filter (isCharacter charSheet.id >> not))
    let enemies =
        model.currentEncounterSetup |> Setup.enumerateMembers
        |> List.choose (function (_, _, otherTeamNumber, Individual { id = id }) when otherTeamNumber <> teamNumber -> Some id | _ -> None)
    let readyToSubmit = party.Length > 0
    let submit _ =
        if readyToSubmit then
            let group = {
                members = (teamNumber, party)
                radius = None
                center = coords(0., 0.) // we'll change this later, during Setup mode
                }
            dispatch (ChangeSetup (fun setup -> (setup |> List.filter (fun { members = (teamNumber', _) } -> teamNumber' <> teamNumber)) @ [group]))
            next()
    let partyDisplay = Html.div [
        let partyRows = [
            let exceptEnemies lst =
                lst |> List.filter (fun char -> enemies |> List.contains char.id |> not)
            for r in model.roster |> exceptEnemies do
                let id = $"chk_{r.id}"
                let isChecked = party |> List.exists (isCharacter r.id)
                if isChecked || r.rp.personalName.StartsWith(filter, System.StringComparison.InvariantCultureIgnoreCase) then
                    classP' "partyRow" Html.tr [
                        prop.key id
                        prop.children (List.map (fun (d:ReactElement) -> Html.td d) [
                            Html.input [prop.type'.checkbox; prop.id id; prop.isChecked isChecked; prop.onChange (fun (_:Browser.Types.Event) -> toggle r isChecked)]
                            Html.label [prop.htmlFor id; prop.text r.rp.personalName]
                            classP' "chooseParty" Html.textarea [prop.valueOrDefault r.draft; prop.disabled true]
                            ])
                        ]
                ]
        class' "partyDisplay" Html.table [
            Html.tbody partyRows
            ]
        Html.div [
            Html.button [prop.text "Back"; prop.onClick (defaultArg back ignore); if Option.isNone back then prop.disabled true]
            Html.button [prop.text "OK"; prop.onClick submit; if not readyToSubmit then prop.disabled true]
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
    | None ->
        // we're tentatively using JSX for layout-heavy stuff, mostly just to see if it will work but also to see if it's easier to read
        let tryAutoAcceptFiltered (e:Browser.Types.KeyboardEvent) =
            if e.key = "Enter" then
                if e.ctrlKey then
                    submit()
                else
                match model.roster |> (List.filter
                    (fun r -> r.rp.personalName.StartsWith(filter, System.StringComparison.InvariantCultureIgnoreCase))) with
                | [r] ->
                    let isChecked = party |> List.exists (isCharacter r.id)
                    toggle r isChecked
                | _ -> ()
            elif e.key = "Escape" then
                setFilter ""
        let filterInput = Html.input [prop.placeholder "Filter by name"; prop.valueOrDefault filter; prop.onChange (setFilter); prop.onKeyDown tryAutoAcceptFiltered]
        let clearButton = Html.button [prop.text "Clear"; prop.onClick (fun _ -> setFilter ""; if filter = "" then setParty [])] // double click to clear filter AND party
        JSX.jsx $"""
        <div>
            <b>{title}</b>
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

[<ReactComponent>]
let PartyDisplay (title: string, model, teams, dispatch) =
    let getTeam (_,_,team,_) = team
    let teams = model.currentEncounterSetup |> Setup.enumerateMembers |> List.groupBy getTeam
    let describe (team, lst) =
        let txt = (String.oxfordJoin [
            for address, group, team, entry in lst do
                match entry with
                | Individual char -> char.rp.personalName
                | Group (quantity, name) ->
                    let m = model.monsters.catalog[name]
                    (m.Quantify(quantity))
            ])
        classTxt' (if team = 0 then "teamBlue" else "teamPurple") Html.span txt
    class' "partyDisplay" Html.div [
        Html.b [
            Html.text title
            ]
        for (team, lst) in teams do
            if team > 0 then Html.b " vs. "
            describe (team, lst)
        ]

let SetupView (model: Model, back, submit, dispatch) =
    let notifyTeamMoved ((groupIx, _subgroupIx), (x: float<yards>, y: float<yards>)) =
        let x, y = Ops.round x, Ops.round y
        let moveGroup (setup: Setup) =
            setup
            |> List.mapi (fun ix group ->
                if ix = groupIx then
                    { group with center = (x, y ) }
                else group)
        ChangeSetup moveGroup |> dispatch
    let db = model.monsters
    let groups = [
        for address, group, teamNumber, members' in Setup.enumerateMembers model.currentEncounterSetup do
            match members' with
            | Individual char -> address, teamNumber, (char: CharacterSheet).rp.personalName, group.center, 0.5<yards>
            | Group(quantity, name) ->
                let m = db.catalog[name]
                let memberCount = members' |> (function Individual _ -> 1 | Group (quantity, _) -> quantity)
                address, teamNumber, m.Quantify quantity, group.center, Domain.CombatRules.radius_ (group, thunk memberCount)
        ]
    Html.div [
        ArenaView.Setup(groups, notifyTeamMoved, dispatch)
        Html.div [
            Html.button [prop.text "Back"; prop.onClick back]
            Html.button [prop.text "OK"; prop.onClick submit]
            ]
        ]


[<ReactComponent>]
let Campaign(header: ReactElement) =
    let (model: Model), dispatch = React.useElmishSimple init update
    let n, setN = React.useState 1
    let goto mode _ = dispatch (SetMode mode)
    class' "campaign" Html.div [
        header
        Html.h4 [Html.i "(Under construction)"]
        match model.mode with
        | ChooseParty ->
            PartyPicker(model, 0, "Choose Your Party", None, goto ChooseOpposition, dispatch)
        | ChooseOpposition ->
            PartyDisplay("Party: ", model, [0], dispatch)
            PartyPicker(model, 1, "Choose Enemies", Some (goto ChooseParty), goto SetupEncounter, dispatch)
        | SetupEncounter ->
            PartyDisplay("Party: ", model, [0;1], dispatch)
            SetupView(model, goto ChooseOpposition, goto Adventure, dispatch)
        | _ ->
            JSX.jsx $"""
                <div>Campaign
                    TODO: {model.mode.ToString()} mode
                    <button onClick={goto SetupEncounter}>Back</button>
                </div>
                """
            |> React.ofJsx
        ]
