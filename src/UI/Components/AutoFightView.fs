module UI.Components.AutoFightView
open Fable.Core
open Feliz
open Elmish
open Fable.Core.JsInterop
open Domain
open Domain.CombatRules
open Domain.Random
open Domain.Random.Parser
open Feliz.Listeners
open UI.Components.AutoFight
open UI.Data

[<ReactComponent>]
let MonsterPicker (db: MonsterDatabase, noMonstersSelectedYet) (clickLabel: string, onClick, side, dispatch) (monsterDetails: ReactElement) =
    let namePrefix, setNamePrefix = React.useState ""
    Html.div [
        Html.input [prop.placeholder "Monster name"; prop.valueOrDefault namePrefix; prop.onChange setNamePrefix]
        classP' "newButton" Html.button [prop.text "New"; prop.onClick(thunk1 dispatch (SetPage (Editing "")))]
        classP' "clearButton" Html.button [prop.text "Clear"; prop.onClick(fun _ -> setNamePrefix ""; dispatch (Clear side))]
        monsterDetails
        if namePrefix.Length > 0 || noMonstersSelectedYet then
            let matchingNames = db.catalog.Keys |> Seq.filter (fun name -> name.StartsWith(namePrefix, System.StringComparison.InvariantCultureIgnoreCase)) |> List.ofSeq
            for name in matchingNames |> List.take (min 10 matchingNames.Length) do
                Html.div [
                    Html.button [prop.text clickLabel; prop.onClick(fun _ -> onClick name)]
                    Html.text name
                    ]
        ]
let private checkbox ctor (label: string) (isChecked, update) =
    ctor [
        let chkid = $"checkbox_{label}"
        Html.label [prop.text label; prop.htmlFor chkid]
        Html.input [prop.type'.checkbox; prop.id chkid; prop.isChecked isChecked; prop.readOnly true; prop.onCheckedChange update]
        ]
let private labeled (label: string) (inner: ReactElement) =
    Html.div [
        Html.text label
        inner
        ]
[<ReactComponent>]
let EditData<'t> (propType: IReactProperty, render: 't -> string, parser: string -> 't option) (label:string) (hint: 't) (value: 't option, update: 't option -> unit) =
    // This is kind of a hack, but we use Editing to keep track of whether we want to use the live view or the text view
    let editing, setEditing = React.useState false
    let liveValue = match value with Some v -> render v | None -> ""
    let txt, updateTxt = React.useState liveValue
    labeled label <| Html.input [
        prop.valueOrDefault (if editing then txt else liveValue)
        prop.placeholder (render hint)
        propType
        if propType = prop.type'.number then
            prop.max 99
        prop.onChange updateTxt
        prop.onFocus (fun _ -> setEditing true)
        prop.onBlur (fun _ ->
            let v = parser txt
            update v
            setEditing false
            match v with
            | Some v -> updateTxt (render v)
            | None -> updateTxt "")
        ]

[<ReactComponent>]
let EditDataNoHint<'t> (propType: IReactProperty, render: 't -> string, parser: string -> 't option) (label:string) (value: 't option, update: 't option -> unit) =
    // This is kind of a hack, but we use Editing to keep track of whether we want to use the live view or the text view
    let editing, setEditing = React.useState false
    let liveValue = match value with Some v -> render v | None -> ""
    let txt, updateTxt = React.useState liveValue
    labeled label <| Html.input [
        prop.valueOrDefault (if editing then txt else liveValue)
        propType
        if propType = prop.type'.number then
            prop.max 99
        prop.onChange updateTxt
        prop.onFocus (fun _ -> setEditing true)
        prop.onBlur (fun _ ->
            let v = parser txt
            update v
            setEditing false
            match v with
            | Some v -> updateTxt (render v)
            | None -> updateTxt "")
        ]

[<ReactComponent>]
let EditDropdown<'t> (render: 't -> string, parser: string -> 't option) (label:string) (defaultOption: string option) (value: 't option, options, update: 't option -> unit) =
    labeled label <| Html.select [
        match value, defaultOption with
        | Some value, _ ->
            prop.valueOrDefault (render value)
        | _, Some hint ->
            prop.valueOrDefault hint
        | _ -> ()
        prop.children [
            match defaultOption with
            | Some v when not (options |> List.exists (fun o -> render o = v)) -> Html.option [prop.text v; prop.value v]
            | _ -> ()
            for option in options do
                Html.option [prop.text (render option); prop.value (render option)]
            ]
        prop.onChange (parser >> update)
        ]
[<ReactComponent>]
let EditDamage (label:string) (stats: Stats) update =
    // This is kind of a hack, but we use Editing to keep track of whether we want to use the live view or the text view
    let editing, setEditing = React.useState false
    let render = toString
    let liveValue = match stats.Damage with Some v -> render v | None -> ""
    let txt, updateTxt = React.useState liveValue
    class' "editDamage" Html.div [
        Html.text label
        Html.input [
            prop.valueOrDefault (if editing then txt else liveValue)
            prop.placeholder (toString stats.Damage_)
            prop.onFocus (fun _ -> setEditing true)
            prop.onChange updateTxt
            prop.onBlur (fun _ ->
                match Packrat.ParseArgs.Init txt with
                | Domain.Parser.DamageOverall((dmg, dtype), Packrat.End) ->
                    let stats' =
                        { stats with Damage = Some dmg; DamageType = dtype |> Option.orElse stats.DamageType }
                    update stats'
                    setEditing false
                    updateTxt (dmg |> render)
                | _ ->
                    let stats' =
                        { stats with Damage = None }
                    update stats'
                    setEditing false
                    updateTxt ""
                )
            ]
        match stats.Damage with
        | None | Some (Explicit _) -> () // if it's placeholdered or explicitly set then showing again would be redundant
        | _ -> Html.text $"({stats.Damage_ |> toString})"
        ]

[<ReactComponent>]
let EditView (name: string) (db: MonsterDatabase) dispatch =
    let stats = (db.catalog |> Map.tryFind name |> Option.defaultValue (Stats.create name))
    let stats, setStats = React.useState stats
    let textView, setTextView = React.useState (stats.ToString())
    let textError, setTextError = React.useState false
    let update = fun (stats: Stats) ->
        setTextView (stats.ToString())
        setStats stats

    let editString = EditData<string>(prop.type'.text, toString, (fun (txt: string) -> if String.isntWhitespace txt then Some txt else None))
    let editNumber = EditData(prop.type'.number, toString, (fun (input: string) -> match System.Int32.TryParse input with true, n -> Some n | _ -> None))
    let editNumberNoHint = EditDataNoHint(prop.type'.number, toString, (fun (input: string) -> match System.Int32.TryParse input with true, n -> Some n | _ -> None))
    let editDecimalNumber = EditData(prop.type'.number, (fun v -> $"%.2f{v}"), (fun (input: string) -> match System.Double.TryParse input with true, n -> Some n | _ -> None))
    let editRollSpec = EditDataNoHint<RandomThrow>(prop.type'.text, toString, (fun (input: string) -> match Packrat.ParseArgs.Init input with Domain.Random.Parser.Roll(r, Packrat.End) -> Some r | _ -> None))
    let editDamageType = EditDropdown(toString, (fun (input: string) -> match Packrat.ParseArgs.Init input with Domain.Parser.DamageType (r, Packrat.End) -> Some r | _ -> None))
    let editInjuryTolerance = EditDropdown(toString, (function "Unliving" -> Some Unliving | "Homogeneous" -> Some Homogeneous  | "Diffuse" -> Some Diffuse | _ -> None))
    let editBerserkLevel = EditDropdown(SelfControlLevel.toDescription, (fun (input: string) -> [Mild; Moderate; Serious; Severe; Always] |> List.tryFind (fun lvl -> SelfControlLevel.toDescription lvl = input)))
    let editBool label (value: bool, update) = checkbox Html.div label (value, update)
    class' "editView" Html.div [
        classP' ("textview" + if textError then " error" else "") Html.textarea [
            prop.valueOrDefault textView
            prop.onChange (fun (input: string) ->
                match Packrat.ParseArgs.Init input with
                | Domain.Parser.Creature(creature, Packrat.End) ->
                    setTextView input
                    setTextError false
                    setStats creature
                | _ ->
                    setTextView input
                    setTextError true
                )
            ]
        editString "Name" "" (Some stats.name, (fun txt -> { stats with name = defaultArg txt "" } |> update))
        editString "Pluralized" (stats.name + "s") (stats.pluralName, (fun txt -> { stats with pluralName = txt } |> update))
        editNumber "ST" stats.ST_ (stats.ST, (fun n -> { stats with ST = n } |> update))
        editNumber "DX" stats.DX_ (stats.DX, (fun n -> { stats with DX = n } |> update))
        editNumber "IQ" stats.IQ_ (stats.IQ, (fun n -> { stats with IQ = n } |> update))
        editNumber "HT" stats.HT_ (stats.HT, (fun n -> { stats with HT = n } |> update))
        editNumber "DR" stats.DR_ (stats.DR, (fun n -> { stats with DR = n } |> update))
        editInjuryTolerance "Injury Tolerance" (Some "Normal") (stats.InjuryTolerance, [Unliving; Homogeneous; Diffuse], (fun v -> { stats with InjuryTolerance = v } |> update))
        editNumber "HP" stats.HP_ (stats.HP, (fun n -> { stats with HP = n } |> update))
        editBool "Unnaturally fragile" (stats.UnnaturallyFragile, (fun b -> { stats with UnnaturallyFragile = b } |> update))
        editBool "High Pain Threshold" (stats.HighPainThreshold, (fun b -> { stats with HighPainThreshold = b } |> update))
        editBool "Immune to shock, stun, unconsciousness" (stats.SupernaturalDurability, (fun b -> { stats with SupernaturalDurability = b } |> update))
        editDecimalNumber "Speed" stats.Speed_ (stats.Speed, (fun n -> { stats with Speed = n } |> update))
        editNumber "Move" stats.Move_ (stats.Move, (fun n -> { stats with Move = n } |> update))
        editNumberNoHint "Parry" (stats.Parry, (fun n -> { stats with Parry = n } |> update))
        editBool "Fencing Parry" (stats.FencingParry, (fun b -> { stats with FencingParry = b } |> update))
        editNumber "Dodge" stats.Dodge_ (stats.Dodge, (fun n -> { stats with Dodge = n } |> update))
        editNumberNoHint "Block" (stats.Block, (fun n -> { stats with Block = n } |> update))
        editNumber "Weapon Skill" stats.WeaponSkill_ (stats.WeaponSkill, (fun n -> { stats with WeaponSkill = n } |> update))
        editBool "Weapon Master" (stats.WeaponMaster, (fun b -> { stats with WeaponMaster = b } |> update))
        EditDamage "Damage" stats update
        editDamageType "Damage type" (Some "Other") (stats.DamageType, [Crushing; Cutting; Piercing; Impaling; Burning; Other], (fun v -> { stats with DamageType = v } |> update))
        editRollSpec "Followup damage" (stats.FollowupDamage, (fun r -> { stats with FollowupDamage = r } |> update))
        if stats.FollowupDamage.IsSome then
            editDamageType "Followup type" None (stats.FollowupDamageType, [Crushing; Cutting; Piercing; Impaling; Burning; Other], (fun v -> { stats with FollowupDamageType = v } |> update))
        else Html.div []
        editBool "Use Rapid Strike" (stats.UseRapidStrike, (fun b -> { stats with UseRapidStrike = b } |> update))
        editBool "Cannot Be Parried" (stats.CannotBeParried, (fun b -> { stats with CannotBeParried = b } |> update))
        editNumber "Extra Attacks" stats.ExtraAttack_ (stats.ExtraAttack, (fun n -> { stats with ExtraAttack = n } |> update))
        editNumber "Extra Parries" stats.ExtraParry_ (stats.ExtraParry, (fun n -> { stats with ExtraParry = n } |> update))
        editNumber "Altered Time Rate" stats.AlteredTimeRate_ (stats.AlteredTimeRate, (fun n -> { stats with AlteredTimeRate = n } |> update))
        editBerserkLevel "Berserk" (Some "") (stats.Berserk, [Mild; Moderate; Serious; Severe; Always], (fun v -> { stats with Berserk = v } |> update))
        class' "buttons" Html.div [
            let cancel _ = dispatch (SetPage Fight)
            let save _ = dispatch (Upsert stats); dispatch (SetPage Fight)
            let noSave = (stats.name |> System.String.IsNullOrWhiteSpace)
            Html.button [prop.text "Cancel"; prop.onClick cancel]
            Html.button [prop.text "OK"; prop.onClick save; prop.disabled noSave]
            React.useListener.onKeyDown(fun ev ->
                if ev.key = "Escape" then ev.preventDefault(); cancel()
                elif ev.key = "s" && ev.ctrlKey && not noSave then ev.preventDefault(); save()
                )
            ]
        ]

[<ReactComponent>]
let ViewCombat (settings: Settings, setup, combatLog: CombatLog) dispatch =
    let showRolls, setShowRolls = React.useState true
    // we want to reinitialize combat if and only if combatLog changes, instead of never reinitializing it. I.e. when we run a new fight we should wipe the old display and start over
    let combatLogEntry, setCombatLogEntry = React.useStateWithDependencies (fun () -> combatLog |> List.last) combatLog
    let logEntry, combat = combatLogEntry
    let currentIndex, setCurrentIndex = React.useState 0
    class' "combat" Html.div [
        if settings.battlegridDisplaySize <> Zero then
            class' "visuals" Html.div [ArenaView.Actual (settings, combat.combatants, logEntry, combat.geo) dispatch]
        if settings.statsTableDisplaySize <> Zero then
            class' ("statusTable" + if settings.statsTableDisplaySize = Big then " big" else "") Html.div [
                Html.table [
                    Html.thead [
                        Html.tr [
                            Html.th "Name"
                            Html.th "HP"
                            Html.th "Condition"
                            ]
                        ]
                    Html.tbody [
                        for c in combat.combatants.Values |> Seq.sortBy(fun c -> c.team, c.number) do
                            Html.tr [
                                prop.key c.personalName
                                prop.className (if c.team = 1 then "teamBlue" else "teamPurple")
                                prop.children [
                                    Html.td c.personalName
                                    Html.td c.CurrentHP_
                                    Html.td [
                                        if c.is Dead then
                                            prop.className "statusDead"
                                            prop.text "Dead"
                                        elif c.is Unconscious then
                                            prop.className "statusDead"
                                            prop.text "Unconscious"
                                        else
                                            match c.statusMods with
                                            | [] ->
                                                prop.className "statusOk"
                                                prop.text "OK"
                                            | mods ->
                                                let txt: string = mods |> List.distinct |> List.map toString |> List.sort |> String.join ", "
                                                prop.className "statusDisabled"
                                                prop.text txt
                                        ]
                                    ]
                                ]
                        ]
                    ]
                ]
        class' "logButtons" Html.div [
            let setIndex gotoNearest newIndex _ =
                if newIndex >= 0 && newIndex < combatLog.Length then
                    setCurrentIndex newIndex
                    setCombatLogEntry (combatLog.[newIndex])
                    // set focus to the newly-selected row
                    let log = (Browser.Dom.document.getElementsByClassName "logEntries")[0]
                    let entry =
                        log.childNodes[newIndex]
                    if gotoNearest then scrollIntoView entry else
                        scrollSectionIntoView entry
                        let logButtons = ((Browser.Dom.document.getElementsByClassName "logButtons")[0])
                        scrollIntoView logButtons // always make sure the buttons are visible after scrolling even on a small mobile screen
            let changeIndex delta =
                let newIndex = currentIndex + delta
                setIndex true newIndex
            let priorRound _ =
                match combatLog
                        |> List.mapi Tuple2.create
                        |> List.tryFindIndexBack (function (ix, ((None | Some (NewRound _)), _)) when ix < currentIndex -> true | _ -> false)
                        with
                | Some ix -> setIndex false ix ()
                | None -> setIndex false 0 () // should only happen when we're already at the front
            let nextRound _ =
                match combatLog
                        |> List.mapi Tuple2.create
                        |> List.tryFindIndex (function (ix, (Some (NewRound _), _)) when (currentIndex = 0 || ix > currentIndex) -> true | _ -> false)
                        with
                | Some ix -> setIndex false ix ()
                | None -> setIndex false (combatLog.Length - 1) ()
            Html.button [prop.text "<<"; prop.onClick priorRound]
            Html.button [prop.text "<"; prop.onClick (changeIndex -1)]
            Html.button [prop.text ">"; prop.onClick (changeIndex +1)]
            Html.button [prop.text ">>"; prop.onClick nextRound]
            React.useWindowListener.onKeyDown(fun ev ->
                if Browser.Dom.window.document.activeElement.tagName <> "INPUT" then // this is a kludge but we don't want to prevent user from moving around inside an input element (e.g. setting how many peshkalis)
                    if ev.key = ">" || ev.key = "ArrowRight" then ev.preventDefault(); nextRound()
                    elif ev.key = "<" || ev.key = "ArrowLeft" then ev.preventDefault(); priorRound()
                    elif ev.key = "ArrowDown" then ev.preventDefault(); changeIndex +1 ()
                    elif ev.key = "ArrowUp" then ev.preventDefault(); changeIndex -1 ()
                )
            checkbox Html.span "Show rolls" (showRolls, setShowRolls)
            ]
        if settings.logEntriesDisplaySize <> Zero then
            class' ("logEntries" + if settings.logEntriesDisplaySize = Big then " big" else "") Html.div [
                for (ix, (msg, state)) in combatLog |> List.mapi Tuple2.create do
                    let header (txt:string) = Html.h3 [prop.text txt; prop.onClick (fun _ -> setCurrentIndex ix; setCombatLogEntry (msg, state)); if ix = currentIndex then prop.className "selected"]
                    let div (children: ReactElement list) = Html.div [prop.children children; prop.onClick (fun _ -> setCurrentIndex ix; setCombatLogEntry (msg, state)); if ix = currentIndex then prop.className "selected"]
                    match msg with
                    | None -> header "Combat begins"
                    | Some msg ->
                        let viewDetails details =
                            if showRolls then
                                classTxt' "details" Html.span $" {details}"
                            else React.fragment []
                        let name = function
                            | (1, name) -> classTxt' "blueName" Html.span name
                            | (2, name) -> classTxt' "purpleName" Html.span name
                            | _ -> shouldntHappen()
                        let miss = Html.img [prop.ariaLabel "Miss"; prop.src "img/shield_16x16.png"] // reuse the "defended" icon for misses because that seems to feel better than using a sword for misses.
                        let defended = Html.img [prop.ariaLabel "Defended"; prop.src "img/shield_16x16.png"]
                        let regularHit = Html.img [prop.ariaLabel "Hit"; prop.src "img/sword_16x16.png"]
                        let bigHit = Html.img [prop.ariaLabel "Big Hit"; prop.src "img/crossedswords_16x16.png"]
                            //<img aria-label="🗡️" src="/assets/47f10f1fb3beec3810f0f37cf4cccd95.svg" alt="🗡️" draggable="false" class="emoji" data-type="emoji" data-name=":dagger:">
                            //<img aria-label="🗡️" src="/assets/47f10f1fb3beec3810f0f37cf4cccd95.svg" alt="🗡️" draggable="false" class="emoji" data-type="emoji" data-name=":dagger:">
                            //<img aria-label="⚔️" src="/assets/e7159ba0fcc85f39f95227dd85f44aeb.svg" alt="⚔️" draggable="false" class="emoji" data-type="emoji" data-name=":crossed_swords:">
                        let hpText hp = classP' "injury" Html.span [prop.text $"{hp} HP"]
                        match msg with
                        | Hit (ids, _, _, injury, statusImpact, rollDetails) ->
                            let hit verb =
                                div [bigHit; name ids.attacker; Html.text $" {verb} "; name ids.target; Html.text $" with a hit for "; hpText injury; viewDetails rollDetails]
                            match statusImpact with
                            | v when v |> List.contains Dead -> hit "kills"
                            | v when v |> List.contains Unconscious -> hit "KOs"
                            | v when v |> List.contains Stunned -> hit "stuns"
                            | v when v |> List.contains Berserk ->
                                div [bigHit; name ids.attacker; Html.text $" drives "; name ids.target; Html.text $" berserk with a hit for "; hpText injury; viewDetails rollDetails]
                            | _ ->
                                div [regularHit; name ids.attacker; Html.text $" hits "; name ids.target; Html.text $" for "; hpText injury; viewDetails rollDetails]
                        | SuccessfulDefense(ids, _, { defense = Parry }, rollDetails) ->
                            div [defended; name ids.attacker; Html.text " attacks "; name ids.target; Html.text " who parries"; viewDetails rollDetails]
                        | SuccessfulDefense(ids, _, { defense = Block }, rollDetails) ->
                            div [defended; name ids.attacker; Html.text " attacks "; name ids.target; Html.text " who blocks"; viewDetails rollDetails]
                        | SuccessfulDefense(ids, _, { defense = Dodge }, rollDetails) ->
                            div [defended; name ids.attacker; Html.text " attacks "; name ids.target; Html.text " who dodges"; viewDetails rollDetails]
                        | Miss (ids, _, rollDetails) ->
                            div [miss; name ids.attacker; Html.text " misses "; name ids.target; viewDetails rollDetails]
                        | FallUnconscious(id, rollDetails) ->
                            div [name id; Html.text " falls unconscious "; viewDetails rollDetails]
                        | Unstun(id, rollDetails) ->
                            div [name id; Html.text " recovers from stun "; viewDetails rollDetails]
                        | StandUp(id, rollDetails) ->
                            div [name id; Html.text " stands up "; viewDetails rollDetails]
                        | Info (id, msg, rollDetails) ->
                            div [name id; Html.text $" {msg}"; viewDetails rollDetails]
                        | MoveTo (id, _, _, _, describe) ->
                            div [name id; Html.text $" {describe}"]
                        | NewRound n ->
                            header $"Round {n} starts"
                ]
        ]

[<ReactComponent>]
let ExecuteButton (model:Model) dispatch =
    match model.execution with
    | InProgress msg ->
        class' "fadeIn" Html.div [
            Html.div (defaultArg msg "Executing...")
            class' "busy" Html.div [
                for _ in 1..10 do
                    class' "wave" Html.div []
                ]
            ]
    | _ ->
        React.useListener.onKeyDown(fun ev ->
            if ev.key = "Enter" && ev.ctrlKey then ev.preventDefault(); beginFights model dispatch
            )
        Html.button [prop.text (if model.execution = NotStarted then "Execute" else "Re-execute"); prop.onClick (thunk2 beginFights model dispatch)]

[<ReactComponent>]
let View (settings, header) (model: Model) dispatch =
    match model.page with
    | Editing name -> EditView name model.database dispatch
    | Fight ->
        Html.div [
            prop.className "homePage"
            prop.children [
                header

                class' "main" Html.div [
                    if model.execution = NotStarted then
                        class' "fightSetup" Html.div [
                            let editLink (quantity: int option) (name: string) (setQuantity: int -> unit) =
                                // for aesthetic and functional reasons, we don't want quantity to be part of the link, so it's a separate HTML element (usually an input element except for calibrated N)
                                let numberTxt, txt =
                                    let creature = model.database.catalog[name]
                                    let numberInput (q: int) =
                                        Html.input [prop.type'.number; prop.valueOrDefault q; prop.onChange setQuantity]
                                    match quantity with
                                    | Some 1 -> numberInput 1, creature.name
                                    | Some q -> numberInput q, creature.PluralName_
                                    | None -> Html.text "N", creature.PluralName_
                                React.fragment [
                                    numberTxt
                                    classP' "editLink" Html.a [prop.text txt; prop.onClick(fun _ -> dispatch (SetPage (Editing name)))]
                                    ]
                            let changeQuantity (side: TeamSetup) (groupIndex: int, name: string) delta =
                                side |> List.mapi (
                                        fun ix group ->
                                            if ix = groupIndex then
                                                let members' =
                                                    group.members
                                                    |> (function (quantity, name') when name = name' -> (quantity + delta, name) | otherwise -> otherwise)
                                                { group with members = members' }
                                            else group
                                        )
                                    |> List.filter (fun group -> fst group.members > 0)
                            let setQuantity (side: TeamSetup) (groupIndex: int, name: string) q =
                                side |> List.mapi (
                                        fun ix group ->
                                            if ix = groupIndex then
                                                let members' =
                                                    group.members
                                                    |> (function (_, name') when name = name' -> (q, name) | otherwise -> otherwise)
                                                { group with members = members' }
                                            else group
                                        )
                                    |> List.filter (fun group -> fst group.members > 0)
                            class' "specificQuantity" Html.div [
                                let changeQuantity address delta fightSetup = { fightSetup with sideA = changeQuantity fightSetup.sideA address delta }
                                let setQuantity address delta fightSetup = { fightSetup with sideA = setQuantity fightSetup.sideA address delta }
                                let addToSideA monsterName fightSetup =
                                    // add a new group, even if the current monster exists within an existing group, so they can be placed in different locations
                                    { fightSetup with
                                        sideA = fightSetup.sideA@([1, monsterName] |> Setup.fresh)
                                        }
                                MonsterPicker (model.database, model.fightSetup.sideA.IsEmpty) <|
                                    ("Add", addToSideA >> ChangeFightSetup >> dispatch, SideA, dispatch) <|
                                        Html.div [
                                            match model.fightSetup.sideA with
                                            | [] -> Html.text "No creatures selected"
                                            | sideA ->
                                                for ix, group in sideA |> List.mapi Tuple2.create do
                                                    let quantity, name = group.members
                                                    Html.div [
                                                        Html.button [prop.text "+"; prop.onClick (fun _ -> dispatch (ChangeFightSetup (changeQuantity (ix, name) +1)))]
                                                        Html.button [prop.text "-"; prop.onClick (fun _ -> dispatch (ChangeFightSetup (changeQuantity (ix, name) -1)))]
                                                        editLink (Some quantity) name (fun q -> dispatch (ChangeFightSetup (setQuantity (ix, name) q)))
                                                        ]
                                            ]
                                ]
                            Html.text "vs."
                            class' "calibrated" Html.div [
                                let wrapInDiv (element: ReactElement) = Html.div [element]
                                let onClick msg =
                                    prop.onClick (fun _ -> dispatch (ChangeFightSetup msg))
                                let changeMode fight =
                                    { fight
                                        with
                                        sideB =
                                            // if there's at least one non-empty group, default to it. User will change it if they want to.
                                            // preserve positioning if possible so user can toggle back and forth without disruption.
                                            match fight.sideB with
                                            | Specific (({ members = (quantity, name) } as group)::_) ->
                                                Calibrate { members = (Some name, None, None, TPK); center = group.center; radius = group.radius }
                                            | Specific _ -> Setup.freshCalibrated()
                                            | Calibrate({ members = Some name, _, _, _} as group) -> Specific [{ members = (1, name); center = group.center; radius = group.radius }]
                                            | _ -> Specific []
                                        }
                                match model.fightSetup.sideB with
                                | Specific sideB ->
                                    let changeQuantity address delta (f: FightSetup) =
                                        match f.sideB with
                                            | Specific lst ->
                                                { f with sideB = Specific (changeQuantity lst address delta) }
                                            | otherwise -> f // shouldn't normally happen but maybe could I guess if user clicks more rapidly than React can process commands. Just ignore it in that case.
                                    let setQuantity address q (f: FightSetup) =
                                        match f.sideB with
                                            | Specific lst ->
                                                { f with sideB = Specific (setQuantity lst address q) }
                                            | otherwise -> f // shouldn't normally happen but maybe could I guess if user clicks more rapidly than React can process commands. Just ignore it in that case.
                                    let addToSideB monsterName fightSetup =
                                        // add a new group, even if the current monster exists within an existing group, so they can be placed in different locations
                                        { fightSetup with
                                            sideB = Specific (sideB@([1, monsterName] |> Setup.fresh))
                                            }

                                    MonsterPicker (model.database, sideB.IsEmpty) ("Add", addToSideB >> ChangeFightSetup >> dispatch, SideB, dispatch) <| React.fragment [
                                        Html.button [prop.text "Specific number"; onClick changeMode]
                                            |> wrapInDiv
                                        match sideB with
                                        | [] -> Html.text "No creatures selected"
                                        | sideB ->
                                            for ix, group in sideB |> List.mapi Tuple2.create do
                                                let quantity, name = group.members
                                                Html.div [
                                                    Html.button [prop.text "+"; onClick (changeQuantity (ix, name) +1)]
                                                    Html.button [prop.text "-"; onClick (changeQuantity (ix, name) -1)]
                                                    editLink (Some quantity) name (setQuantity (ix, name) >> ChangeFightSetup >> dispatch)
                                                    ]
                                        ]
                                | Calibrate({ members = (name, min, max, defeatCriteria) } as setup) ->
                                    let setSideB f fightSetup =
                                        { fightSetup with sideB = Calibrate { setup with members = f setup.members } }
                                    let setSideBName name = setSideB (fun (_, min, max, defeatCriteria) -> (Some name, min, max, defeatCriteria))
                                    let setSideBMin min = setSideB (fun (name, _, max, defeatCriteria) -> (name, min, max, defeatCriteria))
                                    let setSideBMax max = setSideB (fun (name, min, _, defeatCriteria) -> (name, min, max, defeatCriteria))
                                    let setSideBDefeatCriteria defeatCriteria = setSideB (fun (name, min, max, _) -> (name, min, max, defeatCriteria))
                                    MonsterPicker (model.database, name.IsNone) ("Set", setSideBName >> ChangeFightSetup >> dispatch, SideB, dispatch) <| React.fragment [
                                        Html.button [prop.text "Find optimal quantity"; onClick changeMode]
                                            |> wrapInDiv
                                        match name with
                                        | Some name ->
                                            class' "calibrationRange" Html.div [
                                                Html.div [
                                                    editLink None name ignore
                                                    Html.text "should lose"
                                                    class' "calibrationRange" Html.span [
                                                        let changeMin (txt: string) =
                                                            let v = match System.Int32.TryParse txt with true, v -> Some v | _ -> None
                                                            v |> setSideBMin |> ChangeFightSetup |> dispatch
                                                        let changeMax (txt: string) =
                                                            let v = match System.Int32.TryParse txt with true, v -> Some v | _ -> None
                                                            v |> setSideBMax |> ChangeFightSetup |> dispatch
                                                        Html.input [
                                                            prop.type'.number; prop.placeholder (defaultArg min 50 |> toString)
                                                            prop.onChange changeMin; prop.max 99
                                                            match min with Some min -> prop.valueOrDefault min | None -> ()
                                                            ]
                                                        Html.text "% to "
                                                        Html.input [
                                                            prop.type'.number; prop.placeholder (defaultArg max 90 |> toString)
                                                            prop.onChange changeMax; prop.max 99
                                                            match max with Some max -> prop.valueOrDefault max | None -> ()
                                                            ]
                                                        Html.text "% of the time"
                                                        ]
                                                    ]
                                                Html.div [
                                                    Html.text "as measured by"
                                                    let defeatDescription =
                                                        match defeatCriteria with
                                                        | TPK -> "Not killing every enemy"
                                                        | OneCasualty -> "Not killing at least one enemy"
                                                        | HalfCasualties -> "Not killing at least half the enemies"
                                                    let toggleDefeatCriteria _ =
                                                        let defeatCriteria =
                                                            match defeatCriteria with
                                                            | TPK -> OneCasualty
                                                            | OneCasualty -> HalfCasualties
                                                            | HalfCasualties -> TPK
                                                        setSideBDefeatCriteria defeatCriteria |> ChangeFightSetup |> dispatch

                                                    Html.button [prop.text $"{defeatDescription}"; prop.onClick toggleDefeatCriteria]
                                                    ]
                                                ]
                                        | None -> Html.div "No creatures selected"
                                        ]
                                ]
                            let notifyTeamMoved ((isTeamA, groupIx), (x: float<yards>, y: float<yards>)) =
                                let x, y = Ops.round x, Ops.round y
                                let changeTeamSetup (f: FightSetup) =
                                    match isTeamA, f.sideB with
                                    | true, _ ->
                                        let team = f.sideA |> List.mapi (fun ix group -> if ix = groupIx then { group with center = (x, y) } else group)
                                        { f with sideA = team }
                                    | false, Specific team ->
                                        let team = team |> List.mapi (fun ix group -> if ix = groupIx then { group with center = (x, y) } else group)
                                        { f with sideB = Specific team }
                                    | false, Calibrate group ->
                                        // in this case ignore groupIx
                                        { f with sideB = Calibrate { group with center = (x, y) } }
                                ChangeFightSetup changeTeamSetup |> dispatch
                            let db = model.database
                            let setup = model.fightSetup
                            let groups = [
                                let specifics isTeamA (side: GroupSetup list) = [
                                    for ix, group in side |> List.mapi Tuple2.create do
                                        let n, monsterName = group.members
                                        let c = db.catalog[monsterName]
                                        (isTeamA, ix), (if isTeamA then 0 else 1), c.Quantify n, group.center, Domain.CombatRules.radius_ (group, memberCount)
                                    ]
                                yield! (setup.sideA |> specifics true)
                                match setup.sideB with
                                | Specific sideB -> yield! specifics false sideB
                                | Calibrate ({ members = (Some name, _, _, _) } as group) ->
                                    let c = db.catalog[name]
                                    (false, 0), 1, $"N {c.PluralName_}", group.center, 5.<yards>
                                | Calibrate _ -> ()
                                ]
                            ArenaView.Setup(groups, notifyTeamMoved, dispatch)
                            ]

                        ExecuteButton model dispatch
                    match model.execution with
                    | NotStarted -> ()
                    | InProgress _ -> ExecuteButton model dispatch
                    | Completed (setup, result) ->
                        Html.button [prop.text "Reset"; prop.onClick (fun _ -> (Fighting NotStarted) |> dispatch)]
                        ExecuteButton model dispatch
                        let db = model.database
                        let teamToTxt team =
                            team
                            |> List.map (fun group -> group.members)
                            |> List.map (
                                function
                                | (1, name) -> $"1 {name}"
                                | (n, name) -> $"{n} {db.catalog[name].PluralName_}")
                            |> String.oxfordJoin
                        match result with
                        | SpecificResult (combat, victors) ->
                            match victors.victors with
                            | [1] ->
                                Html.div $"Team One wins!"
                            | [2] ->
                                Html.div $"Team Two wins!"
                            | [] ->
                                Html.div $"Everybody dies!"
                            | _ ->
                                Html.div $"Stalemate!"
                            ViewCombat (settings, setup, combat) dispatch
                        | CalibratedResult(min, max, sampleCombat) ->
                            let minQuantity, minPercent = match min with Some (qty, percent) -> Some qty, percent | None -> None, 90
                            let maxQuantity, maxPercent = match max with Some (qty, percent) -> Some qty, percent | None -> None, 50
                            let name = match setup.sideB with Calibrate({ members = Some name, _, _, _}) -> name | _ -> shouldntHappen()
                            class' "statistics" Html.div [
                                let quantityDescription =
                                    match minQuantity, maxQuantity with
                                    | Some 1, Some 1 -> $"1 {db.catalog[name].name}"
                                    | Some n, Some m when n = m -> $"{n} {db.catalog[name].PluralName_}"
                                    | Some n, None -> $"{n} or more {db.catalog[name].PluralName_}"
                                    | None, Some n -> $"{n} or fewer {db.catalog[name].PluralName_}"
                                    | Some n, Some m -> $"{n} to {m} {db.catalog[name].PluralName_}"
                                    | None, None -> $"an unknown number of {db.catalog[name].PluralName_}"
                                let percentDescription =
                                    match minPercent, maxPercent with
                                    | n, m when n = m -> $"{n}%%"
                                    | n, m when n < m -> $"{n}%% to {m}%%"
                                    | n, m -> $"{m}%% to {n}%%"
                                Html.div $"{setup.sideA |> teamToTxt} wins {percentDescription} of the time against {quantityDescription}"
                                ]
                            ViewCombat (settings, setup, sampleCombat) dispatch
                    ]
                ]
            ]

[<ReactComponent>]
let AutoFight(settings, header) =
    let state, dispatch = React.useElmishSimple init update
    View (settings, header) state dispatch