module Domain.Character

let randomNation _ = chooseRandom ["Tir na n'Og"; "Abysia"; "Kailasa"; "Ermor"; "Undauntra"; "Arboria"; "Mordor"]

let makeNameAndTitle (nationOfOrigin: string) (sex: Sex) =
    let rec chooseFromLists =
        function
        | potentialCategory::rest ->
            match Onomastikon.nameLists |> Map.tryFind (nationOfOrigin, potentialCategory) with
            | Some nameList -> chooseRandom nameList
            | None -> chooseFromLists rest
        | [] -> "" // sometimes e.g. there is no last name for a given national origin

    // for Neither sex, we'll choose names haphazardly
    let sex = match sex with Male | Female -> sex | Neither -> chooseRandom [Male; Female]

    let firstName = chooseFromLists [sex.ToString()]
    match firstName with
    | "" -> None // invalid nation/sex combination (e.g. no females in Mordor), try again
    | _ ->
        let lastName (name, _title) =
            let surname = chooseFromLists [$"Last";$"Cognomen{sex}";$"Last{sex}";]
            $"{name} {surname}".Trim(), None
        let prefix (name, _title) =
            let prefixes = ["Insanity"; "Black"; "Merciless"; "Gentle"; "Calamity"]
            $"{chooseRandom prefixes} {name}".Trim(), None
        let title (name, _title) =
            let suffixes = ["Defender of Humanity"; "Last of the Dwarflords"; "the Accursed"; "Esquire"; "the Undying"; "the Merciful"; (if sex = Female then "Duchess of the Realm" else "Duke of the Realm"); "the Wall"; "the fat"]
            name, Some (chooseRandom suffixes)
        let allThree = (prefix >> lastName >> title)
        chooseRandomExponentialDecay 0.4 Seq.head [lastName; (lastName >> title); prefix; allThree] (firstName, None)
        |> Some

let makeNameAnyNation sex =
    let rec recur nation =
        match makeNameAndTitle nation sex with
        | None -> recur (randomNation())
        | Some (name, maybeTitle) -> nation, name, maybeTitle
    recur (randomNation())

type Generate =
    static member randomly(?sex, ?race, ?nation) =
        let sex = sex |> Option.defaultWith (thunk1 chooseRandom [Male; Female]) // only get Neither sex if explicitly asking for it
        let race = race |> Option.orElseWith (fun _ -> chooseRandomExponentialDecay 0.4 Seq.head [Some "Human"; None; Some "Dwarf"; Some "Elf"; Some "Half-ogre"])
        let nation, name, title = makeNameAnyNation sex
        RoleplayingData.create(name, sex, ?race=race, ?title=title, nationalOrigin=nation)

#nowarn "40" // we're not planning on doing any unsafe things during initialization, like evaluating the functions that rely on the object we're busy constructing
module Parser =
    open Packrat
    open Random
    open Random.Parser
    open Domain.Parser
    let validNameCharsNoComma = alphanumeric + whitespace + Set.ofList ['\''; '-']
    let (|CharacterName|_|) =
        function
        | Chars validNameCharsNoComma (txt, rest) -> Some(txt.Trim(), rest)
        | _ -> None
    let (|Sex|_|) =
        function
        | Str "Male" rest -> Some(Male, rest)
        | Str "Female" rest -> Some(Male, rest)
        | Str "Neither" rest -> Some(Male, rest)
        | _ -> None
    let (|Race|_|) =
        function
        | Chars validNameCharsNoComma (txt, rest) -> Some(txt.Trim(), rest)
        | _ -> None
    let (|Title|_|) =
        function
        | Chars validNameCharsNoComma (txt, rest) -> Some(txt.Trim(), rest)
        | _ -> None
    let (|NationalOrigin|_|) =
        function
        | Chars validNameCharsNoComma (txt, rest) -> Some(txt.Trim(), rest)
        | _ -> None

    let (|Maybe|_|) delimiter (|Pattern|_|) = function
        | Str delimiter (OWS (Pattern(v, rest))) -> Some(Some v, rest)
        | rest -> Some(None, rest)

    let (|RoleplayingData|_|) =
        function
        | CharacterName(name, Maybe "," (|Sex|_|) (sex, rest)) -> Some(RoleplayingData.create(name), rest)
        | CharacterName(name, rest) -> Some(RoleplayingData.create(name), rest)
        | _ -> None
    let (|CharacterSheet|_|) = pack <| function
        | RoleplayingData(rp, OWSStr ":" (CreatureProperties(fprops, rest))) ->
            // We use the full title as a sufficiently-unique name. We could use a GUID instead but for some reason I don't want to.
            // There could eventually be bugs though if you try to use two characters with the exact same name/sex/race/nation in
            // the same party so maybe we eventually will change to use GUIDs.
            let typeName = rp.ToString()
            Some(CharacterSheet.create(rp, Stats.create(typeName) |> fprops), rest)
        | _ -> None
