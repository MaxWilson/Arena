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

let makeName(sex, preferredNation) =
    let rec recur nation =
        match makeNameAndTitle nation sex with
        | None -> recur (randomNation())
        | Some (name, maybeTitle) -> nation, name, maybeTitle
    recur (preferredNation |> Option.defaultWith randomNation)

type Generate =
    static member randomly(?sex: Sex, ?race: string, ?nation:string) =
        let sex = sex |> Option.defaultWith (thunk1 chooseRandom [Male; Female]) // only get Neither sex if explicitly asking for it
        let race = race |> Option.orElseWith (fun _ -> chooseRandomExponentialDecay 0.4 Seq.head [Some "Human"; None; Some "Dwarf"; Some "Elf"; Some "Half-ogre"])
        let nation, name, title = makeName(sex, nation)
        RoleplayingData.create(name, ?title=title, ?sex=Some sex, ?race=race, nationalOrigin=nation)

#nowarn "40" // we're not planning on doing any unsafe things during initialization, like evaluating the functions that rely on the object we're busy constructing
module Parser =
    open Packrat
    open Random
    open Random.Parser
    open Domain.Parser
    let validNameCharsNoComma = alphanumeric + whitespace + Set.ofList ['\''; '-']
    let (|CharacterName|_|) = function
        | Chars validNameCharsNoComma (txt, rest) -> Some(txt.Trim(), rest)
        | _ -> None
    let (|Sex|_|) = function
        | OWSStr "Male" rest -> Some(Male, rest)
        | OWSStr "Female" rest -> Some(Female, rest)
        | OWSStr "Neuter" rest -> Some(Neither, rest)
        | _ -> None
    let (|NationalOrigin|_|) = function
        | OWSStr "from" (Chars validNameCharsNoComma (txt, rest)) -> Some(txt.Trim(), rest)
        | _ -> None
    let (|Race|_|) = function
        // we're generally pretty liberal about race names but we don't want to ever confuse "from Ermor", which is a nation declaration, with a race.
        // later on we may restrict race names to a specific list.
        | NationalOrigin _ -> None
        | OWS (Chars validNameCharsNoComma (txt, rest)) -> Some(txt.Trim(), rest)
        | _ -> None
    let (|Title|_|) = function
        // we're generally pretty liberal about titles but we don't want to ever mistake "from Ermor"/"Female"/etc. for titles, which is a nation declaration, with a race.
        | NationalOrigin _ | Sex _ -> None
        | Chars validNameCharsNoComma (txt, rest) -> Some(txt.Trim(), rest)
        | _ -> None
    let (|SexAndRace|_|) = function
        | Sex (sex, (Race (race, rest))) -> Some((sex, race), rest)
        | _ -> None
    let (|Maybe|_|) delimiter (|Pattern|_|) = function
        | Str delimiter (OWS (Pattern(v, rest))) -> Some(Some v, rest)
        | rest -> Some(None, rest)

    let (|RoleplayingData|_|) =
        function
        | CharacterName(name, Str "," (Title(title, Str "," (SexAndRace((sex, race), Maybe "," (|NationalOrigin|_|) (nation, rest)))))) ->
            Some(RoleplayingData.create(name, ?sex=Some sex, race=race, title=title, ?nationalOrigin=nation), rest)
        | CharacterName(name, Str "," (Title(title, Maybe "," (|Sex|_|) (sex, Maybe "," (|NationalOrigin|_|) (nation, rest))))) -> Some(RoleplayingData.create(name, ?sex=sex, title=title, ?nationalOrigin=nation), rest)
        | CharacterName(name, Str "," (Title(title, Maybe "," (|Race|_|) (race, Maybe ", " (|NationalOrigin|_|) (nation, rest))))) -> Some(RoleplayingData.create(name, ?race=race, title=title, ?nationalOrigin=nation), rest)
        | CharacterName(name, Str "," (SexAndRace((sex, race), Maybe "," (|NationalOrigin|_|) (nation, rest)))) ->
            Some(RoleplayingData.create(name, ?sex=Some sex, race=race, ?nationalOrigin=nation), rest)
        | CharacterName(name, Maybe "," (|Sex|_|) (sex, Maybe "," (|NationalOrigin|_|) (nation, rest))) -> Some(RoleplayingData.create(name, ?sex=sex, ?nationalOrigin=nation), rest)
        | CharacterName(name, Maybe "," (|Race|_|) (race, Maybe ", " (|NationalOrigin|_|) (nation, rest))) -> Some(RoleplayingData.create(name, ?race=race, ?nationalOrigin=nation), rest)
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
