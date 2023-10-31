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
            let prefixes = ["Insanity"; "Black"; "Blackheart"; "Merciless"; "Gentle"; "Calamity"; "Doughty"; "Dangerous"; (if sex = Female then "Dame" else "Sir"); "Cowardly"]
            $"{chooseRandom prefixes} {name}".Trim(), None
        let title (name, _title) =
            let suffixes = ["Defender of Humanity"; "Last of the Dwarflords"; "the Accursed"; "Esquire"; "the Undying"; "the Merciful"; (if sex = Female then "Duchess of the Realm" else "Duke of the Realm"); "the Wall"; "the Stout"; "the Black"; "the White"; "the Grey"; "the Kinslayer"; "the Coward"]
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
        let randomPick lst = chooseRandomExponentialDecay 0.3 (fun _ -> chooseRandom (None::lst)) lst // we want no race to be fairly rare so we make it no more common than random even during fallback
        let race = race |> Option.orElseWith (fun _ -> randomPick (List.map Some ["Human"; "Dwarf"; "Elf"; "Half-ogre"; "Coleopteran"]))
        let nation, name, title = makeName(sex, nation)
        RoleplayingData.create(name, ?title=title, ?sex=Some sex, ?race=race, nationalOrigin=nation)

#nowarn "40" // we're not planning on doing any unsafe things during initialization, like evaluating the functions that rely on the object we're busy constructing
module Parser =
    open Packrat
    open Random
    open Random.Parser
    open Domain.Parser
    let validNameCharsNoComma = alphanumeric + Set.ofList ['\''; '-']
    let (|NameSegment|_|) = function
        | OWS (Chars validNameCharsNoComma (txt, rest)) -> Some(txt.Trim(), rest)
        | _ -> None
    let rec (|CharacterName|_|) = pack <| function
        | CharacterName(txt, NameSegment(txt2, rest)) when txt2 <> "the" -> Some(txt + " " + txt2, rest)
        | NameSegment (txt, rest) -> Some(txt, rest)
        | _ -> None
    let rec (|RestOfTitle|_|) = pack <| function
        | RestOfTitle(txt, NameSegment(txt2, rest)) when txt2 <> "from" -> Some(txt + " " + txt2, rest)
        | NameSegment (txt, rest) -> Some(txt, rest)
        | _ -> None
    let (|AppositiveTitle|_|) = function
        | OWSStr "," (RestOfTitle(txt, rest)) -> Some(txt, rest)
        | _ -> None
    let (|NonAppositiveTitle|_|) = function
        | OWSStr "the" (RestOfTitle(txt, rest)) -> Some("the " + txt, rest)
        | _ -> None
    let (|Sex|_|) = function
        | OWSStr "Male" rest -> Some(Male, rest)
        | OWSStr "Female" rest -> Some(Female, rest)
        | OWSStr "Neuter" rest -> Some(Neither, rest)
        | _ -> None
    let rec (|Nation|_|) = pack <| function
        | Nation(txt, NameSegment(txt2, rest)) -> Some(txt + " " + txt2, rest)
        | NameSegment(txt, rest) -> Some(txt, rest)
        | _ -> None
    let (|NationalOrigin|_|) = function
        | OWSStr "from" (Nation (txt, rest)) -> Some(txt, rest)
        | _ -> None
    let (|Race|_|) = function
        // we're generally pretty liberal about race names but we don't want to ever confuse "from Ermor", which is a nation declaration, with a race.
        // later on we may restrict race names to a specific list.
        | NationalOrigin _ -> None
        | OWS (Chars validNameCharsNoComma (txt, rest)) -> Some(txt.Trim(), rest)
        | _ -> None
    let (|Maybe|_|) delimiter (|Pattern|_|) = function
        | Str delimiter (OWS (Pattern(v, rest))) -> Some(Some v, rest)
        | rest -> Some(None, rest)
    let (|SexRaceNation|_|) =
        let (|SexRace|_|) = function
            | Sex(sex, Race(race, rest)) -> Some ((Some sex, Some race), rest)
            | Sex(sex, rest) -> Some ((Some sex, None), rest)
            | Race(race, rest) -> Some ((None, Some race), rest)
            | _ -> None
        function
        | OWSStr "," (SexRace((sex, race), NationalOrigin(nation, rest))) -> Some((sex, race, Some nation), rest)
        | OWSStr "," (SexRace((sex, race), rest)) -> Some((sex, race, None), rest)
        | Optional "," (NationalOrigin(nation, rest)) -> Some((None, None, Some nation), rest) // be tolerant of unneeded commas like in "Gandalf the White, from Undauntra"
        | _ -> None
    let (|RoleplayingData|_|) =
        function
        | CharacterName(name, SexRaceNation ((sex, race, nation), ((OWSStr ":" _) as rest))) ->
            // use lookahead for ":" to ensure that "Lyron Barrister, King of Swords, Male Human from Ermor" does not match using "King" for Race
            Some(RoleplayingData.create(name, ?sex=sex, ?race=race, ?title=None, ?nationalOrigin=nation), rest)
        | CharacterName(name, NonAppositiveTitle(title, SexRaceNation ((sex, race, nation), rest))) ->
            Some(RoleplayingData.create(name, ?sex=sex, ?race=race, ?title=Some title, ?nationalOrigin=nation), rest)
        | CharacterName(name, AppositiveTitle(title, SexRaceNation ((sex, race, nation), rest))) ->
            Some(RoleplayingData.create(name, ?sex=sex, ?race=race, ?title=Some title, ?nationalOrigin=nation), rest)
        | _ -> None
    let (|CharacterSheet|_|) = pack <| function
        | RoleplayingData(rp, OWSStr ":" (CreatureProperties(fprops, rest))) ->
            // We use the full title as a sufficiently-unique name. We could use a GUID instead but for some reason I don't want to.
            // There could eventually be bugs though if you try to use two characters with the exact same name/sex/race/nation in
            // the same party so maybe we eventually will change to use GUIDs.
            let typeName = rp.ToString()
            Some(CharacterSheet.create(rp, Stats.create(typeName) |> fprops), rest)
        | _ -> None
