module Domain.Character
// Unlike Data.fs, this is for character sheet stuff, not just combat stats stuff. We may end up merging them eventually but for now this is roleplay-related

type Sex = Male | Female | Neither
type Name = string

type RoleplayingData = {
    name: Name
    sex: Sex
    nationalOrigin: string
}

let randomNation _ = chooseRandom ["Tir na n'Og"; "Abysia"; "Kailasa"; "Ermor"; "Undauntra"; "Arboria"; "Mordor"]

let makeName (nationOfOrigin: string) (sex: Sex) =
    let rec chooseFromLists =
        function
        | potentialCategory::rest ->
            match Onomastikon.nameLists |> Map.tryFind (nationOfOrigin, potentialCategory) with
            | Some nameList -> chooseRandom nameList
            | None -> chooseFromLists rest
        | [] -> "" // sometimes e.g. there is no last name for a given national origin
    let firstName = chooseFromLists [sex.ToString()]
    match firstName with
    | "" -> None // invalid nation/sex combination (e.g. no females in Mordor), try again
    | _ ->
        let lastName name =
            let surname = chooseFromLists [$"Last";$"Cognomen{sex}";$"Last{sex}";]
            $"{name} {surname}".Trim()
        let prefix name =
            let prefixes = ["Insanity"; "Black"; "Merciless"; "Gentle"; "Calamity"]
            $"{chooseRandom prefixes} {name}".Trim()
        let title name =
            let suffixes = ["Defender of Humanity"; "Last of the Dwarflords"; "the Accursed"; "Esquire"; "the Undying"; "the Merciful"; (if sex = Female then "Duchess of the Realm" else "Duke of the Realm"); "the Wall"; "the fat"]
            $"{name}, {chooseRandom suffixes}".Trim()
        let allThree = (prefix >> lastName >> title)
        chooseRandomExponentialDecay 0.4 Seq.head [lastName; (lastName >> title); prefix; allThree] firstName
        |> Some

let makeNameAnyNation sex =
    let rec recur nation =
        match makeName nation sex with
        | None -> recur (randomNation())
        | Some name -> nation, name
    recur (randomNation())