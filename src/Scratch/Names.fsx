#I ".."
#load @"Core\Common.fs"

// names inspired by Dragon Magazine #34
let nameData = [
    [ "Hawk"; "Black"; "Blade"; "Wood"; "Sure"; "Shadow"; "Dancer"; "Red"; "Wolf"; "Hammer" ]
    [ "Fair"; "Stone"; "Death"; "White"; "Slayer"; "Tiger"; "Flame"; "Horn"; "Blood"; "Storm" ]
    [ "Spear"; "Singer"; "High"; "Bear"; "Helm"; "Shield"; "Bone"; "Soul"; "Bane"; "Piper" ]
    [ "Eagle"; "Gray"; "Staff"; "Moon"; "Free"; "Sea"; "Changer"; "Gold"; "Lion"; "Rune" ]
    [ "Dark"; "Brother"; "Silver"; "Weaver"; "Fox"; "Cleaver"; "Fang"; "Heart"; "Strong"; ]
    [ "Dreamer"; "Star"; "Bow"; "Claw"; "Wave"; "Sky"; "Foam"; "Fist"; "Wise"; "Wind" ]
    [ "Sly" ]
    ]
let generate() =
    let generateSimple columns =
        let candidates = columns |> List.concat
        candidates |> chooseRandom
    let rec generateComposite (columns1, columns2) =
        match generateSimple(columns1), (generateSimple(columns2):string) with
        | (x, y) when x = y -> generateComposite (columns1, columns2)
        | (x, y) -> x + y.ToLowerInvariant()
    let select ns lst = lst |> List.mapi Tuple2.create |> List.choose (fun (ix, x) -> if ns |> List.contains ix then Some x else None)
    let prefix = nameData |> select [0;2;4] // these columns "feel" more prefixy. Blackspear, Moonbear, etc. instead of Bearmoon, Spearblack
    let suffix = nameData |> select [1;3;5]
    let solo = nameData |> select [0..5]
    match rand 100 with
    | n when n <= 50 -> $"{generateComposite(prefix, suffix)} {generateComposite(prefix, suffix)}"
    | n when n <= 80 -> $"{generateSimple(solo)} {generateSimple(solo)}"
    | _ -> $"{generateSimple(solo)} {generateComposite(prefix, suffix)}"
for i in 1..20 do
    generate() |> printf "%s, "
