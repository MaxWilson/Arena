// stuff for tying fights together into adventures and campaigns
module Domain.Campaign

type Name = Character of string | Monster of string
type Roster = CharacterSheet list
type IndividualOrGroup = Individual of CharacterSheet | Group of int * monsterName: string
type TeamNumber = int
type Setup = (TeamNumber * IndividualOrGroup list) GroupSetup list

module Setup =
    let enumerateMembers setup =
        [   for ix, group in setup |> List.mapi Tuple2.create do
                let teamNumber, members = group.members
                for ix2, member1 in members |> List.mapi Tuple2.create do
                    let address = ix, ix2
                    yield address, group, teamNumber, member1
            ]


let createCombat (db: Map<string, Stats>) (teams: Setup) =
    let mutable geo = Geo.ofList []
    let radius_ (group: _ GroupSetup) =
        group.radius |> Option.defaultWith (fun () ->
            let _, members = group.members
            let memberCount = members |> List.sumBy (function Individual _ -> 1 | Group (quantity, _) -> quantity)
            1.0<yards> * (sqrt (float memberCount)))
    let place (group: _ GroupSetup) (combatant: Combatant) =
        let center, radius = group.center, radius_ group
        let gen (radius: float<yards>) =
            let angleRadians = random.NextDouble() * 2. * System.Math.PI
            let radius = random.NextDouble() * radius
            let x = cos angleRadians * radius + fst center |> Ops.round
            let y = sin angleRadians * radius + snd center |> Ops.round
            x, y
        let rec findEmptyCoords failureCount radius candidate =
            let x,y = candidate
            match Geo.tryPlace combatant.Id (x, y) geo with
            | Ok g ->
                geo <- g
            | Error _ ->
                if failureCount > 10 then findEmptyCoords 0 (radius + 1.<yards>) (gen radius) // maybe it's full; widen the radius so we don't get stuck in an infinite loop
                else findEmptyCoords (failureCount+1) radius (gen radius)
        combatant, findEmptyCoords 0 radius (gen radius)
    let toCombatants (db: Map<string, Stats>) project =
        // we want numbers to ascend smoothly on a side, so that we can use numbers to prioritize targets in the same order they were in fightsetup
        let mutable counter = 0
        let mutable perMonsterCounter = Map.empty
        fun (lst: Setup) ->
            [   for group in lst do
                    let teamNumber, members' = group.members
                    for member' in members' do
                        match member' with
                        | Individual _ -> notImpl()
                        | Group (quantity, name) ->
                            for i in 1..quantity do
                                let stats = db[name]
                                let prev = defaultArg (perMonsterCounter.TryFind name) 0 // if there are multiple groups of e.g. 1 orc and 1 orc, the second group should start at Orc 2 not "Orc"
                                yield Combatant.fresh(teamNumber, (if prev + quantity > 1 then $"{name} {prev + i}" else name), counter + i, stats) |> project group
                            counter <- counter + quantity
                            perMonsterCounter <- perMonsterCounter |> Map.add name (defaultArg (perMonsterCounter.TryFind name) 0 + quantity)
                ]
    let setup = teams |> (toCombatants db place)
    let combatants = setup |> List.map (fun (c, _) -> c.Id, c) |> Map.ofList
    let behaviors = setup |> List.map (fun (c, _) -> c.Id, Behavior.justAttack) |> Map.ofList
    {   combat =
            {   Combat.fresh with
                    combatants = combatants
                    geo = geo
            }
        behaviors = behaviors
        }
