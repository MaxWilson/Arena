module UI.Components.Campaign
open Domain.Data

type Name = Character of string | Monster of string
type History = {
    kills: Map<Name, int>
    experience: Map<Name, int>
    }
    with
    static member fresh = { kills = Map.empty; experience = Map.empty }
    static member xp this =
        this.experience.Values |> Seq.sumBy System.Math.Log2 |> int

type CharacterSheet = {
    personalName: string
    stats: Stats
    draft: string // should be identical to stats but store a copy as a fallback, just in case one or the other fails due to deserialization issues
    history: History
    notes: (string * System.DateTimeOffset) list
    }

type Roster = CharacterSheet list