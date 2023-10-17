module UI.Components.Campaign
open Domain.Data

type CharacterSheet = {
    personalName: string
    stats: Stats
    draft: string // should be identical to stats but store a copy as a fallback, just in case one or the other fails due to deserialization issues
    notes: (string * System.DateTimeOffset) list
    }

type Roster = CharacterSheet list