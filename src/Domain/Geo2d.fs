module Domain.Geo
open Domain.Data

let ofList lst = { lookup = Map.ofList lst }
let place (combatant: Combatant) coords (geo:Geo2d) =
    // we'll probably add reverse lookup and other data structures as we go...
    { geo with lookup = geo.lookup |> Map.add combatant.Id coords }

type Geo2d with
    member this.Find id = this.lookup[id]