module Domain.Geo
open Domain.Data

let ofList lst = { lookup = Map.ofList lst }
let place (combatant: Combatant) coords (geo:Geo2d) =
    // we'll probably add reverse lookup and other data structures as we go...
    { lookup = geo.lookup |> Map.add combatant.Id coords }

module private Impl =
    let distSquared ((lx, ly): Coords) (rx, ry) =
        let dx = lx - rx
        let dy = ly - ry
        dx * dx + dy * dy
    let dist lhs rhs = distSquared lhs rhs |> sqrt
open Impl

type Geo2d with
    member this.Find id = this.lookup[id]
    member this.DistanceBetween(lhsId, rhsId) =
        dist (this.Find lhsId) (this.Find rhsId)
    member this.DistanceBetween(lhsPos, rhsPos) = dist lhsPos rhsPos