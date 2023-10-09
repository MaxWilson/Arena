module Domain.Geo
open Domain.Data

let ofList lst = { lookup = Map.ofList lst }
let place (combatantId: CombatantId) coords (geo:Geo2d) =
    // we'll probably add reverse lookup and other data structures as we go...
    { lookup = geo.lookup |> Map.add combatantId coords }

module private Impl =
    let distSquared ((lx, ly): Coords) (rx, ry) =
        let dx = lx - rx
        let dy = ly - ry
        dx * dx + dy * dy
    let dist lhs rhs = distSquared lhs rhs |> sqrt
open Impl

type Line(origin, dest) =
    let length = dist origin dest
    member _.Length = length
    member _.Origin = origin
    member this.Extend (distance: Distance) = // TODO: find a better name than "extend". Basically, go this far in the direction of dest and return the new dest.
        let origX, origY = origin
        let destX, destY = dest
        let (dx, dy) = destX - origX, destY - origY
        let (dx, dy) = (dx / length, dy / length)
        (origX + dx * distance, origY + dy * distance)

type Geo2d with
    member this.Find id = this.lookup[id]
    member this.DistanceBetween(lhsPos, rhsPos) = dist lhsPos rhsPos
    member this.DistanceBetween(lhsId, rhsId) = dist (this.Find lhsId) (this.Find rhsId)
    member this.DistanceBetween(lhsId, rhsPos) = dist (this.Find lhsId) rhsPos
    member this.DistanceBetween(lhsPos, rhsId) = dist lhsPos (this.Find rhsId)
    member this.DistanceSquared(lhsPos, rhsPos) = distSquared lhsPos rhsPos
    member this.DistanceSquared(lhsId, rhsId) = distSquared (this.Find lhsId) (this.Find rhsId)
    member this.DistanceSquared(lhsId, rhsPos) = distSquared (this.Find lhsId) rhsPos
    member this.DistanceSquared(lhsPos, rhsId) = distSquared lhsPos (this.Find rhsId)
    member this.LineFrom (lhsPos: Coords, rhsPos: Coords) = Line(lhsPos, rhsPos)
    member this.LineFrom (lhsId, rhsId) = Line(this.Find lhsId, this.Find rhsId)
