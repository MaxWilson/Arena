module Domain.Geo
open Domain.Data

let ofList lst = { lookup = Map.ofList lst }
let place (combatantId: CombatantId) coords (geo:Geo2d) =
    // we'll probably add reverse lookup and other data structures as we go...
    { lookup = geo.lookup |> Map.add combatantId coords }

module private Impl =
    let distSquared ((lx, ly): Coords) ((rx, ry): Coords) =
        let dx = lx - rx
        let dy = ly - ry
        dx * dx + dy * dy
    let dist lhs rhs = distSquared lhs rhs |> sqrt
    let distanceLessThan lhs rhs distance = dist lhs rhs <= distance + 0.1<yards>
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

// Actual Euclidean distances should be used only for approximation purposes (UI display and target prioritization), not for actual game rules,
// because grids simplify issues like finding where to stand and who's in reach. Insisting on Euclidean precision for things like how many orcs
// can surround a knight would be onerous.
type Geo2d with
    member this.Find id = this.lookup[id]
    member this.WithinDistance(lhsPos, rhsPos, distance) = distanceLessThan lhsPos rhsPos distance
    member this.WithinDistance(lhsId, rhsId, distance) = distanceLessThan (this.Find lhsId) (this.Find rhsId) distance
    member this.EuclideanSquared(lhsPos, rhsPos) = distSquared lhsPos rhsPos
    member this.EuclideanSquared(lhsId, rhsId) = distSquared (this.Find lhsId) (this.Find rhsId)
    member this.EuclideanSquared(lhsId, rhsPos) = distSquared (this.Find lhsId) rhsPos
    member this.EuclideanSquared(lhsPos, rhsId) = distSquared lhsPos (this.Find rhsId)
    member this.LineFrom (lhsPos: Coords, rhsPos: Coords) = Line(lhsPos, rhsPos)
    member this.LineFrom (lhsId, rhsId) = Line(this.Find lhsId, this.Find rhsId)
    member this.Approach (lhsId, dest: Destination, movementBudget: int) : Coords * float<yards> * int =
        match dest with
        | Person _ -> notImpl()
        | Place _ -> notImpl()