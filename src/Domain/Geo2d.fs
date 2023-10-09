module Domain.Geo
open Domain.Data

let indexOf ((x,y): Coords) =
    (x * 2.) |> int, (y * 2.) |> int
let tryPlace (combatantId: CombatantId) coords (geo:Geo2d) =
    let places coords =
        let x, y = indexOf coords
        [ x, y; x + 1, y; x - 1, y; x, y + 1; x, y - 1 ]
    let mutable error = None
    let err msg = match error with None -> error <- Some msg | _ -> ()
    let mutable occupants = geo.occupancy
    match geo.lookup |> Map.tryFind combatantId with
    | Some priorCoords ->
        for point in priorCoords |> places do
            match geo.occupancy |> Map.tryFind point with
            | Some prior when prior = combatantId ->
                occupants <- occupants |> Map.remove point
            | Some v ->
                err $"{combatantId} is leaving {priorCoords} but occupancy says {v} was there instead!"
            | None ->
                err $"{combatantId} is leaving {priorCoords} but occupancy says it was empty!"
    | None -> ()
    for point in places coords do
        match occupants |> Map.tryFind point with
        | None ->
            occupants <- occupants |> Map.add point combatantId
        | Some priorInhabitant ->
            err $"{combatantId} cannot be placed at {point} because it is already occupied by {priorInhabitant}!"
    match error with
    | Some err -> Error err
    | None -> Ok { lookup = geo.lookup |> Map.add combatantId coords; occupancy = occupants }

let place combatantId coords geo =
    match tryPlace combatantId coords geo with
    | Ok v -> v
    | Error e -> shouldntHappen e

let ofList lst =
    lst |> List.fold (fun geo (id, coords) -> place id coords geo) { lookup = Map.empty; occupancy = Map.empty }

module private Impl =

    // we're using hex distance instead of euclidean distance
    let distSquared ((lx, ly): Coords) ((rx, ry): Coords) =
        let dx = lx - rx |> abs
        let dy = ly - ry |> abs
        let bigger = max dx dy
        // in hex distance, you can move laterally "for free" as long as it's not too far.
        let smaller =
            match min dx dy, bigger / 2. with
            | v, lateralMargin when v <= lateralMargin -> 0.<yards>
            | v, lateralMargin -> v - lateralMargin
        bigger * bigger + smaller * smaller
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

type Geo2d with
    member this.Find id = this.lookup[id]
    member this.WithinDistance(lhsPos, rhsPos, distance) = distanceLessThan lhsPos rhsPos distance
    member this.WithinDistance(lhsId, rhsId, distance) = distanceLessThan (this.Find lhsId) (this.Find rhsId) distance
    member this.HexDistanceSquared(lhsPos, rhsPos) = distSquared lhsPos rhsPos
    member this.HexDistanceSquared(lhsId, rhsId) = distSquared (this.Find lhsId) (this.Find rhsId)
    member this.HexDistanceSquared(lhsId, rhsPos) = distSquared (this.Find lhsId) rhsPos
    member this.HexDistanceSquared(lhsPos, rhsId) = distSquared lhsPos (this.Find rhsId)
    member this.LineFrom (lhsPos: Coords, rhsPos: Coords) = Line(lhsPos, rhsPos)
    member this.LineFrom (lhsId, rhsId) = Line(this.Find lhsId, this.Find rhsId)
    member this.Approach (lhsId, dest: Destination, movementBudget: int) : Coords * float<yards> * int =
        match dest with
        | Person _ -> notImpl "Approach person"
        | Place _ -> notImpl "Approach place"
