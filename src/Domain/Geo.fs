module Domain.Geo
open Domain.Data

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
    let yardsToIndex (v: float<yards>) = (v * 2.) |> int
    let indexToYards (v: int) = (v / 2) |> float |> ( * ) 1.<yards>
open Impl
type Place = int * int
let indexOf ((x,y): Coords) =
    (yardsToIndex x, yardsToIndex y)

let indexToCoords (x,y) = indexToYards x, indexToYards y

let placesNear coords hexDistance : Place list =
    let x0, y0 = indexOf coords
    let placeDistance = (hexDistance * 2.) |> int
    [ for x in x0 - placeDistance .. x0 + placeDistance do
        for y in y0 - placeDistance .. y0 + placeDistance do
            if distanceLessThan coords (Data.coords (float x / 2. , float y / 2.)) hexDistance then
                x, y
        ]

let tryPlace (combatantId: CombatantId) coords (geo:Geo2d) =
    let places coords =
        let x, y = indexOf coords
        [ x, y; x + 1, y; x - 1, y; x, y + 1; x, y - 1 ]
    let mutable error = None
    let err msg = match error with None -> error <- Some msg | _ -> ()
    let mutable occupants = geo.occupancy
    let newPlaces = places coords
    for point in newPlaces do
        if error = None then // we optimize for finding collisions fast, so fail immediately if there's an error
            match occupants |> Map.tryFind point with
            | Some priorInhabitant when priorInhabitant <> combatantId ->
                err $"{combatantId} cannot be placed at {point} because it is already occupied by {priorInhabitant}!"
            | _ ->
                occupants <- occupants |> Map.add point combatantId
    if error = None then // we optimize for finding collisions fast, so fail immediately if there's an error
        match geo.lookup |> Map.tryFind combatantId with
        | Some priorCoords ->
            for point in priorCoords |> places do
                match geo.occupancy |> Map.tryFind point with
                | Some prior when prior = combatantId ->
                    // we want to optimize for finding collisions fast, so we try to add new places before removing old places,
                    // which means we need to be careful not to undo those new additions when removing
                    if not (newPlaces |> List.contains point) then
                        occupants <- occupants |> Map.remove point
                | Some v ->
                    // if this ever happens it is a bug not a simple failure. Want it to throw, not just cause tryPlace to fail.
                    shouldntHappen $"{combatantId} is leaving {priorCoords} but occupancy says {v} was there instead!"
                | None ->
                    shouldntHappen $"{combatantId} is leaving {priorCoords} but occupancy says it was empty!"
        | None -> ()
    match error with
    | Some err -> Error err
    | None -> Ok { lookup = geo.lookup |> Map.add combatantId coords; occupancy = occupants }

let place combatantId coords geo =
    match tryPlace combatantId coords geo with
    | Ok v -> v
    | Error e -> shouldntHappen e

let ofList lst =
    lst |> List.fold (fun geo (id, coords) -> place id coords geo) { lookup = Map.empty; occupancy = Map.empty }

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
    member this.TryApproach (lhsId, dest: Destination, movementBudget: int) : (Coords * float<yards> * int) option =
        let line =
            match dest with
            | Person dest -> this.LineFrom(lhsId, dest)
            | Place dest -> this.LineFrom(this.Find lhsId, dest)
        let origin = line.Origin
        let dest = line.Extend (movementBudget |> float |> (( * ) 1.<yards>))
        let rec placeNear coords radius =
            let candidates =
                placesNear coords radius
                    |> List.filter(fun place -> distanceLessThan origin (indexToCoords place) radius) // filter out places that we don't have the budget to reach
                    |> List.sortBy (fun place -> this.HexDistanceSquared (coords, indexToCoords place)) // prefer places that are close to the destination
            match candidates |> List.tryPick (fun place ->
                match tryPlace lhsId coords this with
                | Ok geo ->
                    let coords = indexToCoords place
                    let dist = dist origin coords
                    Some (coords, dist, int dist)
                | _ -> None) with
            | Some results -> Some results
            | None -> if radius < (yards 1. * float movementBudget) then placeNear coords (radius + 1.<yards>) else None
        placeNear dest (yards 1. * float movementBudget)
