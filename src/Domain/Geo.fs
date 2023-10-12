module Domain.Geo
open Domain.Data

module private Impl =

    // we're using hex distance instead of euclidean distance
    let hexDistSquared ((lx, ly): Coords) ((rx, ry): Coords) =
        let dx = lx - rx |> abs
        let dy = ly - ry |> abs
        let bigger = max dx dy
        // in hex distance, you can move laterally "for free" as long as it's not too far.
        let smaller =
            match min dx dy, bigger / 2. with
            | v, lateralMargin when v <= lateralMargin -> 0.<yards>
            | v, lateralMargin -> v - lateralMargin
        bigger * bigger + smaller * smaller
    let hexDist lhs rhs = hexDistSquared lhs rhs |> sqrt
    let hexDistanceLessThan lhs rhs distance = hexDist lhs rhs <= distance + 0.1<yards>
    let euclideanSquared (lx, ly) (rx, ry) =
        // usually we should use hex distance, but euclidean sometimes makes sense for sorting more finely than hex distance can manage
        let dx = lx - rx
        let dy = ly - ry
        dx * dx + dy * dy
    let yardsToPlaces (v: float<yards>) = (v * 2.) |> int
    let placesToYards (v: int) = v |> float |> ( * ) 0.5<yards>
    let placeOf ((x,y): Coords) =
        (yardsToPlaces x, yardsToPlaces y)
    let placesFor coords =
        let x, y = placeOf coords
        [ x, y; x + 1, y; x, y + 1; x + 1, y + 1 ]
open Impl

(* places equate to the zero-based index of a part of a hex.

112233445566
112233445566
778899AABBCC
778899AABBCC
EEFFGGHHIIJJ
EEFFGGHHIIJJ

1 is a hex, and 2 is a hex too, but

12
12

is also a hex, 0.5 to the right of 1. Each hex has four places, and each place is a zero-based index
into the hex grid. 0.0 yards to the right = hex 1 = places 0 and 1 along the x axis.

Observe that places correspond to *areas* of obstruction, not points.

*)

// Zero-based index into geo.occupancy, with each integer increment representing a move of 0.5 yards.
type Place = int * int
let placeOf = placeOf

let placesToCoords (x,y) = placesToYards x, placesToYards y

let placesNear coords hexDistance : Place list =
    let x0, y0 = placeOf coords
    let placeDistance = (hexDistance * 2.) |> int
    [ for x in x0 - placeDistance .. x0 + placeDistance do
        for y in y0 - placeDistance .. y0 + placeDistance do
            if hexDistanceLessThan coords (Data.coords (float x / 2. , float y / 2.)) hexDistance then
                x, y
        ]

let canPlace (combatantId: CombatantId) coords (geo:Geo2d) =
    let newPlaces = placesFor coords
    newPlaces |> List.every (fun place ->
        match geo.obstructions |> Map.tryFind place with
        | None -> true
        | Some priorInhabitant when priorInhabitant = combatantId -> true
        | Some prior -> false
        )

let tryPlace (combatantId: CombatantId) coords (geo:Geo2d) =
    let mutable error = None
    let err msg = match error with None -> error <- Some msg | _ -> ()
    let mutable occupants = geo.obstructions
    let newPlaces = placesFor coords
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
            for point in priorCoords |> placesFor do
                match geo.obstructions |> Map.tryFind point with
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
    | None -> Ok { lookup = geo.lookup |> Map.add combatantId coords; obstructions = occupants }

let place combatantId coords geo =
    match tryPlace combatantId coords geo with
    | Ok v -> v
    | Error e -> shouldntHappen e

let ofList lst =
    lst |> List.fold (fun geo (id, coords) -> place id coords geo) { lookup = Map.empty; obstructions = Map.empty }

type Line(origin, dest) =
    let length = hexDist origin dest
    member _.Length = length
    member _.Origin = origin
    member this.Extend (distance: Distance) = // TODO: find a better name than "extend". Basically, go this far in the direction of dest and return the new dest.
        if distance = 0.<yards> then origin
        else
            let origX, origY = origin
            let destX, destY = dest
            let (dx, dy) = destX - origX, destY - origY
            let (dx, dy) = (dx / length, dy / length)
            (origX + dx * distance, origY + dy * distance)

type Geo2d with
    member this.Find id = this.lookup[id]
    member this.WithinDistance(lhsPos, rhsPos, distance) = hexDistanceLessThan lhsPos rhsPos distance
    member this.WithinDistance(lhsId, rhsId, distance) = hexDistanceLessThan (this.Find lhsId) (this.Find rhsId) distance
    member this.HexDistanceSquared(lhsPos, rhsPos) = hexDistSquared lhsPos rhsPos
    member this.HexDistanceSquared(lhsId, rhsId) = hexDistSquared (this.Find lhsId) (this.Find rhsId)
    member this.HexDistanceSquared(lhsId, rhsPos) = hexDistSquared (this.Find lhsId) rhsPos
    member this.HexDistanceSquared(lhsPos, rhsId) = hexDistSquared lhsPos (this.Find rhsId)
    member this.LineFrom (lhsPos: Coords, rhsPos: Coords) = Line(lhsPos, rhsPos)
    member this.LineFrom (lhsId, rhsId) = Line(this.Find lhsId, this.Find rhsId)
    member this.TryApproach (lhsId, dest: Destination, movementBudget: int) : (Coords * float<yards> * int) option =
        let movementBudgetInHexes = float movementBudget * 1.<yards>
        let line =
            match dest with
            | Person dest -> this.LineFrom(lhsId, dest)
            | Place dest -> this.LineFrom(this.Find lhsId, dest)
        let origin = line.Origin
        let originPlace = placeOf origin
        let dest = line.Extend (min line.Length movementBudgetInHexes)
        let rec placeNear dest radius =
            let candidates =
                placesNear dest radius
                |> List.filter(fun place ->
                    place <> originPlace // ignore the starting place because not moving would be useless
                    && hexDistanceLessThan origin (placesToCoords place) movementBudgetInHexes) // filter out places that we don't have the budget to reach
                |> List.sortBy (fun place ->
                                    let placeCoords = placesToCoords place
                                    this.HexDistanceSquared (dest, placeCoords), // prefer moving as little as possible while getting as hex-close to the target as possible,
                                    this.HexDistanceSquared (origin, placeCoords), // in part to help surround enemies by not aligning perfectly on 4 sides.
                                    euclideanSquared origin placeCoords // Use euclidean distance as a tie breaker because it looks prettier than having a bias towards the leftmost place.
                                    )
            match candidates |> List.tryFind (fun place -> canPlace lhsId (placesToCoords place) this) with
            | Some place ->
                let coords = placesToCoords place
                let dist = hexDist origin coords
                Some (coords, dist, Ops.roundUp dist |> int)
            | None ->
                if radius < movementBudgetInHexes then placeNear dest (radius + 1.<yards>) else None
        placeNear dest (yards 1.)

    // leave combatantId in lookup so it can be displayed, but remove it from obstructions
    member this.RemoveObstruction combatantId =
        match this.lookup |> Map.tryFind combatantId with
        | None -> shouldntHappen "RemoveObstruction found no combatant"
        | Some priorCoords ->
            let occ' = priorCoords |> placesFor |> List.fold (fun occupants place -> occupants |> Map.change place (function (Some id') when id' = combatantId -> None | unexpected -> shouldntHappen $"While removing obstruction {combatantId} at {place} found unexpected occupant '{unexpected}'!")) this.obstructions
            { this with obstructions = occ' }

let tryApproach args (g: Geo2d) = g.TryApproach args