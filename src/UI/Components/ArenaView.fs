module UI.Components.ArenaView

open System
open UI.Konva
open Feliz
open Feliz.UseElmish
open Elmish
open Domain.Data
open Domain.Geo
open UI.Components.Arena
open UI.Data

[<AutoOpen>]
module private Impl =
    open Fable.Core.JsInterop
    type RenderHelper(pixelWidth, pixelHeight) =
        let _scaleX, _scaleY = (float pixelWidth / 40.<yards>), (float pixelHeight / 40.<yards>)
        // hmmm, I guess we want to use the same scale for both X and Y don't we? Take the minimum and just let the other space go unused.
        let _scaleX, _scaleY = let m = min _scaleX _scaleY in m, m
        member inline _.scaleX (x: float<yards>) = x * _scaleX
        member inline _.unscaleX (x: float) = x / _scaleX
        member inline _.scaleY (y: float<yards>) = y * _scaleY
        member inline _.unscaleY (y: float) = y / _scaleY
        member inline this.scale (x: float<yards>, y: float<yards>) = this.scaleX x, this.scaleY y
        member inline this.unscale (x: float, y: float) = this.unscaleX x, this.unscaleY y
        member inline _.rect name (x:float<yards>,y:float<yards>) props =
            Rect.create ([
                Rect.x (x * _scaleX)
                Rect.y (y * _scaleY)
                Rect.key name
                ]
                @props)
        member inline this.line name (points: (float<yards> * float<yards>) list) props =
            Line.create ([
                Line.key name
                Line.points (points |> List.collect (fun (x,y) -> [ this.scaleX x; this.scaleY y ]) |> Array.ofList)
                ]
                @props)
        member inline _.circle name (x:float<yards>,y:float<yards>) props =
            Circle.create ([
                Circle.x (x * _scaleX)
                Circle.y (y * _scaleY)
                Circle.key name
                ]
                @props)
        member inline _.text name (x:float<yards>,y:float<yards>) props =
            Text.create ([
                Text.x (x * _scaleX)
                Text.y (y * _scaleY)
                Text.key name
                ]
                @props)
        member inline _.group name (x:float<yards>,y:float<yards>) (props: IGroupProperty list) =
            Group.create (([
                Group.x (x * _scaleX)
                Group.y (y * _scaleY)
                Group.key name
                ] : IGroupProperty list)
                @ props)
    let toYard(x:int) = float x * 1.<yards>
    let display (pixelSize: int * int) stageProps render =
        let renderHelper = (RenderHelper(pixelSize))
        stage [
            Stage.height (snd pixelSize)
            Stage.width (fst pixelSize)
            Stage.children [
                Layer.createNamed "background" [
                    Rect.create [
                        Rect.x 0
                        Rect.y 0
                        Rect.fill Color.LightGrey
                        Rect.width (fst pixelSize)
                        Rect.height (snd pixelSize)
                        Rect.key "Rect1"
                        ]
                    ]
                yield! render renderHelper
                ]
            yield! stageProps renderHelper
            ]
    let layoutGrid (r: RenderHelper) =
        Layer.createNamed "Grid" [
            for i in [0..40] do
                let x = i |> toYard
                r.line $"Line_vertical{x}" [x,0.<yards>; x, 40.<yards>] [
                    Line.stroke Color.Black
                    Line.strokeWidth 1
                    Line.opacity (if i % 5 = 0 then 0.3 else 0.1)
                    ]
                r.line $"Line_horizontal{x}" [0.<yards>, x; 40.<yards>, x] [
                    Line.stroke Color.Black
                    Line.strokeWidth 1
                    Line.opacity (if i % 5 = 0 then 0.3 else 0.1)
                    ]
            ]
    // tween animation--modify just one node independently of others
    let inline simpleMoveAnimation (r:RenderHelper) ((x,y): Coords) (durationSeconds: float) (onFinish: unit -> unit) (node: KonvaNode)  =
        node.``to`` (createObj [
            "x" ==> r.scaleX x
            "y" ==> r.scaleY y
            "duration" ==> durationSeconds
            "onFinish" ==> onFinish
            ])
    let isMultitouch ev = ev?evt && ev?evt?touches && ev?evt?touches?length > 1

[<ReactComponent>]
let Setup (groups, onDrag, dispatch) =
    display (320, 320) (thunk []) <| fun r -> [
        layoutGrid r
        Layer.createNamed "teams" [
            for (groupAddress, teamNumber, label:string, center, radius) in groups do
                r.group $"Group{groupAddress}" center [
                    Group.draggable
                    Group.onDragEnd(fun e -> onDrag(groupAddress, (r.unscaleX (e.target.x()), r.unscaleY (e.target.y()))))
                    // Group.offsetX -25
                    // Group.offsetY -25
                    Group.children [|
                        circle [
                            Circle.radius (r.scaleX radius)
                            Circle.fill (if teamNumber = 0 then Color.Blue else Color.Purple)
                            Circle.key "outline"
                            ]
                        text [
                            Text.verticalAlign Middle
                            Text.align Center
                            Text.fill Color.Black
                            // do NOT scale text to yards
                            let textWidth = label.Length * 6
                            Text.width textWidth
                            Text.offsetX (textWidth / 2 |> float)
                            Text.height 20
                            Text.offsetY (let r = r.scaleX radius in if float textWidth < r * 2.5 then 10. else 15. + r) // if the circle is big enough, e.g. "N Orcs", center inside it. But if the label is bigger than the circle, offset it above.
                            Text.fontSize 9
                            Text.fontStyle "800" // unusually bold
                            Text.key "name1"
                            Text.text label
                            ]
                        |]
                    ]
            ]
        ]


[<ReactComponent>]
let Actual (settings: Settings, combatants: Map<CombatantId, Combatant>, logEntry: CombatAtoms.Logged option, geo: Geo2d) dispatch =
    let shownNames, setShownNames = React.useState Map.empty
    let hover, setHover = React.useState None
    let isTouch, setIsTouch = React.useState false
    let nearestNeighborCache, setNearestNeighborCache = React.useState Map.empty
    let stageRef = React.useRef None
    let nodeRefs = React.useRef Map.empty
    let priorAnimation = React.useRef None
    let stageProps (r: RenderHelper) = [
        Stage.ref (fun (r: StageNode) -> stageRef.current <- Some r)
        let inline nearest (x, y) =
            let x, y = r.unscaleX x |> Ops.round, r.unscaleY y |> Ops.round
            match nearestNeighborCache |> Map.tryFind (x, y) with
            | Some ids -> ids
            | None ->
                let distancesSquared =
                    combatants.Values
                    |> List.ofSeq
                    |> List.map (fun c -> c.Id, geo.HexDistanceSquared(c.Id, (x,y)))
                    |> List.sortBy snd
                match distancesSquared with
                | (closestId, distanceSquared)::rest when distanceSquared <= 25.<yards^2> -> // look for anything that's reasonably close
                    setNearestNeighborCache (nearestNeighborCache |> Map.add (x, y) (Some closestId))
                    Some closestId
                | _ -> None
        let showClosestMonster isTouch' ev =
            if isTouch <> isTouch' then setIsTouch isTouch'
            if isTouch && isMultitouch ev then () // allow normal pinch/zoom/pan to proceed without interference
            else
            match stageRef.current with
            | None -> ()
            | Some ref ->
                let x, y = let c = ref.getPointerPosition() in c.x, c.y
                match nearest (x,y) with
                | None when hover <> None -> setHover None
                | Some h when hover <> Some h -> setHover (Some h)
                | _ -> ()
        Stage.onTouchStart (showClosestMonster true)
        Stage.onTouchMove (showClosestMonster true)
        Stage.onTouchEnd (fun _ -> setHover None)
        Stage.onMouseMove (showClosestMonster false)
        Stage.onMouseLeave (fun _ -> setHover None)

        // maybe we shouldn't be doing this inside of stageProps since we only want the side effect... but we need access to the renderHelper for scaling,
        // and since stageProps will always run it doesn't really hurt anything. Maybe someday we'll refactor display to take a separate hooks() argument which
        // runs separately before evaluating stageProps but for now it doesn't really matter.
        React.useLayoutEffect ((fun _ ->
            let cancelPriorAnimation currentId = // don't cancel an animation that's still current
                match priorAnimation.current with
                | Some id as prior when currentId <> prior ->
                    let node: KonvaNode = nodeRefs.current[id]
                    // cancel the animation by setting x and y explicitly
                    let x, y = geo.Find id
                    node.x(r.scaleX x)
                    node.y(r.scaleY y)
                | _ -> ()
                priorAnimation.current <- currentId

            match logEntry with
            | Some (Logged.MoveTo(id, origin, dest, _, _)) ->
                cancelPriorAnimation (Some id)
                match nodeRefs.current |> Map.tryFind id with
                | Some node ->
                    node |> simpleMoveAnimation r dest 0.2 ignore
                | _ -> ()
                // we don't want prior animations to stay on the screen if we go back in time, so we need to reset x and y. TODO: find a way to cancel the prior animation by killing the tween BEFORE we reset x and y.
            | _ -> cancelPriorAnimation None
            ), [| box logEntry |])
        ]
    let display =
        match settings.battlegridDisplaySize with
        | Big -> display (600, 600)
        | Small -> display (320, 320)
        | Zero -> fun _ _ -> React.fragment []
    display stageProps <| fun r -> [
        Layer.createNamed "Background" [
            Rect.create [
                Rect.x 0
                Rect.y 0
                Rect.fill Color.LightGrey
                Rect.width winW
                Rect.height winH
                Rect.key "Rect1"
                ]
            ]
        layoutGrid r
        Layer.create [
            Layer.key "Combatants" :> ILayerProperty
            Layer.children [
                // Konva react doesnt' really have a concept of z-index, so make sure that anything hovered will be drawn last so it's on top.
                for c in combatants.Values |> Seq.sortBy (fun c -> hover = Some c.Id) do
                    Group.create ([
                        let x,y =
                            match logEntry with
                            | Some (MoveTo(id, origin, _, _, _)) when id = c.Id -> origin // DON'T use the final position because we want to animate movement towards it
                            | _ -> geo.Find(c.Id)
                        Group.x (r.scaleX x)
                        Group.y (r.scaleY y)
                        Group.key (toString c.Id)
                        Group.onClick (fun e -> shownNames |> Map.change c.Id (function Some () -> None | None -> Some ()) |> setShownNames)
                        Group.ref (fun node ->
                            let current = nodeRefs.current
                            match current |> Map.tryFind c.Id with
                            | Some existingNode when obj.Equals(node, existingNode) = false ->
                                nodeRefs.current <- current |> Map.add c.Id node
                            | None ->
                                nodeRefs.current <- current |> Map.add c.Id node
                            | Some _ -> ()
                            )
                        Group.children [
                            circle [
                                Circle.radius (r.scaleX 0.5<yards>)
                                Circle.fill (if c.team = 1 then Color.Blue else Color.Purple)
                                Circle.key "outline"
                                Circle.onMouseOver (fun e ->
                                    e.target.getStage().container().style.cursor <- CursorType.Pointer
                                    )
                                Circle.onMouseLeave (fun e ->
                                    e.target.getStage().container().style.cursor <- CursorType.Default
                                    )
                                if hover = Some c.Id then
                                    Circle.stroke Color.Black
                                    Circle.strokeWidth 3
                                elif shownNames |> Map.containsKey c.Id then
                                    Circle.stroke Color.Black
                                    Circle.strokeWidth 2
                                elif c.isAny [Dead; Unconscious] then
                                    Circle.opacity 0.2
                                ]
                            if hover = Some c.Id || shownNames |> Map.containsKey c.Id then
                                let label = if c.is Dead then $"Dead {c.personalName}" elif c.is Unconscious then $"Unconscious {c.personalName}" else c.personalName
                                let textWidth = label.Length * 14
                                Rect.create [
                                    Rect.key "hoverBackground"
                                    Rect.width textWidth
                                    Rect.offsetX (textWidth / 2 |> float)
                                    Rect.height 20
                                    Rect.offsetY (32. + if isTouch then 20. else 0.)
                                    Rect.fill Color.White
                                    Rect.stroke Color.Black
                                    Rect.strokeWidth 2
                                    ]
                                Text.create [
                                    Text.verticalAlign Middle
                                    Text.align Center
                                    Text.fill Color.Black
                                    // do NOT scale text to yards
                                    Text.width textWidth
                                    Text.offsetX (textWidth / 2 |> float)
                                    Text.height 20
                                    Text.offsetY (30. + if isTouch then 20. else 0.)
                                    Text.fontSize 18
                                    if hover = Some c.Id then Text.fontStyle "900" // unusually bold
                                    else Text.fontStyle "bold"
                                    Text.key "name"
                                    Text.text label
                                    ]
                            ]
                        ]: IGroupProperty list)
                ]
            ]
        ]

module private Impl0 =
    open Fable.Core.JsInterop
    let r = System.Random()
    type Movespec = { id: UniqueId; from: int * int; unto: int * int; mutable started: bool; afterwards: unit -> unit }
        with
        member this.start(node: KonvaNode0) =
            if this.started then ()
            else
                this.started <- true
                let id = this.id.ToString().Substring(0,6)
                let square x = x * x
                let length = sqrt (square (fst this.from - fst this.unto) + square (snd this.from - snd this.unto) |> float)
                node.to' (createObj [
                    let x,y = this.unto
                    "x" ==> x
                    "y" ==> y
                    "duration" ==> ((float length) * 0.005 |> min 0.3)
                    "onFinish" ==> (fun () -> this.afterwards())
                    ])
    type Todo =
        | Tween of Movespec
        | Immediate of (unit -> unit)
open Impl0

[<ReactComponent>]
let DefaultFrame (args: FrameInputs) stage =
    Html.div [
        prop.className args.className
        prop.children [
            stage
            class' "control" Html.div [
                let addRandom text _ =
                    Add(Guid.NewGuid(), (r.Next(0, stageW), r.Next(0, stageH)), text) |> args.dispatch
                let jiggle _ =
                    match args.model.creatures.Keys |> List.ofSeq with
                    | [] -> () // nothing to do
                    | ids ->
                        let id = chooseRandom ids
                        Move(id, Relative(r.Next(-250, 250), r.Next(-250, 250))) |> args.dispatch
                let moveDown _ =
                    match args.model.creatures.Keys |> List.ofSeq with
                    | [] -> () // nothing to do
                    | ids ->
                        for id in ids do
                            Move(id, Relative(20, 50)) |> args.dispatch
                let moveToTop _ =
                    match args.model.creatures.Keys |> List.ofSeq with
                    | [] -> () // nothing to do
                    | ids ->
                        for ix, id in ids |> List.mapi Tuple2.create do
                            Move(id, Absolute(100 + ix * 100, 50)) |> args.dispatch
                Html.button [prop.text "Add an orc"; prop.onClick (addRandom "Orc") ]
                Html.button [prop.text "Add an Inigo"; prop.onClick (addRandom "Inigo") ]
                Html.button [prop.text "Random movement"; prop.onClick jiggle ]
                Html.button [prop.text "Move down"; prop.onClick moveDown ]
                Html.button [prop.text "Move To Top"; prop.onClick moveToTop ]
                Html.button [prop.text "Clear"; prop.onClick (fun _ -> args.dispatch Clear) ]
                ]
            ]
        ]


// Arena0 probably won't wind up being used. Consider it a proof of concept.
[<ReactComponent>]
let Arena0 (init, history': Msg list) =
    // start with initialModel, then movements flow in through history' to executionQueue and eventually to canon
    let canon, setCanon = React.useState init
    let futureCanon, setFutureCanon = React.useState init
    let knownHistory, setKnownHistory = React.useState []
    let executionQueue = React.useRef ([]: Todo list) // we need all the closures to share the same mutable queue
    let currentTransition, setCurrentTransition = React.useState None
    let movingObject = React.useRef None
    let (|Inert|Ready|Transitioning|) currentTransition =
        match currentTransition, executionQueue.current with
        | Some m, _ -> Transitioning m
        | None, [] -> Inert
        | None, h::_ -> Ready h
    // because React hooks including useLayoutEffect must happen a fixed number of times, we have to split Tween/Immediate execution between a layout effect and regular code
    // TODO: refactor this logic to be clearer
    let rec pump currentTransition =
        match currentTransition with
        | Transitioning _ -> () // we're already in the middle of a transition, don't start another one
        | Inert -> () // nothing to do
        | Ready (todo) ->
            // move into transitioning state
            executionQueue.current <- executionQueue.current.Tail
            match todo with
            | Immediate f -> f(); pump None
            | Tween _ ->
                setCurrentTransition (Some todo) // will trigger re-render, which will start tween
    React.useLayoutEffect <| fun () ->
        match currentTransition with
        | Transitioning (Tween todo) ->
            match movingObject.current with
            | Some obj->
                todo.start obj
            | v -> shouldntHappen v
        | Inert | Ready _ | Transitioning(Immediate _) | _ -> ()
    let proj model model' = function
        | Clear | Add _ -> Some(Immediate(fun () -> setCanon model')) // we want to set ourselves in the state we'd be AFTER this message
        | Move(id, move) as msg ->
            let c = model.creatures[id] // we want to start at where the creature was BEFORE this message and then move to where it should be AFTER
            { id = id; from = (c.x, c.y); unto = updateViaMovement c move; started = false; afterwards = fun  () -> setCanon model'; setCurrentTransition None }
                |> Tween |> Some
    let futureCanon', todo = CQRS.cqrsDiff update proj (futureCanon, knownHistory) history' // DON'T start the diff from canon, start it from the model we'll have after doing all the messages
        // we don't need the model output, that will come as commands flow through the execution queue
    match todo with
    | [] ->
        match currentTransition with
        | Ready _ -> pump currentTransition
        | _ -> ()
    | h::t ->
        setKnownHistory history'
        setFutureCanon futureCanon'
        // start the pump whenever execution queue goes from empty to non-empty
        let startPump = executionQueue.current.IsEmpty
        executionQueue.current <- executionQueue.current @ todo
        if startPump then pump currentTransition
    Stage.create [
        Stage.width stageW // TODO: there's gotta be a better way to be responsive to mobile size constraints
        Stage.height stageH
        Stage.children [
            Layer.createNamed "background" [
                Rect.create [
                    Rect.x 0
                    Rect.y 0
                    Rect.fill Color.LightGrey
                    Rect.width winW
                    Rect.height winH
                    Rect.key "Rect1"
                    ]
                ]
            Layer.createNamed "arena" [
                for creature in canon.creatures.Values do
                    Group.create [
                        match currentTransition with
                        | Transitioning(Tween { id = id; from = (x,y) }) when id = creature.id ->
                            (Group.x x: IGroupProperty)
                            Group.y y
                            Group.ref (fun node -> movingObject.current <- Some (unbox node)) // unbox is a kludge but we don't plan to keep this code anyway
                        | _ ->
                            Group.x creature.x
                            Group.y creature.y
                        Group.key (toString creature.id)
                        Group.children [
                            Circle.create [
                                Circle.radius 25
                                Circle.fill Color.Red
                                Circle.key "circle"
                                Circle.offsetX -25
                                Circle.offsetY -25
                                ]
                            Text.create [
                                Text.verticalAlign Middle
                                Text.align Center
                                Text.fill Color.Black
                                Text.width 50
                                Text.height 50
                                match creature.text with
                                | Some txt -> Text.text txt
                                | None -> ()
                                Text.key "name"
                                ]
                            ]
                        ]
                ]
            ]
        ]


