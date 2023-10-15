module Coroutine

type ExecutionResult<'actionOut, 'feedback, 'ctx, 'finalResult> = Finished of 'finalResult | AwaitingAction of 'actionOut * Behavior<'actionOut, 'feedback, 'ctx, 'finalResult>
and Behavior<'actionOut, 'feedback, 'ctx, 'finalResult> =
    | Parameterized of ('feedback * 'ctx -> ExecutionResult<'actionOut, 'feedback, 'ctx, 'finalResult>)
    | Absolute of (unit -> ExecutionResult<'actionOut, 'feedback, 'ctx, 'finalResult>)
type ReturnAction<'actionOut> = ReturnAction of 'actionOut
type QueryRequest<'ctx, 'result> = QueryRequest of ('ctx -> 'result)
type QueryFeedback<'feedback, 'result> = QueryFeedback of ('feedback -> 'result)
type ChildResult<'actionOut, 'feedback, 'ctx, 'finalResult> = Ready of 'finalResult | Resume of Behavior<'actionOut, 'feedback, 'ctx, 'finalResult>
let inline run logic (feedback, ctx) : ExecutionResult<_,_,_,_> =
    match logic with
    | Absolute logic -> logic()
    | Parameterized logic -> logic(feedback, ctx)

type BehaviorBuilder() =
    member this.Delay behaviorThunk = behaviorThunk
    member this.Run (b:unit -> Behavior<_,_,_,_>) = Parameterized (fun (feedback, ctx) -> run (b())(feedback, ctx))
    member this.Return (x: 't) : Behavior<_,_,_,_> = Absolute(fun () -> Finished x)
    member this.ReturnFrom (x: Behavior<_,_,_,_>) = x
    // member this.Bind(b, f) = bind b f
    member this.Bind(ReturnAction(action), binder: 'feedback * 'ctx -> Behavior<'action,'feedback,'ctx,'finalResult>): Behavior<'action,'feedback,'ctx,'finalResult> =
        (* consider a block of behavior that looks like this:

            let! feedback, context = ReturnAction(SimpleAttack)
            return if feedback > 0 then Success else Failure

            From the caller's perspective, this looks like two separate, nested behaviors

            let AwaitingAction(action1, continuation) = behavior(...) // this is the ReturnAction
            let Finished Failure = continuation (feedback, context) // Here's where we consume feedback and evaluate it to produce a result.

            So even though it LOOKS at first like it's weird for feedback and context to get fed to followupBehavior, it actually makes sense
            because mem and action are outputs whereas feedback and context are inputs.
        *)
        // we discard the action/memory/context here, but we might have used them previously via QueryRequest to construct the action we're requesting
        fun (feedback, ctx) ->
            AwaitingAction(action,
                fun (feedback, ctx) ->
                    match binder (feedback, ctx) with
                    | Absolute logic -> logic() // promote absolute to parameterized behavior
                    | Parameterized logic -> logic(feedback, ctx)
                |> Parameterized
                )
            // ignoring feedback and context in favor of feedback' and ctx' feels wrong but seems to work. What's going on? Is it for the same reason that we ignore feedback and ctx in Return()? (I.e. feedback and ctx may have come in through previous bindings.)
        |> Parameterized
    member this.Bind(q: QueryRequest<_,'result>, binder: 'result -> Behavior<_,_,_,_>): Behavior<'action,'feedback,'ctx,'finalResult>  =
        fun(feedback, ctx) ->
            let (QueryRequest qf) = q
            let r = qf ctx
            match binder r with
            | Absolute logic -> logic() // promote absolute to parameterized behavior
            | Parameterized logic -> logic(feedback, ctx)
        |> Parameterized
    member this.Bind(q: QueryFeedback<'feedback,'result>, binder: 'result -> Behavior<_,_,_,_>): Behavior<'action,'feedback,'ctx,'finalResult> =
        fun(feedback: 'feedback, ctx) ->
            let (QueryFeedback qf) = q
            let r = qf feedback
            match binder r with
            | Absolute logic -> logic() // promote absolute to parameterized behavior
            | Parameterized logic -> logic(feedback, ctx)
        |> Parameterized
    member this.Bind(childResult: ExecutionResult<_,_,_,_>, binder: ChildResult<_,_,_,_> -> Behavior<_,_,_,_>) =
        // when you do let! x = run (child) in ... you should get back either a Ready finalResult or a Resume behavior which continues the child. You can choose whether to actually resume it or switch to a different behavior.
        fun(feedback: 'feedback, ctx) ->
            match childResult with
            | AwaitingAction(action, resume) ->
                AwaitingAction(action, Parameterized (fun args -> run (binder (Resume resume)) args))
            | Finished result ->
                run (binder (Ready result)) (feedback, ctx)
        |> Parameterized
    (*
        Okay, let's think through this scenario from the perspective of the behavior which is USING the child behavior.

        ignoredFeedback, ctx1
            parent behavior: Cowardly
                child behavior: justAttack
                    Action: Attack(followup: justAttack)
                Action: Attack(followup: cowardly justAttack)
        Attack feedback, ctx2
            parent behavior: Cowardly
                child behavior: justAttack
                    Action: Attack(followup: justAttack)
                Action: Attack(followup: cowardly justAttack)
        Attack feedback, ctx3
            parent behavior
                child behavior: flee
                    Action: Move

        or even simpler:
        initialFeedback, ctx1
            parent behavior: loop
                child behavior: walk
                    Action: Move(followup: finish Move)
                Action: Move(followup: loop at Resume(finish Move)) // BehaviorBuilder must do this automatically based on let! _ = RunChildRequest walk
        Move feedback, ctx2
            parent behavior: loop at Resume(finish Move)
                child behavior: finish Move
                    finalResult: ()
                return! loop restart // how does it know to do this? We must be passing in the finalResult somehow.

    *)
let behavior = BehaviorBuilder()

// let check() =
//     let rec smokeTest: Behavior<_,_,_,_> = behavior {
//         let feedback = 12
//         let! startArg = QueryFeedback id
//         let! feedback, () = ReturnAction(startArg)
//         let! feedback, ctx = ReturnAction(feedback)
//         let! ctx = QueryRequest id
//         let! childResult = run smokeTest (feedback, ctx)
//         return feedback
//         }
//     let (AwaitingAction(action, followup)) = run smokeTest (3, ())
//     let (AwaitingAction(action, followup)) = run followup (2, ())
//     let (Finished action) = run followup (1, ())
//     let rec parent child backToBeginning: Behavior<_,_,_,_> = behavior {
//         let! ctx = QueryRequest id
//         let! feedback = QueryFeedback id
//         match! run child (feedback, ctx) with
//         | Ready result -> return! parent backToBeginning backToBeginning
//         | Resume followup -> return! parent followup backToBeginning
//         }
//     let parent = parent smokeTest smokeTest
//     ()


// let smoke2_a: Behavior<int,unit,int,int> =
//     let b = behavior
//     let x = b.Return 4
//     let y = b.Bind(ReturnAction 3, fun ((), ctx) -> x)
//     b.Run(b.Delay(fun () -> b.Bind(ReturnAction 4, fun ((), ctx) -> b.Return ctx))) // note! we're not just using bind, we're applying it to (), ctx! The F# builder doesn't do this automatically. How can we force it to?
// let smoke2: Behavior<int,unit,int,int> =
//     let b = behavior
//     let delayed = b.Delay(fun () -> b.Return 4)
//     let run = b.Run delayed
//     let compare = behavior {
//         let! feedback, ctx = ReturnAction 4
//         let! feedback, ctx = ReturnAction ctx
//         return ctx
//         }
//     b.Run(b.Delay(fun () -> b.Bind(ReturnAction 4, fun ((), ctx) -> b.Bind(ReturnAction ctx, fun ((), ctx) -> b.Return ctx)))) // note! we're not just using bind, we're applying it to (), ctx! The F# builder doesn't do this automatically. How can we force it to?

// let (AwaitingAction(action, followup)) = run smoke2 ((), 1)
// let (AwaitingAction(action, followup)) = run followup ((), 2)

// let smoke3 =
//     let inline bind (ReturnAction(action), binder: Behavior<_,_,_,_>) : ExecutionResult<_,_,_,_> =
//         // we discard the action/memory/context here, but we might have used them previously via QueryRequest to construct the action we're requesting
//         AwaitingAction(action, fun (feedback', context') ->
//             printfn $"Binding to {(feedback', context')} twice"
//             binder (feedback', context')) // ignoring feedback and context in favor of feedback' and ctx' feels wrong but seems to work. What's going on? Is it for the same reason that we ignore feedback and ctx in Return()? (I.e. feedback and ctx may have come in through previous bindings.)
//     let smoke3a = bind(ReturnAction 4, fun ((), ctx) -> Finished ctx)
//     let b = behavior
//     b.Run(b.Delay(fun () -> bind(ReturnAction 4, fun ((), ctx) ->
//         //let feedback, context = ((), ctx)
//         let (ReturnAction(action)) = (ReturnAction ctx)
//         let binder = fun ((), ctx) -> b.ReturnFrom smoke2 ((), ctx)
//         AwaitingAction(action, fun (feedback', context') ->
//             printfn $"Binding to {(feedback', context')} twice"
//             binder (feedback', context')) // ignoring feedback and context in favor of feedback' and ctx' feels wrong but seems to work. What's going on? Is it for the same reason that we ignore feedback and ctx in Return()? (I.e. feedback and ctx may have come in through previous bindings.)
//         )))
// let (AwaitingAction(action, followup)) = smoke3((), 1)
// let (AwaitingAction(action, followup)) = followup((), 2)

// // Okay, part of what's going on is that since we always ignore the initial feedback of a behavior, the duplication isn't always hurting us.