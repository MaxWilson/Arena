module Main

open Feliz
open Feliz.Router
open Feliz.UseListener
open UI.Components.Sample
open UI.Components.AutoFightView
open UI.Components.Arena
open UI.Components.ArenaView
open Browser.Dom
open Fable
open Fable.Core.JsInterop
open Common.UI
importSideEffects "./sass/main.sass"


[<ReactComponent>]
let Router() =
    let (currentUrl, updateUrl) = React.useState(Router.currentUrl())
    let state, dispatch = React.useElmishSimple init update
    React.router [
        router.onUrlChanged updateUrl
        router.children [
            match currentUrl with
            | [ "hello" ] -> Components.HelloWorld()
            | [ "counter" ] -> Components.Counter()
            | [ "arena" ] ->
                let frameArgs = {
                    className = "arena"
                    model = state
                    dispatch = dispatch
                    }
                DefaultFrame frameArgs (Arena (init, state.history))
            | otherwise ->
                AutoFight()
        ]
    ]

// originally from https://github.com/fable-compiler/fable-react/blob/master/docs/react-error-boundaries.md, but updated to Fable 4
module ReactErrorBoundary =
    open Fable.Core
    open Fable.React

    type [<AllowNullLiteral>] InfoComponentObject =
        abstract componentStack: string with get

    type ErrorBoundaryProps =
        {   Inner : React.ReactElement
            ErrorComponent : string -> React.ReactElement
            OnError : exn * InfoComponentObject -> unit }

    type ErrorBoundaryState =
        { Error : string option }

    // See https://github.com/MangelMaxime/Fulma/blob/master/docs/src/Widgets/Showcase.fs
    // See https://reactjs.org/docs/error-boundaries.html
    type ErrorBoundary(props) =
        inherit React.Component<ErrorBoundaryProps, ErrorBoundaryState>(props)
        do base.setInitState({ Error = None })

        override this.componentDidCatch(error, info) =
            let info = info :?> InfoComponentObject
            this.props.OnError(error, info)
            this.setState(fun _ _ -> { Error = Some (error.ToString()) })

        override x.render() =
            match x.state.Error with
            | Some err ->
                x.props.ErrorComponent err
            | None ->
                x.props.Inner

    // let ofType props children =
    //     ReactElementType.create ReactElementType.ofComponent<'T,_,_> props children
    let renderCatchSimple errorElement element =
        ReactElementType.create ReactElementType.ofComponent<ErrorBoundary,_,_> { Inner = element; ErrorComponent = errorElement; OnError = fun _ -> () } [ ]

    let renderCatchFn onError errorElement element =
        ReactElementType.create ReactElementType.ofComponent<ErrorBoundary,_,_> { Inner = element; ErrorComponent = errorElement; OnError = onError } [ ]

let main() =
    let err msg =
        class' "error" Html.div [
            Html.text $"There has been an error: {msg}"
            ]

    ReactErrorBoundary.renderCatchSimple err <|
        Html.div [
            classP' "srcLink" Html.a [
                prop.href "https://github.com/MaxWilson/POCArena/"
                prop.children [Html.img [prop.src "img/GitHub_Logo.png"]]
                prop.target "_blank"
                ]
            Router()
            ]

let root = ReactDOM.createRoot(document.getElementById "feliz-app")
root.render(Html.div [ main() ])
