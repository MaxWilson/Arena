module Main

open Feliz
open Feliz.Router
open Feliz.Listeners
open UI.Components.Sample
open UI.Components.AutoFightView
open UI.Components.Arena
open UI.Components.ArenaView
open UI.Data
open Browser.Dom
open Fable
open Fable.Core.JsInterop

importSideEffects "../main.sass"

[<ReactComponent>]
let Arena() =
    let state, dispatch = React.useElmishSimple init update
    let frameArgs = {
        className = "arena"
        model = state
        dispatch = dispatch
        }
    DefaultFrame frameArgs (Arena0 (init, state.history))

[<ReactComponent>]
let Router() =
    let (currentUrl, updateUrl) = React.useState(Router.currentUrl())
    let settings, changeSettings = React.useState (UI.LocalStorage.Settings.read()) |> Tuple2.mapsnd (fun f -> UI.LocalStorage.Settings.write >> f)
    React.router [
        router.onUrlChanged updateUrl
        router.children [
            let header selected =
                class' "header" Html.div [
                    for link, dest in [ "Autofight", "autofight"; "Interactive", "arena"; "Adventure", "adventure"; "Campaign", "campaign"; "Settings", "settings" ] do
                        if selected = link then
                            classP' "internalLink" Html.b [ prop.children [Html.text link] ]
                        elif (link = "Adventure") then // we want to give an early warning BEFORE changing the URL
                            classP' "internalLink" Html.a [prop.href ("#" + dest); prop.children [Html.text link]; prop.custom("data-text", link); prop.onClick (fun ev -> ev.preventDefault(); notImpl $"{link} mode" )]
                        else classP' "internalLink" Html.a [prop.href ("#" + dest); prop.children [Html.text link]; prop.custom("data-text", link) ]
                    classP' "srcLink" Html.a [
                        prop.href "https://github.com/MaxWilson/Arena/"
                        prop.children [Html.img [prop.ariaLabel "GitHub"; prop.src "img/GitHub_Logo.png"]]
                        prop.target "_blank"
                        ]
                    Html.div [
                        Html.h2 "Shining Sword Arena!"
                        match settings.ruleset with
                        | UI.Data.DFRPG ->
                            Html.h3 "For Dungeon Fantasy RPG and GURPS"
                        | UI.Data.ACKS ->
                            Html.h3 "For Adventurer, Conqueror, King System"
                        ]
                    ]
            match currentUrl with
            | [ "hello" ] -> Components.HelloWorld()
            | [ "counter" ] -> Components.Counter()
            | [ "both" ] -> Components.HelloWorld(); Components.Counter()
            | [ "arena" ] ->
                header "Interactive"
                Html.h4 [Html.i "(Under construction)"]
                Arena()
            | [ "adventure" ] -> notImpl "Adventure mode"
            | [ "campaign" ] ->
                UI.Components.CampaignView.Campaign(header "Campaign")
            | [ "settings" ] ->
                UI.Components.SettingsView.Settings(settings, header "Settings", changeSettings)
            | otherwise ->
                AutoFight(settings, header "Autofight")
            ]
        ]

let main() =
    ReactErrorBoundary.renderCatchSimple ReactErrorBoundary.err <| Router()

let root = ReactDOM.createRoot(document.getElementById "feliz-app")
root.render(Html.div [ main() ])
