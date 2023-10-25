module UI.Components.SettingsView

open UI.Data
open Feliz

[<ReactComponent>]
let Settings (model: Settings, header: ReactElement, updateSettings) =
    class' "settingsView" Html.div [
        header
        let checkbox (category, getValue, setValue) (id: string) (label: string) value = Html.div [
            Html.input [prop.type'.radio; prop.name category; prop.id id; prop.isChecked (getValue() = value); prop.onClick (fun _ -> setValue value); prop.readOnly true]
            Html.label [prop.for' id; prop.text label]
            ]
        let setting (category: string, getValue, setValue) contents =
            class' "setting" Html.div [
                Html.h3 $"{category}: "
                yield! contents {| checkbox = checkbox (category, getValue, setValue) |}
                ]
        setting ("Ruleset", thunk model.ruleset, fun v -> { model with ruleset = v } |> updateSettings) (fun api -> [
            api.checkbox "chk_dfrpg" "Dungeon Fantasy RPG" DFRPG
            api.checkbox "chk_acks" "Adventurer, Conqueror, King System" ACKS
            ])
        setting ("Battlegrid display", thunk model.battlegridDisplaySize, fun v -> { model with battlegridDisplaySize = v } |> updateSettings) (fun api -> [
            api.checkbox "chk_bg_none" "None" Zero
            api.checkbox "chk_bg_small" "Small" Small
            api.checkbox "chk_bg_big" "Big" Big
            ])
        // these settings are ugly, not ready for release yet even though the data structures are there. Needs more CSS work, see https://github.com/MaxWilson/Arena/issues/17. Leaving the code in place but commented-out for visibility.
        // setting ("Log entry display", thunk model.logEntriesDisplaySize, fun v -> { model with logEntriesDisplaySize = v } |> updateSettings) (fun api -> [
        //     api.checkbox "chk_log_none" "None" Zero
        //     api.checkbox "chk_log_small" "Small" Small
        //     api.checkbox "chk_log_big" "Big" Big
        //     ])
        // setting ("Stats table display", thunk model.statsTableDisplaySize, fun v -> { model with statsTableDisplaySize = v } |> updateSettings) (fun api -> [
        //     api.checkbox "chk_stats_none" "None" Zero
        //     api.checkbox "chk_stats_small" "Small" Small
        //     api.checkbox "chk_stats_big" "Big" Big
        //     ])
        ]
