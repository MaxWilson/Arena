module UI.Data
open Domain.Data

type FightSetup = {
    sideA: TeamSetup
    sideB: Opposition
    }
    with static member fresh setPosition = { sideA = []; sideB = Opposition.calibrated (None, None, None, TPK) setPosition }
type Percent = float
type FightResult =
    | CalibratedResult of lower:(int * Percent) option * upper:(int * Percent) option * sample:CombatLog
    | SpecificResult of CombatLog * {| victors: int list |}

type Ruleset = DFRPG | ACKS
type DisplaySize = Zero | Small | Big
type Settings = {
    ruleset: Ruleset
    logEntriesDisplaySize: DisplaySize
    statsTableDisplaySize: DisplaySize
    battlegridDisplaySize: DisplaySize
    }
    with static member fresh = { ruleset = DFRPG; logEntriesDisplaySize = Small; statsTableDisplaySize = Small; battlegridDisplaySize = Small }