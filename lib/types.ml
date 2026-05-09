type tile =
  | Empty
  | Wall
  | Fire
  | Water
  | Goo
  | Firecaml_door
  | Watercaml_door
  | Ice
  | Conveyor_left
  | Conveyor_right
  | Spikes
  | Slope_up
  | Slope_down

type player_kind =
  | Firecaml
  | Watercaml

type direction =
  | Left
  | Right

type signal_id = string

type anim_state =
  | Idle
  | Walk
  | Jump
  | Fall
  | Die

type status =
  | Playing
  | Won
  | Lost
  | Paused