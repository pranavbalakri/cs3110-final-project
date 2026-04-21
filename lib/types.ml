type tile =
  | Empty
  | Wall
  | Fire
  | Water
  | Goo
  | Fireboy_door
  | Watergirl_door
  | Ice
  | Conveyor_left
  | Conveyor_right
  | Spikes
  | Slope_up
  | Slope_down

type player_kind =
  | Fireboy
  | Watergirl

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