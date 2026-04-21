(** Thin abstraction over the underlying graphics/input/audio backend
    (currently raylib). The rest of the codebase must not import
    [Raylib] directly; go through [Gfx] so the backend can be swapped
    (e.g. to tsdl) without touching the game. *)

(** {1 Colors} *)

type color

val rgb : int -> int -> int -> color
val rgba : int -> int -> int -> int -> color
val black : color
val white : color
val transparent : color

(** {1 Window lifecycle} *)

val init : width:int -> height:int -> title:string -> unit
val shutdown : unit -> unit
val should_close : unit -> bool
val set_target_fps : int -> unit
val screen_width : unit -> int
val screen_height : unit -> int

(** {1 Frame} *)

val begin_frame : unit -> unit
val end_frame : unit -> unit
val clear : color -> unit

(** {1 Drawing primitives} *)

val draw_rect : x:int -> y:int -> w:int -> h:int -> color -> unit
val draw_rect_lines : x:int -> y:int -> w:int -> h:int -> color -> unit
val draw_circle : cx:int -> cy:int -> radius:float -> color -> unit
val draw_poly :
  cx:int ->
  cy:int ->
  sides:int ->
  radius:float ->
  rotation:float ->
  color ->
  unit
val draw_text : string -> x:int -> y:int -> size:int -> color -> unit

(** {1 Textures} *)

type texture

val load_texture : string -> texture
val unload_texture : texture -> unit
val draw_texture : texture -> x:int -> y:int -> color -> unit

(** {1 Input}

    Keys are exposed as opaque values. Add new constants here as the
    game needs them. Polling is edge- ([is_key_pressed]) or
    level-triggered ([is_key_down]). *)

type key

val key_a : key
val key_b : key
val key_d : key
val key_e : key
val key_i : key
val key_j : key
val key_k : key
val key_l : key
val key_n : key
val key_p : key
val key_q : key
val key_r : key
val key_s : key
val key_w : key
val key_semicolon : key
val key_space : key
val key_enter : key
val key_escape : key
val key_f3 : key
val key_f11 : key

val is_key_down : key -> bool
val is_key_pressed : key -> bool

(** {1 Audio (stubs)} *)

type sound

val load_sound : string -> sound
val unload_sound : sound -> unit
val play_sound : sound -> unit
val set_master_volume : float -> unit
