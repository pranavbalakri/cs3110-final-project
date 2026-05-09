(** Game world rendering.

    All drawing is performed through {!Gfx} primitives.  The coordinate
    system is converted from world space (y upward) to screen space
    (y downward, origin at top-left) by {!world_to_screen_y}.

    Call {!draw} once per frame between {!Gfx.begin_frame} and
    {!Gfx.end_frame}; it renders the complete scene including the HUD
    and any active overlays. *)

(** {1 Initialisation} *)

(** [load_textures ()] loads all sprite sheet textures from disk.
    Must be called once after the graphics context is initialised and
    before the first call to {!draw}. *)
val load_textures : unit -> unit

(** {1 Coordinate conversion} *)

(** [world_to_screen_y world_y height] converts a world-space y coordinate
    to a screen-space y coordinate.  [height] is the pixel height of the
    object being positioned (used to flip the bottom-left origin). *)
val world_to_screen_y : float -> int -> int

(** {1 Main draw call} *)

(** [draw game] renders one complete frame for [game]:
    {ol
      {li Clears the background.}
      {li Draws the tile grid.}
      {li Draws all entity layers (fans, teleporters, elevators, gates,
          buttons, levers, crates).}
      {li Draws diamonds and player characters.}
      {li Draws the HUD via {!Hud.draw}.}
      {li Draws the win banner when [game.status = Won].}
      {li Draws the signal debug overlay when [game.debug] is [true].}
    } *)
val draw : Game.t -> unit
