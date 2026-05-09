(** Heads-up display rendering.

    Draws per-player diamond counts and elapsed game time in the corners
    of the screen.  Called each frame from {!Render.draw} after the world
    scene has been painted. *)

(** [draw game] renders the HUD overlay for the current frame:
    {ul
      {li Fireboy's diamond count in the top-left, in orange.}
      {li Watergirl's diamond count below it, in blue.}
      {li Elapsed time in the top-right, in white.}
    } *)
val draw : Game.t -> unit
