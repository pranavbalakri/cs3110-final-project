(** Level-select menu. Owns a cursor index into [Level.levels];
    [draw] renders the menu screen and the navigation/selection helpers
    move the cursor or report the selection. *)

type t = { mutable cursor : int }

let create () = { cursor = 0 }

let num_levels = Array.length Level.levels

(** Move cursor up; clamps at the top entry. *)
let move_up (m : t) = if m.cursor > 0 then m.cursor <- m.cursor - 1

(** Move cursor down; clamps at the last entry. *)
let move_down (m : t) =
  if m.cursor < num_levels - 1 then m.cursor <- m.cursor + 1

(** Currently highlighted level data. *)
let selected_level (m : t) = snd Level.levels.(m.cursor)

(** Render the menu to the current frame. *)
let draw (m : t) =
  Gfx.clear (Gfx.rgb 20 20 30);
  Gfx.draw_text "Firecaml & Watercaml" ~x:200 ~y:80 ~size:36
    (Gfx.rgb 230 200 100);
  Gfx.draw_text "Select a level" ~x:300 ~y:140 ~size:20
    (Gfx.rgb 200 200 200);
  Array.iteri
    (fun i (name, _) ->
      let y = 220 + (i * 60) in
      let highlighted = i = m.cursor in
      let bg =
        if highlighted then Gfx.rgb 80 80 120 else Gfx.rgb 40 40 60
      in
      let fg =
        if highlighted then Gfx.rgb 255 255 100 else Gfx.rgb 200 200 200
      in
      Gfx.draw_rect ~x:250 ~y ~w:300 ~h:40 bg;
      Gfx.draw_text name ~x:280 ~y:(y + 10) ~size:22 fg)
    Level.levels;
  Gfx.draw_text "W/S to navigate, Enter to start, Q to return"
    ~x:160 ~y:540 ~size:14
    (Gfx.rgb 150 150 150)
