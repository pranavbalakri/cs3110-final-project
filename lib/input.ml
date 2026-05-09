type player_input = {
  left : bool;
  right : bool;
  jump : bool;
  jump_pressed : bool;
  interact_pressed : bool;  (** edge-triggered; used by levers *)
}

type t = {
  fireboy : player_input;
  watergirl : player_input;
  reset : bool;
  debug_toggle : bool;      (** F3 — toggles signal debug overlay *)
}

let poll () =
  let fireboy =
    {
      left = Gfx.is_key_down Gfx.key_a;
      right = Gfx.is_key_down Gfx.key_d;
      jump = Gfx.is_key_down Gfx.key_w;
      jump_pressed = Gfx.is_key_pressed Gfx.key_w;
      interact_pressed = Gfx.is_key_pressed Gfx.key_e;
    }
  in
  let watergirl =
    {
      left = Gfx.is_key_down Gfx.key_j;
      right = Gfx.is_key_down Gfx.key_l;
      jump = Gfx.is_key_down Gfx.key_i;
      jump_pressed = Gfx.is_key_pressed Gfx.key_i;
      interact_pressed = Gfx.is_key_pressed Gfx.key_o;
    }
  in
  let reset = Gfx.is_key_pressed Gfx.key_r in
  let debug_toggle = Gfx.is_key_pressed Gfx.key_f3 in
  { fireboy; watergirl; reset; debug_toggle }
