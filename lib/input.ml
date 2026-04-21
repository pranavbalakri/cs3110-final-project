type player_input = {
  left : bool;
  right : bool;
  jump : bool;
  jump_pressed : bool;
}

type t = { fireboy : player_input; watergirl : player_input; reset : bool }

let poll () =
  let fireboy =
    {
      left = Gfx.is_key_down Gfx.key_a;
      right = Gfx.is_key_down Gfx.key_d;
      jump = Gfx.is_key_down Gfx.key_w;
      jump_pressed = Gfx.is_key_pressed Gfx.key_w;
    }
  in
  let watergirl =
    {
      left = Gfx.is_key_down Gfx.key_j;
      right = Gfx.is_key_down Gfx.key_l;
      jump = Gfx.is_key_down Gfx.key_i;
      jump_pressed = Gfx.is_key_pressed Gfx.key_i;
    }
  in
  let reset = Gfx.is_key_pressed Gfx.key_r in
  { fireboy; watergirl; reset }
