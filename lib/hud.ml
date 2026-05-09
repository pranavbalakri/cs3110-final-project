let color_firecaml = Gfx.rgb 255 120 60
let color_watercaml = Gfx.rgb 60 120 255
let color_time = Gfx.rgb 255 255 255

let draw game =
  let firecaml_text =
    Printf.sprintf "Firecaml: %d" game.Game.firecaml.Player.diamonds
  in
  let watercaml_text =
    Printf.sprintf "Watercaml: %d" game.Game.watercaml.Player.diamonds
  in
  let time_text = Printf.sprintf "Time: %.1f" game.Game.elapsed in
  Gfx.draw_text firecaml_text ~x:10 ~y:10 ~size:20 color_firecaml;
  Gfx.draw_text watercaml_text ~x:10 ~y:35 ~size:20 color_watercaml;
  Gfx.draw_text time_text ~x:700 ~y:10 ~size:20 color_time
