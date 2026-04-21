let color_fireboy = Gfx.rgb 255 120 60
let color_watergirl = Gfx.rgb 60 120 255
let color_time = Gfx.rgb 255 255 255

let draw game =
  let fireboy_text =
    Printf.sprintf "Fireboy: %d" game.Game.fireboy.Player.diamonds
  in
  let watergirl_text =
    Printf.sprintf "Watergirl: %d" game.Game.watergirl.Player.diamonds
  in
  let time_text = Printf.sprintf "Time: %.1f" game.Game.elapsed in
  Gfx.draw_text fireboy_text ~x:10 ~y:10 ~size:20 color_fireboy;
  Gfx.draw_text watergirl_text ~x:10 ~y:35 ~size:20 color_watergirl;
  Gfx.draw_text time_text ~x:700 ~y:10 ~size:20 color_time
