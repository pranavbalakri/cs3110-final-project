module R = Raylib

type color = R.Color.t

let rgb r g b = R.Color.create r g b 255
let rgba r g b a = R.Color.create r g b a
let black = rgb 0 0 0
let white = rgb 255 255 255
let transparent = rgba 0 0 0 0

let init ~width ~height ~title =
  R.init_window width height title

let shutdown () = R.close_window ()
let should_close () = R.window_should_close ()
let set_target_fps n = R.set_target_fps n
let screen_width () = R.get_screen_width ()
let screen_height () = R.get_screen_height ()

let begin_frame () = R.begin_drawing ()
let end_frame () = R.end_drawing ()
let clear c = R.clear_background c

let draw_rect ~x ~y ~w ~h c = R.draw_rectangle x y w h c
let draw_rect_lines ~x ~y ~w ~h c = R.draw_rectangle_lines x y w h c
let draw_circle ~cx ~cy ~radius c = R.draw_circle cx cy radius c

let draw_line ~x1 ~y1 ~x2 ~y2 ~thickness c =
  R.draw_line_ex
    (R.Vector2.create (float_of_int x1) (float_of_int y1))
    (R.Vector2.create (float_of_int x2) (float_of_int y2))
    thickness c

let draw_poly ~cx ~cy ~sides ~radius ~rotation c =
  R.draw_poly (R.Vector2.create (float_of_int cx) (float_of_int cy))
    sides radius rotation c

let draw_text s ~x ~y ~size c = R.draw_text s x y size c

type texture = R.Texture.t

let load_texture path = R.load_texture path
let unload_texture t = R.unload_texture t
let draw_texture t ~x ~y c = R.draw_texture t x y c

let texture_width t = R.Texture.width t
let texture_height t = R.Texture.height t

let draw_texture_pro t ~src_x ~src_y ~src_w ~src_h ~dst_x ~dst_y ~dst_w ~dst_h c
    =
  let src =
    R.Rectangle.create (float_of_int src_x) (float_of_int src_y)
      (float_of_int src_w) (float_of_int src_h)
  in
  let dst =
    R.Rectangle.create (float_of_int dst_x) (float_of_int dst_y)
      (float_of_int dst_w) (float_of_int dst_h)
  in
  let origin = R.Vector2.create 0. 0. in
  R.draw_texture_pro t src dst origin 0. c

type key = R.Key.t

let key_a = R.Key.A
let key_b = R.Key.B
let key_d = R.Key.D
let key_e = R.Key.E
let key_i = R.Key.I
let key_j = R.Key.J
let key_k = R.Key.K
let key_l = R.Key.L
let key_n = R.Key.N
let key_o = R.Key.O
let key_p = R.Key.P
let key_q = R.Key.Q
let key_r = R.Key.R
let key_s = R.Key.S
let key_w = R.Key.W
let key_semicolon = R.Key.Semicolon
let key_space = R.Key.Space
let key_enter = R.Key.Enter
let key_escape = R.Key.Escape
let key_f3 = R.Key.F3
let key_f11 = R.Key.F11

let is_key_down k = R.is_key_down k
let is_key_pressed k = R.is_key_pressed k

type sound = R.Sound.t

let load_sound path = R.load_sound path
let unload_sound s = R.unload_sound s
let play_sound s = R.play_sound s
let set_master_volume v = R.set_master_volume v
