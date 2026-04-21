open Fwg

let () =
  Gfx.init ~width:800 ~height:600 ~title:"Fireboy & Watergirl";
  Gfx.set_target_fps 60;
  let bg = Gfx.rgb 20 22 38 in
  while not (Gfx.should_close ()) do
    Gfx.begin_frame ();
    Gfx.clear bg;
    Gfx.draw_text "fwg" ~x:12 ~y:10 ~size:20 Gfx.white;
    Gfx.end_frame ()
  done;
  Gfx.shutdown ()
