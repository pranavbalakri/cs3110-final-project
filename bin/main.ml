open Fwg

let () =
  Gfx.init ~width:800 ~height:600 ~title:"Fireboy & Watergirl";
  Gfx.set_target_fps 60;
  let state = ref (Game.init Level.test_level) in
  while not (Gfx.should_close ()) do
    Gfx.begin_frame ();
    let inp = Input.poll () in
    state := Game.update !state inp (1.0 /. 60.0);
    Render.draw !state;
    Gfx.end_frame ()
  done;
  Gfx.shutdown ()
