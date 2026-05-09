open Fwg

let () =
  Gfx.init ~width:800 ~height:600 ~title:"Fireboy & Watergirl";
  Gfx.set_target_fps 60;
  let menu = Menu.create () in
  let game : Game.t option ref = ref None in
  while not (Gfx.should_close ()) do
    Gfx.begin_frame ();
    (match !game with
     | None ->
         if Gfx.is_key_pressed Gfx.key_w then Menu.move_up menu;
         if Gfx.is_key_pressed Gfx.key_s then Menu.move_down menu;
         if Gfx.is_key_pressed Gfx.key_enter then
           game := Some (Game.init (Menu.selected_level menu));
         Menu.draw menu
     | Some g ->
         if Gfx.is_key_pressed Gfx.key_q then game := None
         else begin
           let inp = Input.poll () in
           let g' = Game.update g inp (1.0 /. 60.0) in
           Render.draw g';
           game := Some g'
         end);
    Gfx.end_frame ()
  done;
  Gfx.shutdown ()
