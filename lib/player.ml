type t = {
  kind : Types.player_kind;
  mutable pos : Vec2.t;
  mutable vel : Vec2.t;
  mutable on_ground : bool;
  mutable alive : bool;
  mutable at_door : bool;
  mutable diamonds : int;
}

let create kind spawn_pos =
  {
    kind;
    pos = spawn_pos;
    vel = Vec2.zero;
    on_ground = false;
    alive = true;
    at_door = false;
    diamonds = 0;
  }

let width = float_of_int Tuning.player_w
let height = float_of_int Tuning.player_h

let bbox p =
  let half_w = width /. 2. in
  let x = p.pos.x -. half_w in
  let y = p.pos.y in
  (x, y, width, height)

let center p =
  let half_h = height /. 2. in
  { Vec2.x = p.pos.x; y = p.pos.y +. half_h }

let apply_input p (inp : Input.player_input) =
  if not p.alive then ()
  else begin
    let dx =
      (if inp.left then -.Tuning.walk_speed else 0.)
      +. if inp.right then Tuning.walk_speed else 0.
    in
    p.vel <- { p.vel with Vec2.x = dx };
    if inp.jump_pressed && p.on_ground then
      p.vel <- { p.vel with Vec2.y = Tuning.jump_impulse }
  end

let apply_gravity p =
  if not p.alive then ()
  else begin
    let new_vy = p.vel.y -. Tuning.gravity in
    let capped_vy = max (-.Tuning.max_fall) new_vy in
    p.vel <- { p.vel with Vec2.y = capped_vy }
  end

let reset p spawn_pos =
  p.pos <- spawn_pos;
  p.vel <- Vec2.zero;
  p.on_ground <- false;
  p.alive <- true;
  p.at_door <- false;
  p.diamonds <- 0
