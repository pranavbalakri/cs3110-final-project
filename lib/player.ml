(** Ground-surface under the player's feet this frame.
    Drives horizontal friction/carry behaviour in [apply_input]. *)
type ground_surface =
  | Normal
  | Ice
  | Conveyor_belt of float  (** Signed horizontal velocity added per frame. *)

type t = {
  kind : Types.player_kind;
  mutable pos : Vec2.t;
  mutable vel : Vec2.t;
  mutable on_ground : bool;
  mutable coyote_left : int;
  mutable jump_buffer_left : int;
  mutable jump_hold_left : int;
  mutable alive : bool;
  mutable at_door : bool;
  mutable diamonds : int;
  mutable ground_surface : ground_surface;
  mutable in_fan : bool;
  mutable teleport_cooldown : float;
}

let create kind spawn_pos =
  {
    kind;
    pos = spawn_pos;
    vel = Vec2.zero;
    on_ground = false;
    coyote_left = 0;
    jump_buffer_left = 0;
    jump_hold_left = 0;
    alive = true;
    at_door = false;
    diamonds = 0;
    ground_surface = Normal;
    in_fan = false;
    teleport_cooldown = 0.;
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

let dec_to_zero n = if n <= 0 then 0 else n - 1

let apply_input p (inp : Input.player_input) =
  if not p.alive then ()
  else begin
    if p.on_ground then p.coyote_left <- Tuning.coyote_frames
    else p.coyote_left <- dec_to_zero p.coyote_left;

    if inp.jump_pressed then p.jump_buffer_left <- Tuning.jump_buffer_frames
    else p.jump_buffer_left <- dec_to_zero p.jump_buffer_left;

    let has_input = inp.left || inp.right in
    let input_x =
      (if inp.left then -.Tuning.walk_speed else 0.)
      +. if inp.right then Tuning.walk_speed else 0.
    in
    let vx =
      match p.ground_surface with
      | Normal -> input_x
      | Ice ->
          if has_input then input_x
          else p.vel.x *. Tuning.ice_friction
      | Conveyor_belt cv ->
          (if has_input then input_x else 0.) +. cv
    in
    p.vel <- { p.vel with Vec2.x = vx };

    let can_jump = p.on_ground || p.coyote_left > 0 in
    if p.jump_buffer_left > 0 && can_jump then begin
      p.vel <- { p.vel with Vec2.y = Tuning.jump_impulse };
      p.on_ground <- false;
      p.coyote_left <- 0;
      p.jump_buffer_left <- 0;
      p.jump_hold_left <- Tuning.jump_hold_frames
    end;

    if inp.jump && p.jump_hold_left > 0 && p.vel.y > 0. then begin
      p.vel <-
        {
          p.vel with
          Vec2.y = Float.min Tuning.jump_impulse (p.vel.y +. Tuning.jump_hold_boost);
        };
      p.jump_hold_left <- p.jump_hold_left - 1
    end
    else if not inp.jump then p.jump_hold_left <- 0
  end

let apply_gravity p =
  if not p.alive then ()
  else if p.in_fan then begin
    let new_vy = p.vel.y +. Tuning.fan_lift in
    let capped = Float.min Tuning.fan_max_up_vy new_vy in
    p.vel <- { p.vel with Vec2.y = capped }
  end
  else begin
    let new_vy = p.vel.y -. Tuning.gravity in
    let capped_vy = Float.max (-.Tuning.max_fall) new_vy in
    p.vel <- { p.vel with Vec2.y = capped_vy }
  end

let reset p spawn_pos =
  p.pos <- spawn_pos;
  p.vel <- Vec2.zero;
  p.on_ground <- false;
  p.coyote_left <- 0;
  p.jump_buffer_left <- 0;
  p.jump_hold_left <- 0;
  p.alive <- true;
  p.at_door <- false;
  p.diamonds <- 0;
  p.ground_surface <- Normal;
  p.in_fan <- false;
  p.teleport_cooldown <- 0.
