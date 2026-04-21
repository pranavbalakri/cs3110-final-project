type t = {
  pos : Vec2.t;
  kind : Types.player_kind;
  mutable collected : bool;
}

let size = 20.0

let from_spec (spec : Level.diamond_spec) =
  let ts = float_of_int Tuning.tile_size in
  let x = (float_of_int spec.col *. ts) +. (ts /. 2.) in
  let y = (float_of_int spec.row *. ts) +. (ts /. 2.) in
  { pos = { Vec2.x; y }; kind = spec.kind; collected = false }

let bbox d =
  let half = size /. 2. in
  (d.pos.x -. half, d.pos.y -. half, size, size)

let rects_overlap (x1, y1, w1, h1) (x2, y2, w2, h2) =
  x1 < x2 +. w2 && x1 +. w1 > x2 && y1 < y2 +. h2 && y1 +. h1 > y2

let try_collect d (p : Player.t) =
  if d.collected then ()
  else if d.kind <> p.kind then ()
  else if not p.alive then ()
  else begin
    let d_bbox = bbox d in
    let p_bbox = Player.bbox p in
    if rects_overlap d_bbox p_bbox then begin
      d.collected <- true;
      p.diamonds <- p.diamonds + 1
    end
  end

let reset d = d.collected <- false
