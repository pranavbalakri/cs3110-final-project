(** Signal graph evaluator.
    Each frame, emitters call [emit] to set signals true in the [table].
    At end-of-frame, listeners call [eval] to read the snapshot.
    The table is cleared at the start of each frame via [clear]. *)

type expr =
  | Lit of Types.signal_id
  | Any of expr list
  | All of expr list
  | Not of expr

type table = (Types.signal_id, bool) Hashtbl.t

let create () : table = Hashtbl.create 16

let clear (tbl : table) = Hashtbl.clear tbl

let emit (tbl : table) id = Hashtbl.replace tbl id true

let eval (tbl : table) expr =
  let rec go = function
    | Lit id -> Option.value (Hashtbl.find_opt tbl id) ~default:false
    | Any exprs -> List.exists go exprs
    | All exprs -> List.for_all go exprs
    | Not e -> not (go e)
  in
  go expr
