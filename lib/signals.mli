(** Signal graph evaluator for entity logic.

    Signals connect emitters (buttons, levers) to listeners (gates,
    fans) through a one-frame snapshot table.

    {b Protocol per frame:}
    {ol
      {li Call {!clear} at the start of the frame to reset the table.}
      {li Emitters call {!emit} for each signal they are asserting.}
      {li Listeners call {!eval} at any point after their inputs have been emitted.}
    } *)

(** {1 Signal expressions} *)

(** A boolean expression over named signal channels. *)
type expr =
  | Lit of Types.signal_id
      (** True iff [id] was emitted this frame. *)
  | Any of expr list
      (** True iff at least one sub-expression is true (logical OR). *)
  | All of expr list
      (** True iff all sub-expressions are true (logical AND). *)
  | Not of expr
      (** Logical negation of the sub-expression. *)

(** {1 Signal table} *)

(** A mutable per-frame snapshot mapping signal ids to their boolean state. *)
type table

(** [create ()] returns a new, empty signal table. *)
val create : unit -> table

(** [clear tbl] removes all emitted signals, resetting the table for a new frame. *)
val clear : table -> unit

(** [emit tbl id] marks [id] as true in [tbl] for the current frame. *)
val emit : table -> Types.signal_id -> unit

(** [eval tbl expr] recursively evaluates [expr] against the current snapshot
    in [tbl].  Returns [false] for any [Lit] whose id has not been emitted. *)
val eval : table -> expr -> bool
