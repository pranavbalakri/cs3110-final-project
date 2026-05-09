(** Immutable 2-D floating-point vector.

    All arithmetic produces new values; no mutation.  The coordinate
    system matches the game's world space: x increases rightward,
    y increases upward. *)

(** A 2-D vector with components [x] and [y]. *)
type t = { x : float; y : float }

(** The zero vector [{x = 0.; y = 0.}]. *)
val zero : t

(** [add a b] returns the component-wise sum [a + b]. *)
val add : t -> t -> t

(** [sub a b] returns the component-wise difference [a - b]. *)
val sub : t -> t -> t

(** [scale k v] returns the scalar multiple [k * v]. *)
val scale : float -> t -> t

(** [length v] returns the Euclidean length of [v]. *)
val length : t -> float
