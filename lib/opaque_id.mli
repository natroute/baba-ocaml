type t = int

val create : int -> t
val to_int : t -> int
val equal : t -> t -> bool
val hash : t -> int
val compare : t -> t -> int
