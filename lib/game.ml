module TileId : sig
  type t
  val create : int -> t
  val to_int : t -> int
  val equal : t -> t -> bool
  val hash : t -> int
  val compare : t -> t -> int
end = struct
  type t = int
  let create x = x
  let to_int x = x
  let equal = Int.equal
  let hash = Hashtbl.hash
  let compare = Int.compare
end

module TileIdHashtbl = Hashtbl.Make(TileId)

