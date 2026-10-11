open Vec2
open Vec2i

type t

module Entry_id : sig
  type t

  val create : int -> t
  val to_int : t -> int
  val equal : t -> t -> bool
  val hash : t -> int
  val compare : t -> t -> int
end

module Tbl : Hashtbl.S with type key = Entry_id.t

module Entry : sig
  type t = private {
    mutable pos : Vec2i.t;
    mutable tile : Tile.t;
    mutable dir : Dir.t;
  }

  val create : Vec2i.t -> Tile.t -> Dir.t -> t
end

val create : int -> int -> t

val width : t -> int

val height : t -> int

val is_in_bounds : t -> Vec2i.t -> bool

val get_all_at : t -> Vec2i.t -> unit Tbl.t option

val get_by_id : t -> Entry_id.t -> Entry.t

val add_with_id : t -> Entry_id.t -> Entry.t -> unit

val add : t -> Entry.t -> Entry_id.t

val set_tile : t -> Entry_id.t -> Tile.t -> unit

val set_dir : t -> Entry_id.t -> Dir.t -> unit

val move : t -> Entry_id.t -> Vec2i.t -> unit

val remove : t -> Entry_id.t -> unit

val enumerate_seq : t -> (Vec2i.t * unit Tbl.t) Seq.t