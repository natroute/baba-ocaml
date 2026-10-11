open Grid

type t = { grid : Grid.t }

module Cmd : sig
  type t = unit -> unit -> unit
end

val step : t -> Dir.t -> Cmd.t list

val example : unit -> t