open Grid

type t = { grid : Grid.t }

val step : t -> Dir.t -> (unit -> unit) list

val example : unit -> Grid.t