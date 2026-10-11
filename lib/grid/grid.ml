open Vec2
open Vec2i

module Entry_id = Opaque_id

module Tbl = Hashtbl.Make(Entry_id)

module Entry = struct
  type t = {
    mutable pos : Vec2i.t;
    mutable tile : Tile.t;
    mutable dir : Dir.t;
  }

  let create pos tile dir = { pos; tile; dir }
end

open Entry

(*
  [matrix] contains references to the [tiles] hashtable. All IDs in [tiles]
  must occur exactly once in [matrix]. 
*)
type t = {
  matrix : unit Tbl.t array array;
  entries : Entry.t Tbl.t;
  mutable max_id : int;
}

let matrix_get grid { x; y } = grid.matrix.(x).(y)

let matrix_add grid pos id =
  Tbl.add (matrix_get grid pos) id ()

let matrix_remove grid pos id =
  Tbl.remove (matrix_get grid pos) id

(*** public stuff ***)

let create width height =
  if width = 0 || height = 0 then
    invalid_arg "Baba.Grid.create"
  else
    {
      matrix = Array.init_matrix width height (fun _ _ -> Tbl.create 8);
      entries = Tbl.create 128;
      max_id = 0;
    }

let width grid = Array.length grid.matrix

let height grid = Array.length grid.matrix.(0)

let is_in_bounds grid { x; y } =
  x >= 0 && x < width grid && y >= 0 && y < height grid

let get_by_id grid id = Tbl.find grid.entries id

let get_all_at grid pos =
  try
    Some (matrix_get grid pos)
  with
    Invalid_argument _ -> None

let add_with_id level id entry =
  matrix_add level entry.pos id;
  Tbl.replace level.entries id entry

let add level entry =
  let id = Entry_id.create level.max_id in
  level.max_id <- level.max_id + 1;
  add_with_id level id entry;
  id

let set_tile level id tile =
  (Tbl.find level.entries id).tile <- tile

let set_dir level id dir =
  (Tbl.find level.entries id).dir <- dir

let move level id dst =
  let { pos = src } as entry = Tbl.find level.entries id in
  if src = dst then ()
  else
    matrix_remove level src id;
    matrix_add level dst id;
    entry.pos <- dst

let remove level id =
  let { pos } = Tbl.find level.entries id in
  matrix_remove level pos id;
  Tbl.remove level.entries id

let enumerate_seq grid =
  let open Seq in
  ints 0 |> take (height grid) |> map (fun y ->
    ints 0 |> take (width grid) |> map (fun x ->
      let pos = { x; y } in
      pos, Option.get (get_all_at grid pos)
    )
  )
  |> concat