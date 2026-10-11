open Logic
open Logic.Level
open Logic.Grid.Entry
open Logic.Vec2
open Logic.Vec2.Vec2i
open Logic.Vec2.Vec2f

open Common

open Option.Syntax

let edited_level_to_grid level =
  let grid = Grid.create (Array.length level) (Array.length level.(0)) in
  level |> Array.iteri (fun x column ->
    column |> Array.iteri (fun y cell ->
      cell |> Array.iter (fun elem ->
        match elem with
        | Some (tile, dir) ->
            Grid.add grid (Grid.Entry.create { x; y } tile dir) |> ignore
        | None -> ()
      )  
    )
  );
  grid

let play global =
  let level = { grid = edited_level_to_grid global.edited_level } in
  let undo_stack = Stack.create () in

  fun () ->
    let dir_input key dir =
      if R.is_key_pressed key then
        undo_stack |> Stack.push (Level.step level dir)
    in
    dir_input R.Key.Left Dir.Left;
    dir_input R.Key.Right Dir.Right;
    dir_input R.Key.Up Dir.Up;
    dir_input R.Key.Down Dir.Down;

    if R.is_key_pressed R.Key.Z then
      ignore begin
        let+ undos = Stack.pop_opt undo_stack in
        undos |> List.iter (fun undo -> undo ())
      end;

    drawing (fun () ->
      R.clear_background R.Color.black;

      R.draw_text "Playing (E to switch to edit mode)" 0 0 20 R.Color.gray;

      Grid.enumerate_seq level.grid |> Seq.iter (fun (pos, ids) ->
        Grid.Tbl.to_seq_keys ids |> List.of_seq
        |> List.map (fun id -> Grid.get_by_id level.grid id)
        |> List.sort (fun { tile = a } { tile = b } ->
          let open Tile_info in
          compare (tile_z_index a) (tile_z_index b)
        )
        |> List.iter (fun { tile } ->
          draw_tile global ({ x = 0; y = 20 } +: pos *: global.tile_size) tile
        )
      )
    )