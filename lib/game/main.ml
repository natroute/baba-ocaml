open Logic
open Logic.Grid
open Logic.Grid.Entry

open Common

open Option.Syntax

let start () =
  R.init_window 960 960 "blum";
  R.set_target_fps 60;

  let sprites =
    Tile_info.objs_to_names
    |> Hashtbl.to_seq
    |> Seq.map (fun (obj, name) ->
      let tex = R.load_texture ("share/sprites/" ^ name ^ ".png") in
      R.set_texture_filter tex R.TextureFilter.Bilinear;
      obj, tex
    )
    |> Hashtbl.of_seq
  in

  let tile_size = 48 in
  let texture_size = 64 in 
  let global = {
    tile_size; texture_size;
    texture_scale = (Float.of_int tile_size) /. (Float.of_int texture_size);
    sprites;
    edited_level = Array.init_matrix 20 20 (fun _ _ -> Array.make 4 None)
  } in

  let play = ref false in
  let loop = ref (Edit.edit global) in
  while not (R.window_should_close ()) do
    if R.is_key_pressed R.Key.E then begin
      play := not !play;
      loop := (if !play then Play.play else Edit.edit) global
    end;

    !loop ()
  done;

  R.close_window ()