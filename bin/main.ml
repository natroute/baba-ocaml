open Baba
open Grid
open Entry
open Level.Cmd
open Vec2
open Vec2i
open Vec2f
open Option.Syntax

module R = Raylib

let vec2f_to_rl ({ x; y }: Vec2f.t) = R.Vector2.create x y

let tile_size = 48
let texture_size = 64

let drawing f =
  R.begin_drawing ();
  f ();
  R.end_drawing ()

let draw_text_centered text pos_x pos_y font_size color =
  let width = R.measure_text text font_size in
  R.draw_text text (pos_x - width / 2) pos_y font_size color

let () =
  let texture_scale = (Float.of_int tile_size) /. (Float.of_int texture_size) in
  let level = Level.example () in

  R.init_window
    ((Grid.width level.grid) * tile_size)
    ((Grid.height level.grid) * tile_size)
    "blum";
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

  let draw_tile (pos : Vec2i.t) tile =
    let screen_pos = pos *: tile_size in

    match (tile : Tile.t) with
    | Word word ->
        let text, (r, g, b) = Tile_info.word_to_text_and_color word in
        let color = R.Color.create r g b 255 in

        let fg_color, bg_color =
          match word with
          | Prop _ -> R.Color.black, color
          | _ -> color, R.Color.black
        in

        R.draw_rectangle screen_pos.x screen_pos.y tile_size tile_size bg_color;

        let draw_text text offset_y =
          draw_text_centered
            text
            (screen_pos.x + tile_size / 2) (screen_pos.y + offset_y)
            20
            fg_color
        in

        begin match text with
        | Either.Left line ->
            draw_text line (tile_size / 4)
        | Either.Right (line_1, line_2) ->
            draw_text line_1 0;
            draw_text line_2 (tile_size / 2)
        end
    | Obj obj ->
        R.draw_texture_ex
          (Hashtbl.find sprites obj)
          (vec2f_to_rl (Vec2f.of_vec2i screen_pos))
          0.
          texture_scale
          R.Color.white
  in

  let loop () =
    let undo_stack = Stack.create () in

    while not (R.window_should_close ()) do
      R.clear_background R.Color.black;

      let dir_input key dir =
        if R.is_key_pressed key then
          let undos =
            Level.step level dir
            |> List.map (fun cmd -> cmd ())
          in
          undo_stack |> Stack.push undos
      in
      dir_input R.Key.Left Dir.Left;
      dir_input R.Key.Right Dir.Right;
      dir_input R.Key.Up Dir.Up;
      dir_input R.Key.Down Dir.Down;

      if R.is_key_pressed R.Key.Z then
        ignore begin
          let+ undos = Stack.pop_opt undo_stack in
          undos |> List.iter ((|>) ())
        end;

      drawing (fun () ->
        Grid.enumerate_seq level.grid |> Seq.iter (fun (pos, ids) ->
          Grid.Tbl.to_seq_keys ids |> List.of_seq
          |> List.map (fun id -> Grid.get_by_id level.grid id)
          |> List.sort (fun { tile = a } { tile = b } ->
            let open Tile_info in
            compare (tile_z_index a) (tile_z_index b)
          )
          |> List.iter (fun { tile } ->
            draw_tile pos tile
          )
        )
      )
    done;
    R.close_window ()
  in
  loop ()