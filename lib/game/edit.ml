open Common

open Logic
open Logic.Vec2
open Logic.Vec2.Vec2i
open Logic.Tile

open Option.Syntax

let edit global =
  let level = global.edited_level in

  let selected_tile = ref (Object Hi) in
  let in_sheet = ref false in

  let sheet = Array.make_matrix 20 20 None in 
  Tile.all |> List.iteri (fun i tile ->
    sheet.(i mod 20).(i / 20) <- Some tile     
  );

  print_endline "edit";

  fun () ->
    if R.is_key_pressed R.Key.Tab then in_sheet := not !in_sheet;

    let mouse_pos = R.get_mouse_position () in
    let mouse_cell_pos =
      (
        (mouse_pos |> vec2f_of_rl |> Vec2i.of_vec2f)
        -: { x = 0; y = 20 }
      )
      /: global.tile_size
    in

    if R.is_mouse_button_pressed R.MouseButton.Left then
      if !in_sheet then
        ignore begin
          let+ clicked_tile =
            try sheet.(mouse_cell_pos.x).(mouse_cell_pos.y)
            with Invalid_argument _ -> None
          in
          selected_tile := clicked_tile;
          in_sheet := false
        end
      else
        level.(mouse_cell_pos.x).(mouse_cell_pos.y).(0) <- Some (!selected_tile, Dir.Right);

    if R.is_mouse_button_pressed R.MouseButton.Right then
      level.(mouse_cell_pos.x).(mouse_cell_pos.y).(0) <- None;

    drawing (fun () ->
      R.clear_background R.Color.black;

      R.draw_text "Editing (E to switch to play mode, Tab to select tile)" 0 0 20 R.Color.gray;

      let mouse_cell_screen_pos = { x = 0; y = 20 } +: mouse_cell_pos *: global.tile_size in

      if !in_sheet then
        sheet |> Array.iteri (fun x column ->
          column |> Array.iteri (fun y tile' ->
            ignore begin
              let+ tile = tile' in
              draw_tile global ({ x = 0; y = 20 } +: { x; y } *: global.tile_size) tile
            end
          )
        )
      else begin
        level |> Array.iteri (fun x column ->
          column |> Array.iteri (fun y cell ->
            cell |> Array.iter (fun elem ->
              ignore begin
                let+ tile, dir = elem in
                draw_tile global ({ x = 0; y = 20 } +: { x; y } *: global.tile_size) tile
              end
            )
          )
        );

        draw_tile global mouse_cell_screen_pos ~alpha:127 !selected_tile
      end;

      R.draw_rectangle_lines
        mouse_cell_screen_pos.x mouse_cell_screen_pos.y
        global.tile_size global.tile_size
        R.Color.blue
    )