open Logic
open Logic.Grid
open Logic.Grid.Entry
open Logic.Vec2
open Logic.Tile

module R = Raylib

let vec2f_to_rl { Vec2f.x; y } = R.Vector2.create x y

let vec2f_of_rl v = { Vec2f.x = R.Vector2.x v; y = R.Vector2.y v }

type global = {
  tile_size : int;
  texture_size : int;
  texture_scale : float;
  sprites : (Object.t, R.Texture.t) Hashtbl.t;
  edited_level : (Tile.t * Dir.t) option array array array;
}

let drawing f =
  R.begin_drawing ();
  f ();
  R.end_drawing ()

let draw_text_centered text pos_x pos_y font_size color =
  let width = R.measure_text text font_size in
  R.draw_text text (pos_x - width / 2) pos_y font_size color

let draw_object global pos ?(alpha = 255) obj =
  R.draw_texture_ex
    (Hashtbl.find global.sprites obj)
    (vec2f_to_rl (Vec2f.of_vec2i pos))
    0.
    global.texture_scale
    (R.Color.create 255 255 255 alpha)

let draw_word global pos ?(alpha = 255) word =
  let open Vec2i in

  let text, (r, g, b) = Tile_info.word_to_text_and_color word in
  let color = R.Color.create r g b alpha in

  let fg_color, bg_color =
    match word with
    | Prop _ -> R.Color.black, color
    | _ -> color, R.Color.black
  in

  R.draw_rectangle
    pos.x pos.y
    global.tile_size global.tile_size
    bg_color;

  let draw_text text offset_y =
    draw_text_centered
      text
      (pos.x + global.tile_size / 2) (pos.y + 2 + offset_y)
      20
      fg_color
  in

  begin match text with
  | Either.Left line ->
      draw_text line (global.tile_size / 4)
  | Either.Right (line_1, line_2) ->
      draw_text line_1 0;
      draw_text line_2 (global.tile_size / 2)
  end

let draw_tile global pos ?(alpha = 255) tile =
  let open Vec2i in
  match tile with
  | Word word -> draw_word global pos ~alpha word
  | Object obj -> draw_object global pos ~alpha obj
