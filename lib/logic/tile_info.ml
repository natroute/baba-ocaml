open Tile
open Tile.Word

let objs_to_names =
  Hashtbl.of_seq (List.to_seq Tile.Object.[
    (Blum, "baba");
    (Flag, "flag");
    (Rock, "rock");
    (Wall, "wall");
    (Hi, "hi");
  ])

let tile_z_index tile =
  match tile with
  | Object Wall -> 0
  | Object Rock -> 1
  | Object Flag -> 2
  | Object Blum -> 3
  | Object Hi -> 4
  | Word _ -> 5

let word_to_text_and_color tile =
  let white = (255, 255, 255) in
  let text', color =
    match tile with
    | And -> "AND", white
    | Not -> "NOT", white
    | Well -> "we/ll", white

    | Prep On -> "ON", white
    | Prep Feeling -> "FEEL/ING", white

    | Prop You -> "YOU", (255, 0, 127)
    | Prop Win -> "WIN", (255, 255, 64)
    | Prop Stop -> "ST/OP", (0, 127, 0)
    | Prop Push -> "PU/SH", (192, 127, 0)

    | Verb Is -> "IS", white
    | Verb Make -> "MA/KE", white

    | Noun Special All -> "ALL", white
    | Noun Special Empty -> "EMP/TY", white

    | Noun Object Blum -> "BL/UM", (255, 0, 127)
    | Noun Object Flag -> "FL/AG", (255, 255, 64)
    | Noun Object Wall -> "WA/LL", (127, 127, 127)
    | Noun Object Rock -> "RO/CK", (192, 127, 0)
    | Noun Object Hi -> "Hi", (0, 255, 0)
  in
  let text =
    match String.split_first ~sep:"/" text' with
    | Some lines -> Either.Right lines
    | None -> Either.Left text'
  in
  text, color