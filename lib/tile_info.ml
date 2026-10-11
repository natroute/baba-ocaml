let objs_to_names =
  Hashtbl.of_seq (List.to_seq Tile.[
    (Blum, "baba");
    (Flag, "flag");
    (Rock, "rock");
    (Wall, "wall");
    (Hi, "hi");
  ])

let tile_z_index tile =
  match (tile : Tile.t) with
  | Obj Wall -> 0
  | Word _ -> 1
  | Obj Rock -> 2
  | Obj Flag -> 3
  | Obj Blum -> 4
  | Obj Hi -> 5

let word_to_text_and_color tile =
  let white = (255, 255, 255) in
  let text', color =
    match (tile : Tile.word) with
    | And -> "AND", white
    | Not -> "NOT", white

    | Prep On -> "ON", white
    | Prep Feeling -> "FEEL/ING", white

    | Prop You -> "YOU", (255, 0, 127)
    | Prop Win -> "WIN", (255, 255, 64)
    | Prop Stop -> "ST/OP", (0, 127, 0)
    | Prop Push -> "PU/SH", (192, 127, 0)

    | Verb Is -> "IS", white
    | Verb Make -> "MA/KE", white

    | Noun Special All -> "ALL", white
    | Noun Special Empty -> invalid_arg "Tile_info.word_to_text_and_color"

    | Noun Obj Blum -> "BL/UM", (255, 0, 127)
    | Noun Obj Flag -> "FL/AG", (255, 255, 64)
    | Noun Obj Wall -> "WA/LL", (127, 127, 127)
    | Noun Obj Rock -> "RO/CK", (192, 127, 0)
    | Noun Obj Hi -> "HI", (0, 255, 0)
  in
  let text =
    match String.split_first ~sep:"/" text' with
    | Some lines -> Either.Right lines
    | None -> Either.Left text'
  in
  text, color