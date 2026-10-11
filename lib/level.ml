open Grid
open Entry
open Rule
open Vec2
open Vec2i
open Option.Syntax

module Tbl = Grid.Tbl

type t = {
  grid : Grid.t;
}

(* would rather define this in a separate file but circular dependencies *)
module Cmd = struct
  type t = unit -> unit -> unit
  (**
    A single, reversible operation over a level, such as adding or moving an
    entry. [command ()] applies the operation and return a function that will
    undo it, i.e. bring the entire mutable state of the level to what it was
    before the application.
  *)

  let add level entry () =
    let id = Grid.add level.grid entry in
    fun () -> Grid.remove level.grid id

  let remove level id =
    let entry = Grid.get_by_id level.grid id in
    fun () ->
      Grid.remove level.grid id;
      fun () -> Grid.add_with_id level.grid id entry

  let move level id dst =
    let { pos = src } = Grid.get_by_id level.grid id in
    fun () ->
      Grid.move level.grid id dst;
      fun () -> Grid.move level.grid id src

  let set_tile level id tile =
    let { tile = prev_tile } = Grid.get_by_id level.grid id in
    fun () ->
      Grid.set_tile level.grid id tile;
      fun () -> Grid.set_tile level.grid id prev_tile
end

(* produce a sequence of words for Parse_rule.parse *)
let word_seq level pos dir =
  let dir' = Dir.to_vec2i dir in
  let rec recurse i () =
    let word =
      let* tiles = Grid.get_all_at level.grid (pos +: dir' *: i) in
      tiles |> Tbl.to_seq_keys |> Seq.find_map (fun id ->
        let { tile } = Grid.get_by_id level.grid id in
        match tile with
        | Tile.Word word -> Some word
        | _ -> None
      )
    in
    match word with
    | Some word -> Seq.Cons (word, recurse (i + 1))
    | None -> Seq.Nil
  in
  Seq.memoize (recurse 0)

let step level dir =
  (* hashset of tile ids and the props they have *)
  let ids_props = Hashtbl.create 128 in
  
  let has_prop id prop = Hashtbl.mem ids_props (id, prop) in

  let matches_selector id selector =
    let { pos; tile; dir } = Grid.get_by_id level.grid id in

    let matches_noun tile noun =
      match (noun : Tile.Noun.t) with
      | Special All -> true
      | Special Empty -> false
      | Obj obj ->
          begin match (tile : Tile.t) with
          | Obj tile_obj -> tile_obj = obj
          | _ -> false
          end
    in

    let matches_cond cond =
      match cond with
      | On noun ->
          Grid.get_all_at level.grid pos
          |> Option.get
          |> Tbl.to_seq_keys |> Seq.exists (fun on_id ->
            id != on_id
              && matches_noun (Grid.get_by_id level.grid on_id).tile noun
          )
      | Feeling prop -> has_prop id prop
    in

    matches_noun tile selector.noun
      && (selector.conds |> List.for_all matches_cond)
  in

  let rules =
    Grid.enumerate_seq level.grid |> List.of_seq |> List.concat_map (fun (pos, _) ->
      [Dir.Right; Dir.Down]
      |> List.filter_map (fun dir ->
        Parse_rule.parse (word_seq level pos dir)
      )
      |> List.concat
    )
  in

  (* for each tile ID in the level, find the clauses that apply to it *)
  let find_clauses_for_ids () =
    Grid.enumerate_seq level.grid |> List.of_seq |> List.concat_map (fun (_, ids) ->
      ids |> Tbl.to_seq_keys |> List.of_seq |> List.map (fun id ->
        id,
        rules |> List.filter_map (fun rule ->
          if matches_selector id rule.selector then
            Some rule.clause
          else
            None
        )
      )
    )
  in

  (*
    rule processing pass 1 - populate ids_props

    we go only one layer deep here as in the real game, so "A FEELING X IS Y,
    A FEELING Y IS Z" won't assign Z to A
  *)
  find_clauses_for_ids () |> List.iter (fun (id, clauses) ->
    clauses |> List.iter (fun (clause) ->
      match clause with
      | Is_prop prop -> Hashtbl.add ids_props (id, prop) ()
      | _ -> ()
    )
  );

  (* rule processing pass 2 - recompute with ids_props populated *)
  let ids_clauses = find_clauses_for_ids () in

  let movements =
    let is_pushable id =
      match (Grid.get_by_id level.grid id).tile with
      | Word _ -> true
      | Obj _ -> has_prop id Push
    in

    (* None => the nudge failed *)
    let rec nudge id dir =
      let { pos = src } = Grid.get_by_id level.grid id in
      let dst = src +: (Dir.to_vec2i dir) in

      let* dst_tiles = Grid.get_all_at level.grid dst in
      if
        dst_tiles |> Tbl.to_seq_keys
        |> Seq.exists (fun id -> has_prop id Stop)
      then
        None
      else
        let dst_cmds =
          dst_tiles |> Tbl.to_seq_keys |> List.of_seq
          |> List.map (fun dst_id ->
            if is_pushable dst_id then
              nudge dst_id dir
            else
              Some []
          )
        in
        if dst_cmds |> List.exists Option.is_none then
          None
        else
          Some (Cmd.move level id dst :: List.concat_map Option.get dst_cmds)
    in

    ids_clauses |> List.concat_map (fun (id, clauses) ->
      clauses |> List.concat_map (fun clause ->
        match clause with
        | Is_prop You -> nudge id dir |> Option.value ~default:[]
        | _ -> []
      )
    )
  in

  let transformations =
    ids_clauses |> List.concat_map (fun (id, clauses) ->
      clauses |> List.filter_map (fun clause ->
        match clause with
        | Is_noun noun ->
            begin match noun with
            | Special All -> None
            | Special Empty -> Some (Cmd.remove level id)
            | Obj obj -> Some (Cmd.set_tile level id (Obj obj))
            end
        | _ -> None
      )
    )
  in

  movements @ transformations

let example () =
  let grid = Grid.create 15 11 in

  let add x y ?(dir = Dir.Right) tile =
    Grid.add grid (Grid.Entry.create { x; y } tile dir) |> ignore
  in
 
  [3; 7] |> List.iter (fun y ->
    for x = 2 to 12 do
      add x y (Obj Wall)
    done
  );

  add  3  5 (Obj Blum);
  add  7  4 (Obj Rock);
  add  7  5 (Obj Hi);
  add  7  6 (Obj Rock);
  add 11  5 (Obj Flag);

  add  2  2 (Word (Noun (Obj Blum)));
  add  3  2 (Word (Verb Is));
  add  4  2 (Word (Prop You));
 
  add 10  2 (Word (Noun (Obj Flag)));
  add 11  2 (Word (Verb Is));
  add 12  2 (Word (Prop Win));

  add  2  8 (Word (Noun (Obj Wall)));
  add  3  8 (Word (Verb Is));
  add  4  8 (Word (Prop Stop));
 
  add  8  8 (Word (Noun (Obj Rock)));
  add  9  8 (Word And);
  add 10  8 (Word (Noun (Obj Hi)));
  add 11  8 (Word (Verb Is));
  add 12  8 (Word (Prop Push));

  { grid = grid }