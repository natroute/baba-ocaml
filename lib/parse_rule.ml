exception Stop

let map_fst = Pair.map_fst

let parse seq =
  let next seq =
    match seq () with
    | Seq.Cons (x, seq) -> (x, seq)
    | Seq.Nil -> raise Stop
  in

  let rec parse_negative seq =
    match seq () with
    | Seq.Cons (Tile.Not, seq) -> parse_negative seq |> map_fst not
    | node -> false, (fun () -> node)
  in

  let parse_noun seq =
    next seq |> map_fst (
      function
      | Tile.Noun noun -> noun
      | _ -> raise Stop
    )
  in

  let parse_prop seq =
    next seq |> map_fst (
      function
      | Tile.Prop prop -> prop
      | _ -> raise Stop
    )
  in

  let parse_verb seq =
    next seq |> map_fst (
      function
      | Tile.Verb verb -> verb
      | _ -> raise Stop
    )
  in

  let parse_subjects seq =
    let parse_subject seq =
      let parse_conds seq =
        let parse_cond seq prep =
          match prep with
          | Tile.On ->
              parse_noun seq |> map_fst (fun noun -> Rule.On noun)
          | Tile.Feeling ->
              parse_prop seq |> map_fst (fun prop -> Rule.Feeling prop)
        in

        let rec recurse seq =
          match seq () with
          | Seq.Cons (Tile.Prep prep, seq) ->
              let cond, seq = parse_cond seq prep in
              let t, seq = recurse seq in
              (cond :: t, seq)
          | node -> [], fun () -> node
        in

        recurse seq |> map_fst List.rev
      in

      let negative, seq = parse_negative seq in
      let noun, seq = parse_noun seq in
      let conds, seq = parse_conds seq in
      ({ Rule.noun; negative; conds }, seq)
    in
    
    let rec recurse seq =
      match seq () with
      | Seq.Cons (Tile.And, seq) ->
          let cond, seq = parse_subject seq in
          recurse seq |> map_fst (fun t -> cond :: t)
      | node -> [], fun () -> node
    in

    let first, seq = parse_subject seq in
    let subjects, seq = recurse seq in
    (first :: subjects, seq)
  in

  let parse_clauses seq =
    let parse_clause seq verb =
      match verb with
      | Tile.Is ->
          next seq |> map_fst (
            function
            | Tile.Prop prop -> Rule.Is_prop prop
            | Tile.Noun noun -> Rule.Is_noun noun
            | _ -> raise Stop
          )
      | Tile.Make ->
          parse_noun seq |> map_fst (fun noun -> Rule.Make noun)
    in

    let rec recurse seq prev_verb =
      match seq () with
      | Seq.Cons (Tile.And, seq) ->
          let verb, seq =
            match seq () with
            | Seq.Cons (Tile.Verb verb, seq) -> verb, seq
            | node -> prev_verb, fun () -> node
          in
          let cond, seq = parse_clause seq verb in
          recurse seq verb |> map_fst (fun t -> cond :: t)
      | node -> [], fun () -> node
    in

    let first_verb, seq = parse_verb seq in
    let first, seq = parse_clause seq first_verb in
    let rest, seq = recurse seq first_verb in
    (first :: rest, seq)
  in

  try
    let subjects, seq = parse_subjects seq in
    let clauses, seq = parse_clauses seq in
    Some (
      subjects |> List.map (fun subject ->
        clauses |> List.map (fun clause -> { Rule.subject; clause })
      )
      |> List.concat
    )
  with
    Stop -> None