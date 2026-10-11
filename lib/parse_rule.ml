let parse seq =
  (*
    A common pattern here is the following:
  
    {[
    match seq () with
    | Seq.Cons (X, seq) -> Y
    | _ -> Z, seq
    ]}
  
    To make an analogy to imperative logic (because I can't think of any other
    way to explain this (skill issue)), the [seq ()] is _sort of_ like
    consuming the next token. If the token matches _X_, we do some stuff _Y_
    with it. If it doesn't, we return the state of the sequence as it was
    before we "consumed" the token, in order to "backtrack".
  *)

  let (>>|) (x, y) f = (f x, y) in
  let (let+) = (>>|) in

  let next seq =
    match seq () with
    | Seq.Cons (x, seq) -> (x, seq)
    | Seq.Nil -> raise Exit
  in

  let parse_noun seq = next seq >>| function Tile.Noun x -> x | _ -> raise Exit in
  let parse_prop seq = next seq >>| function Tile.Prop x -> x | _ -> raise Exit in
  let parse_verb seq = next seq >>| function Tile.Verb x -> x | _ -> raise Exit in

  let rec parse_negative seq =
    match seq () with
    | Seq.Cons (Tile.Not, seq) -> parse_negative seq >>| not
    | _ -> false, seq
  in

  let parse_selectors seq =
    let parse_selector seq =
      let parse_conds seq =
        let parse_cond seq prep =
          match prep with
          | Tile.On ->
              let+ noun = parse_noun seq in Rule.On noun
          | Tile.Feeling ->
              let+ prop = parse_prop seq in Rule.Feeling prop
        in

        let rec recurse seq =
          match seq () with
          | Seq.Cons (Tile.Prep prep, seq) ->
              let cond, seq = parse_cond seq prep in
              recurse seq >>| List.cons cond
          | _ -> [], seq
        in

        recurse seq
      in

      let negative, seq = parse_negative seq in
      let noun, seq = parse_noun seq in
      let conds, seq = parse_conds seq in
      { Rule.noun; negative; conds }, seq
    in
    
    let rec recurse seq =
      match seq () with
      | Seq.Cons (Tile.And, seq) ->
          let selector, seq = parse_selector seq in
          recurse seq >>| List.cons selector
      | _ -> [], seq
    in

    let first, seq = parse_selector seq in
    let rest, seq = recurse seq in
    first :: rest, seq
  in

  let parse_clauses seq =
    let parse_clause seq verb =
      match verb with
      | Tile.Is ->
          next seq >>| begin function
          | Tile.Prop prop -> Rule.Is_prop prop
          | Tile.Noun noun -> Rule.Is_noun noun
          | _ -> raise Exit
          end
      | Tile.Make ->
          let+ noun = parse_noun seq in Rule.Make noun
    in

    let rec recurse seq prev_verb =
      match seq () with
      | Seq.Cons (Tile.And, seq) ->
          let verb, seq =
            match seq () with
            | Seq.Cons (Tile.Verb verb, seq) -> verb, seq
            | _ -> prev_verb, seq
          in
          begin try
            let clause, seq = parse_clause seq verb in
            recurse seq verb >>| List.cons clause
          with Exit -> [], seq end
      | _ -> [], seq
    in

    let first_verb, seq = parse_verb seq in
    let first, seq = parse_clause seq first_verb in
    let rest, seq = recurse seq first_verb in
    first :: rest, seq
  in

  try
    let selectors, seq = parse_selectors seq in
    let clauses, seq = parse_clauses seq in
    Some (
      selectors |> List.concat_map (fun selector ->
        clauses |> List.map (fun clause ->
          { Rule.selector; clause }
        )
      )
    )
  with
    Exit -> None