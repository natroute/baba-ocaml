type subject_cond =
| On of Tile.Noun.t
| Feeling of Tile.prop

type subject = {
  noun : Tile.Noun.t;
  negative : bool;
  conds : subject_cond list;
}

type clause =
| Is_prop of Tile.prop
| Is_noun of Tile.Noun.t
| Make of Tile.Noun.t

type t = {
  subject : subject;
  clause : clause;
}