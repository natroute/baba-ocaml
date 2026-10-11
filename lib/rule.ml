type selector_cond =
| On of Tile.Noun.t
| Feeling of Tile.prop

type selector = {
  noun : Tile.Noun.t;
  negative : bool;
  conds : selector_cond list;
}

type clause =
| Is_prop of Tile.prop
| Is_noun of Tile.Noun.t
| Make of Tile.Noun.t

type t = {
  selector : selector;
  clause : clause;
}