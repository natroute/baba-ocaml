open Tile

type selector_cond =
| On of Word.Noun.t
| Feeling of Word.Prop.t

type selector = {
  noun : Word.Noun.t;
  negative : bool;
  conds : selector_cond list;
}

type clause =
| Is_prop of Word.Prop.t
| Is_noun of Word.Noun.t
| Make of Word.Noun.t

type t = {
  selector : selector;
  clause : clause;
}