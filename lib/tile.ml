module Noun = struct
  type entity =
  | Baba

  type special =
  | All

  type t =
  | Entity of entity
  | Special of special
end

type prop =
| You
| Push

type verb =
| Is
| Make

type prep =
| On
| Feeling

type word =
| Noun of Noun.t
| Prop of prop
| Verb of verb
| Prep of prep
| And
| Not