type obj =
| Blum
| Flag
| Wall
| Rock
| Hi

module Noun = struct
  type special =
  | All
  | Empty

  type t =
  | Obj of obj
  | Special of special
end

type prop =
| You
| Win
| Stop
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

type t =
| Obj of obj
| Word of word