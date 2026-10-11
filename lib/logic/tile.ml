module Object = struct
  type t =
  | Blum
  | Flag
  | Wall
  | Rock
  | Hi
  [@@deriving enumerate]
end

module Word = struct
  module Noun = struct
    module Special = struct
      type t =
      | All
      | Empty
      [@@deriving enumerate]
    end

    type t =
    | Object of Object.t
    | Special of Special.t
    [@@deriving enumerate]
  end

  module Prop = struct
    type t =
    | You
    | Win
    | Stop
    | Push
    [@@deriving enumerate]
  end

  module Verb = struct
    type t =
    | Is
    | Make
    [@@deriving enumerate]
  end

  module Prep = struct
    type t =
    | On
    | Feeling
    [@@deriving enumerate]
  end

  type t =
  | Noun of Noun.t
  | Prop of Prop.t
  | Verb of Verb.t
  | Prep of Prep.t
  | And
  | Not
  | Well
  [@@deriving enumerate]
end

type t =
| Object of Object.t
| Word of Word.t
[@@deriving enumerate]