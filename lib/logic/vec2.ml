module rec Vec2i : sig
  type t = { x : int; y : int }
  
  val map : (int -> int) -> t -> t
  val map2 : (int -> int -> int) -> t -> t -> t

  val (+:) : t -> t -> t
  val (-:) : t -> t -> t
  val ( *: ) : t -> int -> t
  val ( /: ) : t -> int -> t

  val length : t -> float

  val of_vec2f : Vec2f.t -> t
end = struct
  type t = { x : int; y : int }
  
  let map f { x; y } = { x = f x; y = f y }
  let map2 f a b = { x = f a.x b.x; y = f a.y b.y }

  let (+:) = map2 (+)
  let (-:) = map2 (-)
  let ( *: ) v s = map (( * ) s) v
  let (/:) v s = map (fun c -> c / s) v

  let length { x; y } = hypot (Float.of_int x) (Float.of_int y)

  let of_vec2f ({ x; y } : Vec2f.t) =
    { x = Int.of_float x; y = Int.of_float y }
end

and Vec2f : sig
  type t = { x : float; y : float }
  
  val map : (float -> float) -> t -> t
  val map2 : (float -> float -> float) -> t -> t -> t

  val (+!) : t -> t -> t
  val (-!) : t -> t -> t
  val ( *! ) : t -> float -> t
  val (/!) : t -> float -> t

  val length : t -> float

  val of_vec2i : Vec2i.t -> t
end = struct
  type t = { x : float; y : float }
  
  let map f { x; y } = { x = f x; y = f y }
  let map2 f a b = { x = f a.x b.x; y = f a.y b.y }

  let (+!) = map2 (+.)
  let (-!) = map2 (-.)
  let ( *! ) v s = map (( *. ) s) v
  let (/!) v s = map (fun c -> c /. s) v

  let length { x; y } = hypot x y

  let of_vec2i ({ x; y } : Vec2i.t) =
    { x = Float.of_int x; y = Float.of_int y }
end