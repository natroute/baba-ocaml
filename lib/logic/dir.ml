open Vec2.Vec2i

type t =
| Left
| Right
| Up
| Down

let to_vec2i = function
| Left  -> {x = -1; y =  0}
| Right -> {x = +1; y =  0}
| Up    -> {x =  0; y = -1}
| Down  -> {x =  0; y = +1}