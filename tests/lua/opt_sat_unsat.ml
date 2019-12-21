external val opaque_id : 'a -> 'a = "function(x) return x end"
let id x = x

let rec map : forall 'a 'b. ('a -> 'b) -> list 'a -> list 'b =
  fun f -> function
  | [] -> []
  | Cons (x, xs) -> 
    let _ = (opaque_id map) id [1] (* Non-saturated usage of map *)
    Cons (f x, map f xs)

let _ = opaque_id (map @int @int)
