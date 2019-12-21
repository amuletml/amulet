external val use : 'a -> () = "print"

let rec map f = function
  | [] -> []
  | Cons (x, xs) -> Cons (f x, map f xs)

let () = use map

external val bottom : 'a = "nil"

let rec map_no_sat f = function
  | [] -> []
  | Cons (x, xs) ->
      let _ = map_no_sat bottom []
      Cons (f x, map f xs)

let () = use map_no_sat
