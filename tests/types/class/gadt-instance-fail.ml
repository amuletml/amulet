type nat = Z | S of nat
type vec 'n 'a =
  | Nil : vec Z 'a
  | Cons : 'a * vec 'n 'a -> vec (S 'n) 'a

let rec zip (a : vec 'n 'a) (b : vec 'n 'b) : vec 'n ('a * 'b) =
  match a, b with
  | Nil, Nil -> Nil
  | Cons (x, xs), Cons (y, ys) -> Cons ((x, y), xs `zip` ys)

let rec map (f : 'a -> 'b) (v : vec 'n 'a) : vec 'n 'b =
  match v with
  | Nil -> Nil
  | Cons (x, xs) -> Cons (f x, map f xs)

class map_vec 'n begin
  val map_vec : ('a -> 'b) -> vec 'n 'a -> vec 'n 'b
end

instance map_vec Z begin
  let map_vec _ (Cons _) = Nil
end
