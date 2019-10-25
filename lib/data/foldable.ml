open import "../amulet/base.ml"
open import "../amulet/option.ml"

class functor 'f => foldable 'f begin
  val foldl : ('b -> 'a -> 'b) -> 'b -> 'f 'a -> 'b
  val foldr : ('a -> 'b -> 'b) -> 'b -> 'f 'a -> 'b

  let foldl f z xs = foldr (fun b g x -> g (f x b)) id xs z
  let foldr f z xs = foldl (fun g b x -> g (f b x)) id xs z
end

instance foldable list begin
  let foldl f =
    let loop z = function
      | [] -> z
      | Cons (x, xs) -> loop (f z x) xs
    loop
  let foldr f z =
    let loop = function
      | [] -> z
      | Cons (x, xs) -> f x (loop xs)
    loop
end

instance foldable option begin
  let foldl f z = function
    | Some x -> f z x
    | None -> z

  let foldr f z = function
    | Some x -> f x z
    | None -> z
end

let sum xs = foldl (+) 0 xs
let floating_sum xs = foldl (+.) 0.0 xs

let product xs = foldl ( * ) 1 xs
let floating_product xs = foldl ( *. ) 1.0 xs

let length xs = foldl (fun l _ -> l + 1) 0 xs

let to_list xs = foldr (::) [] xs

(* Kinda gross: a module with all the above definitions specialised for
 * lists, so the optimiser (hopefully) deals with them better. *)

module List = begin
  let sum (xs : list _) = foldl (+) 0 xs
  let floating_sum (xs : list _) = foldl (+.) 0.0 xs

  let product (xs : list _) = foldl ( * ) 1 xs
  let floating_product (xs : list _) = foldl ( *. ) 1.0 xs

  let length (xs : list _) = foldl (fun l _ -> l + 1) 0 xs
end
