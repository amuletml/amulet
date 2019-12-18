open import "../amulet/exception.ml"
open import "../amulet/option.ml"
open import "../amulet/either.ml"
open import "../amulet/base.ml"

class functor 'f => foldable 'f begin
  val foldl : ('b -> 'a -> 'b) -> 'b -> 'f 'a -> 'b
  val foldr : ('a -> 'b -> 'b) -> 'b -> 'f 'a -> 'b

  let foldl f z xs = foldr (fun b g x -> g (f x b)) (fun x -> x) xs z
  let foldr f z xs = foldl (fun g b x -> g (f b x)) (fun x -> x) xs z

  val foldl1 : ('a -> 'a -> 'a) -> 'f 'a -> 'a

  let foldl1 f xs = 
    let kf m y =
      Some (match m with
        | None -> y
        | Some x -> f x y)
    match foldl kf None xs with
    | None -> error "foldl1: empty structure"
    | Some x -> x
end

instance foldable list begin
  let foldl f =
    let rec loop z = function
      | [] -> z
      | Cons (x, xs) -> loop (f z x) xs
    loop
  let foldr f z =
    let rec loop = function
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

instance foldable (either 'a) begin
  let foldl f z = function
    | Right a -> f z a
    | Left _ -> z

  let foldr f z = function
    | Right a -> f a z
    | Left _ -> z
end

let sum xs = foldl (+) 0 xs
let floating_sum xs = foldl (+.) 0.0 xs

let product xs = foldl ( * ) 1 xs
let floating_product xs = foldl ( *. ) 1.0 xs

let length xs = foldl (fun l _ -> l + 1) 0 xs

let to_list xs = foldr (::) [] xs

let minimum xs =
  foldl1 (fun s x -> if x < s then x else s) xs

let maximum xs =
  foldl1 (fun s x -> if x > s then x else s) xs

let minimumBy compare xs =
  foldl1 (fun s x -> match x `compare` s with | Lt -> x | _ -> s) xs

let maximumBy compare xs =
  foldl1 (fun s x -> match x `compare` s with | Gt -> x | _ -> s) xs
