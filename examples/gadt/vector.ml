type z ;;
type s 'a ;;

type vect 'n 'a =
  | Nil : vect z 'a
  | Cons : 'a * vect 'k 'a -> vect (s 'k) 'a ;;

let map (f : 'a -> 'b) (xs : vect 'n 'a) : vect 'n 'b =
  match xs with
  | Nil -> Nil
  | Cons (x, xs) -> Cons (f x, map f xs) ;;

let fold (f : 'a -> 'z -> 'z) (xs : vect 'n 'a) (z : 'z) : 'z =
  match xs with
  | Nil -> z
  | Cons (x, xs) -> f x (fold f xs z) ;;

let it'sNil Nil = Nil ;;
let it'sCons (Cons x) = Cons x ;;

let head (Cons (x, _)) = x
and tail (Cons (_, xs)) = xs ;;

let eq_vect eq_elt (x : vect 'n 'a) (y : vect 'n 'a) : bool =
  match (x, y) with
  | (Cons (xh, xt), Cons (yh, yt)) -> eq_elt xh yh && eq_vect eq_elt xt yt
  | (Nil, Nil) -> true ;;

type lte 'a 'b =
  | LteZero : lte z 'a
  | LteSucc : lte 'a 'b -> lte (s 'a) (s 'b) ;;

let zip_vect (x : vect 'n 'a) (y : vect 'm 'b) (prf : lte 'n 'm) : vect 'n ('a * 'b) =
  match (x, y, prf) with
  | (Cons (xh, xt), Cons (yh, yt), LteSucc p) -> Cons ((xh, yh), zip_vect xt yt p)
  | (Nil, Nil, LteZero) -> Nil
