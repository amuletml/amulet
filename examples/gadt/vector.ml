type z ;;
type s 'a ;;

external val print : 'a -> unit = "print" ;;

type vect 'n 'a =
  | Nil : vect z 'a
  | Cons : 'a * vect 'k 'a -> vect (s 'k) 'a ;;

type eq 'a 'b = Refl : eq 'a 'a ;;

type lte 'a 'b =
  | LteZero : lte z 'a
  | LteSucc : lte 'a 'b -> lte (s 'a) (s 'b) ;;

(* uses supplied type signature *)
let map (f : 'a -> 'b) (xs : vect 'n 'a) : vect 'n 'b =
  match xs with
  | Nil -> Nil
  | Cons (x, xs) -> Cons (f x, map f xs) ;;

(* uses supplied type signature *)
let fold (f : 'a -> 'z -> 'z) (z : 'z) (xs : vect 'n 'a) : 'z =
  match xs with
  | Nil -> z
  | Cons (x, xs) -> f x (fold f z xs) ;;

(* vect (s 'k) 'a -> 'a *)
let head (Cons (x, _)) = x
(* vect (s 'k) 'a -> vect 'k 'a *)
and tail (Cons (_, xs)) = xs ;;

(* uses supplied type signature *)
let eq_vect (eq_elt : 'a -> 'a -> bool) (x : vect 'n 'a) (y : vect 'm 'a) (Refl : eq 'n 'm) : bool =
  match (x, y) with
  | (Cons (xh, xt), Cons (yh, yt)) -> eq_elt xh yh && eq_vect eq_elt xt yt Refl
  | (Nil, Nil) -> true ;;

(* uses supplied type signature *)
let zip_vect (x : vect 'n 'a) (y : vect 'm 'b) (prf : lte 'n 'm) : vect 'n ('a * 'b) =
  match (x, y, prf) with
  | (Cons (xh, xt), Cons (yh, yt), LteSucc p) -> Cons ((xh, yh), zip_vect xt yt p)
  | (Nil, _, LteZero) -> Nil ;;

(* vect 'n 'a -> vect 'k 'a -> lte 'n 'k -> vect 'n 'a *)
let trim v x k =
  map (fun (x, _) -> x) (zip_vect v x k) ;;

(* vect 'n 'a -> unit *)
let print_vect v =
  fold (fun x k () -> begin print x; k () end) (fun () -> print "nil") v () ;;

(* unit *)
let main =
  (* vect (s (s (s z))) int *)
  let foo = trim (Cons (1, Cons (2, Cons (3, Nil))))
                 (Cons (1, Cons (2, Cons (3, Cons (4, Nil)))))
                 (LteSucc (LteSucc (LteSucc LteZero)))
  (* uses supplied type *)
  and go (x : vect 'n 'a) : unit =
    match x with
    | Nil -> print "Nil"
    | Cons (a, xs) -> begin
      print a;
      go xs
    end
  in go foo
