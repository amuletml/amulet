external val print : 'a -> unit = "print"
external val any_to_string : 'a -> string = "tostring"
external val io_write : string -> unit = "io.write"
external val error : string -> 'a = "error"
external val repeat_string : string -> int -> string = "string.rep"

type natural = Z | S of natural

type vect 'n 'a =
  | Nil : vect Z 'a
  | Cons : 'a * vect 'k 'a -> vect (S 'k) 'a

type eq 'a 'b = Refl : eq 'a 'a

type lte 'a 'b =
  | LteZero : lte Z 'a
  | LteSucc : lte 'a 'b -> lte (S 'a) (S 'b)

type nat 'n =
  | Zero : nat Z
  | Succ : nat 'k -> nat (S 'k)

type some_nat =
  | SomeNat : nat 'n -> some_nat

type maybe 'a = Some of 'a | None

let left && (right : lazy 'a) = if left then force right else false

(* uses supplied type signature *)
let map (f : 'a -> 'b) (xs : vect 'n 'a) : vect 'n 'b =
  match xs with
  | Nil -> Nil
  | Cons (x, xs) -> Cons (f x, map f xs)

(* uses supplied type signature *)
let fold (f : 'a -> 'z -> 'z) (z : 'z) (xs : vect 'n 'a) : 'z =
  match xs with
  | Nil -> z
  | Cons (x, xs) -> f x (fold f z xs)

(* vect (s 'k) 'a -> 'a *)
let head (Cons (x, _)) = x
(* vect (s 'k) 'a -> vect 'k 'a *)
let tail (Cons (_, xs)) = xs

(* uses supplied type signature *)
let eq_vect (eq_elt : 'a -> 'a -> bool) (x : vect 'n 'a) (y : vect 'm 'a) (Refl : eq 'n 'm) : bool =
  match (x, y) with
  | (Cons (xh, xt), Cons (yh, yt)) -> eq_elt xh yh && eq_vect eq_elt xt yt Refl
  | (Nil, Nil) -> true

(* uses supplied type signature *)
let zip_vect (x : vect 'n 'a) (y : vect 'm 'b) (prf : lte 'n 'm) : vect 'n ('a * 'b) =
  match (x, y, prf) with
  | (Cons (xh, xt), Cons (yh, yt), LteSucc p) -> Cons ((xh, yh), zip_vect xt yt p)
  | (Nil, _, LteZero) -> Nil

(* vect 'n 'a -> vect 'k 'a -> lte 'n 'k -> vect 'n 'a *)
let trim v x k =
  map (fun (x, _) -> x) (zip_vect v x k)

(* uses supplied type *)
let replicate (n : nat 'n) (x : 'a) : vect 'n 'a =
  let go : forall 'n. nat 'n -> vect 'n 'a = fun l ->
    match l with
    | Zero -> Nil
    | Succ k -> Cons (x, go k)
  in go n

(* rank n: forall 'b. (forall 'n. nat 'n -> 'b) -> int -> 'b *)
let with_natural (k : forall 'n. nat 'n -> 'b) i =
  let go n =
    if n == 0 then
      SomeNat Zero
    else
      match go (n - 1) with
      | SomeNat x -> SomeNat (Succ x)
  in match go i with
  | SomeNat n -> k n

(* uses supplied type *)
let decide_lte (x : nat 'n) (y : nat 'm) : maybe (lte 'n 'm) =
  match x, y with
  | Zero, y -> Some LteZero
  | Succ k, Zero -> None
  | Succ k, Succ n ->
      match decide_lte k n with
      | Some p -> Some (LteSucc p)
      | None -> None


(* forall 'n . nat 'n -> unit *)
let ppr_nat n : unit =
  match n with
  | Zero -> print "Zero"
  | Succ k ->
    io_write "Succ "
    ppr_nat k

(* unit *)
let main =
  (* vect (s (s (s z))) int *)
  let foo n =
    match decide_lte (Succ (Succ (Succ Zero))) n with
    | Some x -> trim (replicate (Succ (Succ (Succ Zero))) 2)
                     (replicate n 1) x
    | None -> error "oh no"
  (* uses supplied type *)
  let go (x : vect 'n 'a) (i : int) : unit =
    match x with
    | Nil -> print ("Nil" ^ repeat_string ")" i)
    | Cons (a, xs) ->
      io_write ("Cons (" ^ any_to_string a ^ ", ")
      go xs (i + 1)
  with_natural (fun x -> go (foo x) 0) 123
