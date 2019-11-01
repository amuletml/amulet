type nat = Z | S of nat
type vect 'n 'a =
  | Nil : vect Z 'a
  | Cons : 'a * vect 'k 'a -> vect (S 'k) 'a

(* At the value level we need a singleton for natural numbers since
 * Amulet's *type* system doesn't have Π *)
type snat 'n =
  | SZ : snat Z
  | SS : snat 's -> snat (S 's)

let rec replicate (n : snat 'n) (x : 'a) : vect 'n 'a =
  match n with
  | SZ -> Nil
  | SS n -> Cons (x, replicate n x)

let rec map (f : 'a -> 'b) (xs : vect 'n 'a) : vect 'n 'b =
  match xs with
  | Nil -> Nil
  | Cons (x, xs) -> Cons (f x, map f xs)

(* Type-level versions of replicate and map: *)

(* Amulet's *kind* system does have Π (formally - it's closed under
 * arbitrary products) and so we can use 'n (an argument) in the return
 * kind *)
type function replicate {'k : type} 'n ('a : 'k) : vect 'n 'k begin
  replicate Z 'x      = Nil
  replicate (S 'k) 'a = Cons ('a, replicate 'k 'a)
end

type function
  map {'a : type} {'b : type} {'n : nat}
      ('f : 'a -> 'b) ('xs : vect 'n 'a) : vect 'n 'b
begin
  map 'f (Cons ('x, 'xs)) = Cons ('f 'x, map 'f 'xs)
  map 'f Nil = Nil
end
