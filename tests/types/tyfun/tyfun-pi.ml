type nat = Z | S of nat
type vect 'n 'a =
  | Nil : vect Z 'a
  | Cons : 'a * vect 'k 'a -> vect (S 'k) 'a

(* 'replicate' has a proper pi:
  * forall {'k : type} ('n : nat) ('a : 'k) -> vect 'n 'k
  *)
type function replicate {'k : type} 'n ('a : 'k) : vect 'n 'k begin
  replicate Z 'x      = Nil
  replicate (S 'k) 'a = Cons ('a, replicate 'k 'a)
end

(* type 'a ~~ 'b = Refl : 'a ~ 'b => 'a ~~ 'b *)

(* let foo : replicate (S (S (S Z))) [int] ~~ [int] = Refl *)
