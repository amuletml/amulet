type nat = Z | S of nat
type snat 'n =
  | SZ : snat Z
  | SS : snat 'n -> snat (S 'n)

type add_ev 'a 'b 'c =
  | AddZ : add_ev Z 'a 'a
  | AddS : add_ev 'a 'b 'c -> add_ev (S 'a) 'b (S 'c)

class add 'a 'b 'c (* | 'a 'b -> 'c *) begin
  val ev : add_ev 'a 'b 'c
end

instance add Z 'a 'a begin
  let ev = AddZ
end

instance add 'a 'b 'c => add (S 'a) 'b (S 'c) begin
  let ev = AddS ev
end

type vect 'n 'a =
  | Nil : vect Z 'a
  | Cons : 'a * vect 'n 'a -> vect (S 'n) 'a

let append_with (ev : add_ev 'n 'k 'l) (xs : vect 'n 'a) (ys : vect 'k 'a) : vect 'l 'a =
  match ev, xs with
  | AddZ, Nil -> ys
  | AddS p, Cons (x, xs) -> Cons (x, append_with p xs ys)
let append xs ys = append_with ev xs ys

let x :: xs = Cons (x, xs)

(* add class without the fundep => can't solve for the result of
 * addition as if it were a function *)
let onetofive = append (1 :: 2 :: Nil) (3 :: 4 :: 5 :: Nil)
