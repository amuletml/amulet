type nat = Z | S of nat

type vect 'n 'a =
  | Nil : vect Z 'a
  | Cons : 'a * vect 'k 'a -> vect (S 'k) 'a

let (::) x y = Cons (x, y)

type add 'a 'b 'c =
  | AZ : add Z 'b 'b
  | AS : add 'a 'b 'c -> add (S 'a) 'b (S 'c)

let implicit add_zero = AZ
let implicit add_succ ?p = AS p

let append : forall 'n 'k 'l 'a. add 'n 'k 'l => vect 'n 'a -> vect 'k 'a -> vect 'l 'a =
  let go : forall 'n 'k 'l 'a. add 'n 'k 'l -> vect 'n 'a -> vect 'k 'a -> vect 'l 'a = function
    | AZ -> fun Nil l -> l
    | AS p -> fun (Cons (x, xs)) t -> Cons (x, go p xs t)
  fun p -> go p

let main = append (1 :: 2 :: Nil) (3 :: 4 :: 5 :: Nil)
