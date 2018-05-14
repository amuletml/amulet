type natural = Z | S of natural

type vect 'n 'a =
  | Nil : vect Z 'a
  | Cons : 'a * vect 'k 'a -> vect (S 'k) 'a

let head (Cons (x, _)) = x
and tail (Cons (_, xs)) = xs

let main = head (tail (tail (Cons (1, Cons (2, Cons (3, Nil))))))
