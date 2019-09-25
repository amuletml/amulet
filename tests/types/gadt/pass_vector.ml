type natural = Z | S of natural

type vect 'n 'a =
  | Nil : vect Z 'a
  | Cons : 'a * vect 'k 'a -> vect (S 'k) 'a

let head (Cons (x, _) : vect (S 'n) 'a) : 'a = x
and tail (Cons (_, xs) : vect (S 'n) 'a) : vect 'n 'a = xs

let main = head (tail (tail (Cons (1, Cons (2, Cons (3, Nil))))))
