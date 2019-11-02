type nat = Z | S of nat
type vect 'n 'a =
  | Nil : vect Z 'a
  | Cons : 'a * vect 'k 'a -> vect (S 'k) 'a

type function map 'f 'xs
begin
  map 'f (Cons ('x, 'xs)) = Cons ('f 'x, map 'f 'xs)
  map 'f Nil = Nil
end
