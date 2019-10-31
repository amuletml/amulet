type 'a :: 'as <- Cons ('a, 'as)

class ty_fun 'f begin
  type arg
  type ret
  type apply ('x : arg 'f) : ret 'f
end

type function map 'f 'xs begin
  map 'f Nil = Nil
  map 'f (Cons ('a, 'as)) = apply 'f 'a :: map 'f 'as
end

type nat = Z | S of nat

instance ty_fun S begin
  type arg = nat
  type ret = nat
  type apply 'n = S 'n
end
