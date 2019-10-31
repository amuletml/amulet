class ty_fun 'f begin
  type ret
  type arg
  type apply ('x : arg 'f) : ret 'f
end

(* map has a proper pi type:
  * forall ('f : 'k) -> list (arg 'f) -> list (ret 'f)
  * we have exactly 0 hope of inferring it.
  *)
type function map 'f 'xs begin
  map 'f Nil = Nil
  map 'f (Cons ('a, 'as)) = Cons (apply 'f 'a, map 'f 'as)
end
