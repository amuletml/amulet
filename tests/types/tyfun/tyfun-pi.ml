class ty_fun 'f begin
  type ret
  type arg
  type apply ('x : arg 'f) : ret 'f
end

(* map has a proper pi type:
  * forall ('f : 'k) -> list (arg 'f) -> list (ret 'f)
  *)
type function map 'f 'xs : list (ret 'f) begin
  map 'f Nil = Nil
  map 'f (Cons ('a, 'as)) = Cons (apply 'f 'a, map 'f 'as)
end

type nat = Z | S of nat

instance ty_fun S begin
  type arg = nat
  type ret = nat
  type apply 'n = S 'n
end

type 'a :~: 'b = Refl : 'a ~ 'b => 'a :~: 'b

(* this error is /fast/ because we don't ever need to reduce the map.
 * not a single step *)
let x : map S [Z, Z, Z, Z, Z, Z, Z, Z, Z, Z, Z, Z, Z, Z, Z, Z, Z, Z, Z, Z, Z, Z, Z, Z, Z, Z, Z, Z, Z, Z, Z, Z, Z, Z, Z] :~: [12] = Refl
