type fix 'f = Fix of 'f (fix 'f)

class recursive 't begin
  type rep : type -> type
  val into : 't -> fix (rep 't)
  val from : fix (rep 't) -> 't
end

type 'a :~: 'b = Refl : 'a ~ 'b => 'a :~: 'b
type proxy 'a = Proxy

class (forall 'a. 'x 'a ~ 'y 'a) => eq_1 'x 'y begin
  val inj_1 : proxy 'x -> proxy 'y -> 'x :~: 'y
end

external val sorry : 'a -> 'b = "function(x) return x end"

instance 'x ~ 'y => eq_1 'x 'y begin
  let inj_1 _ _ = sorry Refl
end

let subst (Refl : 'a :~: 'b) (x : 'p 'a) : 'p 'b = x

let cast' :
  forall 'a 'b.
    (recursive 'a * recursive 'b * eq_1 (rep 'a) (rep 'b)) =>
    proxy (rep 'a) -> proxy (rep 'b) -> 'a -> 'b
  = fun (p1 : proxy 'r1) (p2 : proxy 'r2) x ->
      let rep1 = into x
      let rep2 = subst (inj_1 p1 p2) rep1
      from rep2
