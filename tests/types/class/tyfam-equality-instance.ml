type id 'a = Id of 'a
type const 'a 'b = Const of 'a
type (:+:) 'f 'g 'a = L of 'f 'a | R of 'g 'a
type (:*:) 'f 'g 'a = P of 'f 'a * 'g 'a
type fix 'f = Fix of 'f (fix 'f)

class functor 'f begin
  val (<$>) : ('a -> 'b) -> 'f 'a -> 'f 'b
end

class functor (rep 't) => recursive 't begin
  type rep : type -> type
  val into : 't -> fix (rep 't)
  val from : fix (rep 't) -> 't
end

instance functor (const 'a) begin
  let _ <$> (Const x) = Const x
end

instance functor id begin
  let f <$> (Id x) = Id (f x)
end

instance functor 'f * functor 'g => functor ('f :*: 'g) begin
  let f <$> (P (a, b)) = P (f <$> a, f <$> b)
end

instance functor 'f * functor 'g => functor ('f :+: 'g) begin
  let f <$> xs =
    match xs with
    | L a -> L (f <$> a)
    | R a -> R (f <$> a)
end

instance recursive (list 'a) begin
  type rep = const () :+: (const 'a :*: id)

  let into = function
    | Cons (x, xs) -> Fix (R (P (Const x, Id (into xs))))
    | [] -> Fix (L (Const ()))

  let from = function
    | Fix (R (P (Const hd, Id tl))) -> Cons (hd, from tl)
    | Fix (L (Const ())) -> []
end

let unfix (Fix f) = f

let f @@ x = f x

let cata phi x =
  let rec cata_base phi x =
    phi @@ (cata_base phi <$>) @@ unfix x
  cata_base phi (into x)
