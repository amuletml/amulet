open import "prelude.ml"

class monoid 'a begin
  val (×) : 'a -> 'a -> 'a
  val zero : 'a
end

instance monoid string begin
  let (×) = (^)
  let zero = ""
end

type identity 'a = Identity of 'a
let runId (Identity x) = x

type const 'a 'b = Const of 'a
let getConst (Const x) = x

instance functor identity begin
  let f <$> (Identity x) = Identity (f x)
end

instance applicative identity begin
  let pure = Identity
  let (Identity f) <*> (Identity x) = Identity (f x)
end

instance functor (const 'a) begin
  let _ <$> (Const x) = Const x
end

instance monoid 'm => applicative (const 'm) begin
  let pure _ = Const zero
  let (Const x) <*> (Const y) = Const (x × y)
end

let f & g = fun x -> f (g x)
let x |> f = f x

let first k (a, b) = (,b) <$> k a
let second k (a, b) = (a,) <$> k b

let l ~. b = runId & l (fun _ -> Identity b)
let s ^. l = getConst (l Const s)

instance monoid (list 'a) begin
  let zero = Nil
  let (×) x ys = match x with
    | Nil -> ys
    | Cons (x, xs) -> x :: (xs × ys)
end

let foldMapOf l f = getConst & l (Const & f)
let toListOf l = foldMapOf l (fun x -> [x])

let f = toListOf (traverse & first) [(1, ())]
let f =
  let xs = [(1, ()), (1, ()), (1, ())]
  print (toListOf (traverse & first) xs)

let () =
  let p = (1, "foo")
  print (p |> second ~. 2)
  let xs = [p, p, p]
  print (toListOf (traverse & first) xs)
  ()
