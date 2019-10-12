external val tostring : 'a -> string = "tostring"
external val writeln : string -> unit = "print"
external val (^) : string -> string -> string = "function(x, y) return x .. y end"

class functor 'f begin
  val (<$>) : forall 'a 'b. ('a -> 'b) -> 'f 'a -> 'f 'b
end

class functor 'f => applicative 'f begin
  val pure : forall 'a. 'a -> 'f 'a
  val (<*>) : forall 'a 'b. 'f ('a -> 'b) -> 'f 'a -> 'f 'b
end

class monoid 'a begin
  val (×) : 'a -> 'a -> 'a
  val zero : 'a
end

class show 'a begin
  val show : 'a -> string
end

instance show int begin
  let show x = tostring x
end

let print x = writeln (show x)

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

let first k (a, b) = (,b) <$> k a
let second k (a, b) = (a,) <$> k b

let (::) x y = Cons (x, y)

instance show 'a => show (list 'a) begin
  let show = function
    | Nil -> "Nil"
    | Cons (x, xs) -> show x ^ " :: " ^ show xs
end

instance functor list begin
  let (<$>) f = function
    | Nil -> Nil
    | Cons (x, xs) -> Cons (f x, f <$> xs)
end

instance applicative list begin
  let pure x = x :: Nil
  let f <*> x =
    match f, x with
    | Cons (f, fs), Cons (x, xs) -> f x :: (fs <*> xs)
    | _, _ -> Nil
end

class traversable 't begin
  val traverse : forall 'f 'a 'b. applicative 'f => ('a -> 'f 'b) -> 't 'a -> 'f ('t 'b)
end

instance traversable list begin
  let traverse k = function
    | Nil -> pure Nil
    | Cons (x, xs) -> (::) <$> k x <*> traverse k xs
end

instance monoid (list 'a) begin
  let zero = Nil
  let (×) x ys = match x with
  | Nil -> ys
  | Cons (x, xs) -> x :: (xs × ys)
end

let foldMapOf l f = getConst & l (Const & f)
let toListOf l = foldMapOf l (fun x -> x :: Nil)

let () =
  let xs = [(1, ()), (1, ()), (1, ())]
  print (toListOf (traverse & first) xs)
