external val tostring : 'a -> string = "tostring"
external val writeln : string -> unit = "print"

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

instance monoid string begin
  let (×) = (^)
  let zero = ""
end

class show 'a begin
  val show : 'a -> string
end

instance show string begin
  let show x = x
end

instance show int begin
  let show x = tostring x
end

instance show bool begin
  let show = function
    | true -> "true"
    | false -> "false"
end

instance show 'a * show 'b => show ('a * 'b) begin
  let show (x, y) = "(" ^ show x ^ ", " ^ show y ^ ")"
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
let x |> f = f x

let first k (a, b) = (,b) <$> k a
let second k (a, b) = (a,) <$> k b

let l .~ b = runId & l (fun _ -> Identity b)
let s ^. l = getConst (l Const s)

type list 'a = Nil | Cons of 'a * list 'a
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

let f = toListOf (traverse & first) ((1, ()) :: Nil)
let f =
  let xs = (1, ()) :: (1, ()) :: (1, ()) :: Nil
  print (toListOf (traverse & first) xs)
let () =
  let p = (1, "foo")
  print (p |> second .~ 2)
  let xs = p :: p :: p :: Nil
  print (toListOf (traverse & first) xs)
