external val tostring : 'a -> string = "tostring"
external val writeln : string -> unit = "print"

type identity 'a = Identity of 'a
let runIdentity (Identity x) = x

type const 'a 'b = Const of 'a
let getConst (Const x) = x

type functor 'f = Functor of (forall 'a 'b. ('a -> 'b) -> 'f 'a -> 'f 'b)
let (<$>) ?(Functor f) = f

type applicative 'f = Applicative of (forall 'a 'b. 'f ('a -> 'b) -> 'f 'a -> 'f 'b) * (forall 'a. 'a -> 'f 'a)
let (<*>) ?(Applicative (f, _)) = f
let pure ?(Applicative (_, f)) = f

type monoid 'm = Monoid of 'm * ('m -> 'm -> 'm)
let mappend ?(Monoid (_, f)) = f
let mempty ?(Monoid (m, _)) = m

type show 'a = Show of 'a -> string

let show ?(Show f) = f
let print ?(Show f) x = writeln (f x)

let implicit show_string = Show (fun x -> x)
let implicit show_int = Show (fun (x : int) -> tostring x)
let implicit show_bool = Show (function | true -> "true" | false -> "false")
let implicit show_pair ?(sa : show 'a) ?(sb : show 'b) : show ('a * 'b) =
  Show (fun (a, b) -> "(" ^ show a ^ ", " ^ show b ^ ")")

let implicit functor_identity =
  Functor (fun f (Identity x) -> Identity (f x))

let implicit functor_const =
  Functor (fun _ (Const x) -> Const x)

let implicit applicative_identity =
  Applicative (fun (Identity f) (Identity x) -> Identity (f x), Identity)

let implicit applicative_const ?(Monoid (mempty, mappend) : monoid 'x) =
  Applicative (fun (Const (f : 'x)) (Const x) -> Const (mappend f x), fun _ -> Const mempty)

let implicit functor_from_applicative ?(ap : applicative 'f) = Functor (fun f x -> (pure f) <*> x)

let f & g = fun x -> f (g x)
let x |> f = f x

let fst ?f k (a, b) = (,b) <$> k a
let snd ?f k (a, b) = (a,) <$> k b

let l .~ b = runIdentity & l (fun _ -> Identity b)
let s ^. l = getConst (l Const s)

type list 'a = Nil | Cons of 'a * list 'a
let (::) x y = Cons (x, y)

let implicit show_list ?(Show ke : show 'a) : show (list 'a) =
  let show_it = function
    | Nil -> "Nil"
    | Cons (a, b) -> "Cons (" ^ ke a ^ ", " ^ show_it b ^ ")"
  Show show_it

let implicit functor_list =
  let fmap (f : 'a -> 'b) : list 'a -> list 'b = function
    | Nil -> Nil
    | Cons (x, xs) -> f x :: fmap f xs
  Functor fmap

let implicit applicative_list =
  let ap (f : list ('a -> 'b)) (x : list 'a) : list 'b =
    match f, x with
    | Cons (f, fs), Cons (x, xs) -> f x :: ap fs xs
    | _, _ -> Nil
  let pure (x : 'a) : list 'a = x :: Nil
  Applicative (ap, pure)

let implicit monoid_list =
  let concat (x : list 'a) (y : list 'a) : list 'a =
    match x, y with
    | Nil, y -> y
    | Cons (x, xs), y -> Cons (x, concat xs y)
  Monoid (Nil, concat)

let traverse ?(x : applicative 'f) f = function
  | Nil -> pure Nil
  | Cons (x, xs) -> (::) <$> f x <*> traverse f xs

let foldMapOf l f = getConst & l (Const & f)
let toListOf l = foldMapOf l (fun x -> Cons (x, Nil))

let main =
  let p = (1, "foo")
  print (p |> snd .~ 2)
  let xs = Cons (p, Cons (p, Cons (p, Nil)))
  print (toListOf (traverse & fst) xs)
  print (toListOf (traverse & snd) xs)
  print (xs |> (traverse & snd) .~ 3)
