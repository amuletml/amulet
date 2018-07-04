external val to_string : 'a -> string = "tostring"
external val writeln : string -> unit = "print"

type show 'a = Show of 'a -> string

let implicit show_string = Show (fun x -> x)
let implicit show_int = Show (to_string : int -> string)

let show ?(Show f) = f
let print ?(Show f) x = writeln (f x)

type functor 'f = Functor of (forall 'a 'b. ('a -> 'b) -> 'f 'a -> 'f 'b)
let fmap ?(Functor f) = f

type foldable 'f = Foldable of (forall 'r 'a. ('a -> 'r -> 'r) -> 'r -> 'f 'a -> 'r)
let foldr ?(Foldable f) = f

type nat = Z | S of nat

type vect 'n 'a =
  | Nil : vect Z 'a
  | Cons : 'a * vect 'k 'a -> vect (S 'k) 'a

let (::) x y = Cons (x, y)

let implicit show_nil : forall 'a. show (vect Z 'a) = Show (fun _ -> "Nil")
let implicit show_cons : forall 'n 'a. show 'a => show (vect 'n 'a) => show (vect (S 'n) 'a) =
  fun (Show es : show 'a) (Show ts) -> Show (fun (Cons (e : 'a, t)) -> es e ^ " :: " ^ ts t)

let implicit functor_nil : functor (vect Z) = Functor (fun _ _ -> Nil)
let implicit functor_succ ?(Functor fmap : functor (vect 'k)) : functor (vect (S 'k)) =
  Functor (fun f (Cons (x, t)) -> f x :: fmap f t)

let implicit foldable_nil : foldable (vect Z) = Foldable (fun _ r _ -> r)
let implicit foldable_succ ?(Foldable foldr : foldable (vect 'k)) : foldable (vect (S 'k)) =
  Foldable (fun f z (Cons (x, t)) -> x `f` foldr f z t)

let main = print (foldr (+) 1 (fmap (+ 1) (1 :: 2 :: Nil)))
