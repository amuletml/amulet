external val tostring : 'a -> string = "tostring"
external val print : string -> unit = "print"

type identity 'a = Identity of 'a
let runIdentity (Identity x) = x

type const 'a 'b = Const of 'a
let getConst (Const x) = x

type functor 'f = Functor of (forall 'a 'b. ('a -> 'b) -> 'f 'a -> 'f 'b)

let implicit functor_identity =
  Functor (fun f (Identity x) -> Identity (f x))

let implicit functor_const =
  Functor (fun _ (Const x) -> Const x)

let f & g = fun x -> f (g x)
let x |> f = f x

let (<$>) : forall 'a 'b 'f. functor 'f => ('a -> 'b) -> 'f 'a -> 'f 'b =
  fun (Functor f) -> f

let fst : forall 'a 'b 'c 'f. functor 'f => ('a -> 'f 'b) -> ('a * 'c) -> 'f ('b * 'c) =
  fun _ k (a, b) -> (,b) <$> k a

let snd : forall 'a 'b 'c 'f. functor 'f => ('a -> 'f 'b) -> ('c * 'a) -> 'f ('c * 'b) =
  fun _ k (a, b) -> (a,) <$> k b

let (.~) : forall 's 't 'a 'b. (('a -> identity 'b) -> 's -> identity 't) -> 'b -> 's -> 't =
  fun l b -> runIdentity & l (fun _ -> Identity b)

let (^.) : forall 's 'a. 's -> (('a -> const 'a 'a) -> 's -> const 'a 's) -> 'a =
  fun s l -> getConst (l Const s)

let print_pair (x, y) = print ("(" ^ tostring x ^ ", " ^ tostring y ^ ")")

let main =
  let p = (1, "foo")
  print_pair (p |> snd .~ 2)
  print (tostring (p ^. fst))
