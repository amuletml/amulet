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

let (<$>) ?(Functor f) = f

let fst ?f k (a, b) = (,b) <$> k a

let snd ?f k (a, b) = (a,) <$> k b

let l .~ b = runIdentity & l (fun _ -> Identity b)

let s ^. l = getConst (l Const s)

let print_pair (x, y) = print ("(" ^ tostring x ^ ", " ^ tostring y ^ ")")

let main =
  let p = (1, "foo")
  print_pair (p |> snd .~ 2)
  print (tostring (p ^. fst))
