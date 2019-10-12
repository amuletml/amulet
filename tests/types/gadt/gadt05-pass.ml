external val (+) : int -> int -> int = ""

type t 't 'a =
  | T : 'a -> t () 'a

let foo : forall 'a 't. ('a -> 'a) -> t 't 'a -> t 't 'a =
  fun f (T x) -> T (f x)

let bar : forall 't. t 't int -> t 't int =
  fun t -> match t with
  | T _ -> foo (+ 1) t
