external val ( * ) : int -> int -> int = "function(a, b) return a * b end"

type term 'a =
  | Lit : int -> term int
  | Fun : ('a -> 'b) -> term ('a -> 'b)
  | App : term ('a -> 'b) * term 'a -> term 'b


let rec eval (x : term 'a) : 'a =
  match x with
  | Lit l -> l
  | Fun x -> x
  | App (f, x) -> (eval f) (eval x)

external val print : 'a -> unit = "print"

let mul a b = App (App (Fun (fun x y -> x * y), a), b)

let () = eval (App (Fun print, mul (Lit 123) (Lit 2)))
