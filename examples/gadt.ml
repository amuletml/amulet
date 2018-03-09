type term 'a =
  | Lit : int -> term int
  | Fun : forall 'c 'b. ('c -> 'b) -> term ('c -> 'b)
  | App : forall 'c 'b. term ('c -> 'b) * term 'c -> term 'b
;;

let eval (x : term 'a) : 'a =
  match x with
  | Lit l -> l
  | Fun x -> x
  | App (f, x) -> (eval f) (eval x) ;;

external val print : 'a -> unit = "print" ;;

let mul a b = App (App (Fun (fun x y -> x * y), a), b) ;;

let main _ = eval (App (Fun print, mul (Lit 123) (Lit 2)))
