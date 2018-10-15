type list 'a = Nil | Cons of 'a * list 'a

class show 'a
  val show : 'a -> string

instance show ()
  let show () = "()"

instance show 'a => show (list 'a)
  let show = function
    | Nil -> "[]"
    | Cons (x, Nil) -> "[" ^ show x ^ "]"

instance show 'a * show (list 'b) => show ('a * 'b)
  let show (a, b) = "(" ^ show a ^ ", " ^ show b ^ ")"

(* One variable appearing more times *)
instance show 'a * show 'b * show 'b => show ('a * 'b)
  let show (a, b) = "(" ^ show a ^ ", " ^ show b ^ ")"

(* Multiple variables appearing more times *)
instance show 'a * show 'a * show 'b * show 'b => show ('a * 'b)
  let show (a, b) = "(" ^ show a ^ ", " ^ show b ^ ")"
