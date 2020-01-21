open import "prelude.ml"

type church 'a = ChurchList of forall 'r. ('a -> 'r -> 'r) -> 'r -> 'r

let unList (ChurchList l) = l

let cons x xs = ChurchList (fun k z -> k x (unList xs k z))
and nil = ChurchList (fun _ z -> z)

instance show 'a => show (church 'a)
  let show l = unList l (fun x k () -> show x ^ " :: " ^ k ()) (fun _ -> "nil") ()

let () = print (cons 1 (cons 2 nil))
