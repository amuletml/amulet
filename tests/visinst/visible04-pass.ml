let foo : forall ('a : type) -> 'a -> forall ('b : type) -> 'b -> 'a =
  fun x _ -> x ;;

let main = foo @{int} 1 @{unit} ()
