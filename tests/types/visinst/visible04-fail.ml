let foo : forall ('a : type) -> 'a -> forall ('b : type) -> 'b -> 'a =
  fun x _ -> x ;;

let main = foo @{unit} 1 @{unit} ()
