external val print : string -> unit = "print" ;;

type church 'a = ChurchList of forall 'r. ('a -> 'r -> 'r) -> 'r -> 'r ;;

let unList (ChurchList l) = l ;;

let cons x xs =
  ChurchList (fun k z -> k x (unList xs k z))
and nil =
  ChurchList (fun k z -> z) ;;

let printList l =
  unList l (fun x k () -> begin print x; k () end) (fun _ -> print "nil") () ;;

let main = printList (cons "foo" (cons "bar" nil))
