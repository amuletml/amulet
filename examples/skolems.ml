type id 'a = Id of 'a ;;

let pure (f : forall 'a. 'a -> 'f 'a) = f 1 ;;
let pure_checks = pure Id ;;
let pure_errors = pure (fun x -> x) ;;

let escapes (f : forall 'a. 'a -> 'b) = f "foo" ;;
let escape_checks = escapes (fun x -> 0) ;;
let escape_errors = escapes (fun x -> x) ;;

let poly (f : forall 'a. 'a -> 'a) = f () ;;
let poly_checks = poly (fun x -> x) ;;
let poly_errors = poly (fun x -> x + 1) ;;

let main _ = 1

