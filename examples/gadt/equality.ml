external val unsafe_coerce : 'a -> 'b = "(function(x) return x end)" ;;
external val print : 'a -> unit = "print" ;;

type s 'a = S of 'a ;;
type k 'a = K of 'a ;;

type eq 'a 'b =
  | Refl : eq 'a 'a ;;

let subst (Refl : eq 'a 'b) (x : 'a) : 'b = x ;;

let foo (S x : 'a) =
  subst (unsafe_coerce Refl : eq (s 'a) (k 'a)) (S x) ;;

let main = print (foo (S 1))
