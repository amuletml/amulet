external val unsafe_coerce : 'a -> 'b = "(function(x) return x end)" ;;
external val print : 'a -> unit = "print" ;;

type s 'a = S of 'a ;;
type k 'a = K of 'a ;;

module Equality = begin
  type t 'a 'b =
    | Refl : t 'a 'a ;;

  let subst (Refl : t 'a 'b) (x : 'a) : 'b = x ;;

  let sym (Refl : t 'a 'b) : t 'b 'a = Refl
  and trans (Refl : t 'a 'b) (Refl : t 'b 'c) : t 'a 'c = Refl
end ;;

let s_is_k (x : 'a) : Equality.t (s 'a) (k 'a) = unsafe_coerce Equality.Refl

and s_to_k (S x) = Equality.subst (s_is_k x) (S x)
and k_to_s (K x) = Equality.subst (Equality.sym (s_is_k x)) (K x)

and k_to_s_to_k x = s_to_k (k_to_s x)
and s_to_k_to_s x = k_to_s (s_to_k x) ;;

let main = print (s_to_k (S 1))
