let auto (x : forall 'a. 'a -> 'a) : forall 'b. 'b -> 'b = x
let choose (x : 'a) (_ : 'a) = x

let id x = x

(* This /would/ work, as in the paper, but amulet deeply instantiates
 * the type of auto to be the same as auto' in ql-impredicative05.ml.
 *
 * Darn. *)
let t = choose id auto
