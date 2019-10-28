(* Library imports *)
module M = import "lua/math.ml" (* math.ml won't change *)

(* Relative paths *)
module M = import "./modules/basic.ml"
module M = import "../resolve/modules/basic.ml"
