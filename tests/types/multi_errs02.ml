external val ( + ) : int -> int -> int = ""
let () =
  let foo x = 2 + true
  foo "foo"
  foo 1

(* doesn't get down here *)
let () = "foo"
