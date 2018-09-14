let () =
  let foo x = 2 + true
  foo "foo"
  foo 1
(* doesn't spot this either *)
and () = "foo"
