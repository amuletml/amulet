type cont 'a = C of forall 'r. ('a -> 'r) -> 'r

let run_cont (C x) = x
let elim_cont (C x) = x (fun x -> x)

let f @@ x = f x

let call_cc (f : ('a -> cont 'b) -> cont 'a) : cont 'a = C @@ fun k ->
  run_cont (f (fun x -> C (fun _ -> k x))) k