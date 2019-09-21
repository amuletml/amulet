(* Ensure that we generate a nice and simple if with the test inline, rather
   than doing a full pattern match *)
let main f = if f 1
             then f 2
             else f 3

external val ignore : 'a -> () = "nil"
let () = ignore main
