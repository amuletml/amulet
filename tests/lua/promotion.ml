external val ignore : 'a -> unit = "function(x) end"

type id 'a  = Id of 'a

let main f =
  let Id x = f ()
  match x with
  | (a, _) ->
     match x with
     | (_, b) -> (a, b)

let () = ignore main
