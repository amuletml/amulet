external val external_id : forall 'a. 'a -> 'a = "(function(x) return x end)"

let main =
  let foo x = external_id x
  foo ()
  foo 1
