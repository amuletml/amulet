let foo : ( (forall 'a. 'a -> 'a) -> bool ) * int =
  ( (fun _ -> false) : (forall 'b. lazy 'b -> lazy 'b) -> bool
  , 0
  )
