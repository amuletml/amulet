type 'f ~> 'g <- forall 'x. 'f 'x -> 'g 'x

type id 'x = Id of 'x

let foo : id ~> list =
  fun (Id x) -> [x]
