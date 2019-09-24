type void = |

let _ =
  let foo (x : 'a) : 'a = _

  let rearrange : forall 'a 'b. 'a * 'b -> 'b * 'a =
    _

  let implies : forall 'a 'b. ('a -> 'b) -> 'a -> 'b = 
    _

  let compose : forall 'a 'b 'c. ('b -> 'c) -> ('a -> 'b) -> 'a -> 'c = 
    _

  let contrapositive : forall 'p 'q. ('p -> 'q) -> ('p -> void) -> 'q -> void =
    _

  ()
