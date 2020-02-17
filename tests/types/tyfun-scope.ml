class idx 'a
  type key
  val idx_key : 'a -> key 'a -> ()
  val idx : 'a -> unit

open
  type array 'a

  instance idx (array 'a)
    type key = int
    let idx_key _ _ = ()
    let idx _ = ()

let f : array int -> unit = fun x -> idx_key x 0
