external val print : 'a -> unit = "print"

type nt = Nt of int
type not = A of nt | B

let foo x =
  match x with
  | A (Nt x) -> x
  | B -> 0

let () = print foo
