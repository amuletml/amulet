type exp 'a =
  | Exp : int -> exp int

type hidden =
  | MkHid : forall 'a. exp 'a * exp 'a -> hidden

let hval = MkHid (Exp 0, Exp 1)

let weird =
  match hval with
  | MkHid (Exp _, a) -> a

