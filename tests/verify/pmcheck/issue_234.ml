type atom = I of int
type expr =
  | Atom of atom
  | Case of expr * list () * expr

let t expr =
  match expr with
  | Case (Atom(t), s, f)->()
