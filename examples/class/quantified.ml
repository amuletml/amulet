let x && y = if x then y else false

class eq 'a
  val (.==) : 'a -> 'a -> bool

instance eq ()
  let (.==) = (==)

type id 'a = Id of 'a

instance eq 'a => eq (id 'a)
  let (Id x) .== (Id y) = x .== y

type option 'a = Some of 'a | None

instance eq 'a => eq (option 'a)
  let x .== y =
    match x, y with
    | None, None -> true
    | Some x, Some y -> x .== y
    | _, _ -> false

type rose 'f 'a = Branch of 'a * 'f (rose 'f 'a)

instance eq 'a * (forall 'b. eq 'b => eq ('f 'b)) => eq (rose 'f 'a)
  let (Branch (h, x)) .== (Branch (j, y)) = h .== j && x .== y

(* \/ does nothing *)
let true = Branch ((), None) .== Branch ((), None)

(* \/ explodes *)
let true = Branch ((), Some (Branch ((), None))) .== Branch ((), None)
