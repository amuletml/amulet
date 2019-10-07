type x 'a =
  | Xi : x int
  | Xb : x bool
  | Xs : x string

type function collapse 'a begin
  collapse int = int
  collapse 'a = string
end

let collapse : forall 'a. x 'a -> x (collapse 'a) =
  function
  | Xi -> Xi
  | _ -> Xs
