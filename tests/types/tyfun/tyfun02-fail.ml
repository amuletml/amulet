type x 'a =
  | Xi : x int
  | Xb : x bool
  | Xs : x string

type function collapse 'a begin
  collapse int = int
  collapse 'a = string
end ;;

let collapse (x : x 'a) : x (collapse 'a) =
  match x with
  | Xi -> Xi
  | _ -> Xs
