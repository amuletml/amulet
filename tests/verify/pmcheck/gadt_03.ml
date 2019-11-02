type a = A of int
type b = B of string

type t 'a =
  | A : t a
  | B : t b

let rec f (A : t a) : a = f A
