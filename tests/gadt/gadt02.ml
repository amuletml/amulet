type t2 'a =
  | L1 : t2 int
  | L2 : t2 bool

type t1 'a =
  | K1 : t1 int
  | K2 : t2 int -> t2 bool
