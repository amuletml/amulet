type t 'a = X : 'a -> t 'a | Y : int -> t int

let foo (X x | Y x) = x
