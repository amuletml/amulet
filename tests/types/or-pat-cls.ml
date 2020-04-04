class x 'a begin end
type t = X : x int => int -> t | Y of int

let foo (X x | Y x) = x
