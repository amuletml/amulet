(* Tests the ambiguous type error message *)

type foo 'a = Foo of int

let implicit foo_int : foo int = Foo 1
let implicit foo_string : foo string = Foo 1

let foo : forall 'a. foo 'a => foo 'a = fun x -> x

let main = foo
