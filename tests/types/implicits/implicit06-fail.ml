(* Tests the ambiguous type error message *)

type foo 'a = Foo of int

let implicit foo_int : foo int = Foo 1
let implicit foo_string : foo string = Foo 1
let implicit foo_unit : foo () = Foo 1
let implicit foo_float : foo float = Foo 1
let implicit foo_lazy_int : foo (lazy int) = Foo 1
let implicit foo_lazy_string : foo (lazy string) = Foo 1

let foo : forall 'a. foo 'a => foo 'a = fun x -> x

let main : forall 'a. foo 'a = foo
