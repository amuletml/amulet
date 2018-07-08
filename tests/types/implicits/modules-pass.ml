module Foo =
  let implicit x = 1

open Foo

let foo : int => int = fun x -> x + 1

let main : int = foo
