(* Missing variables *)
let _ =
  let foo_1 = 0
  let foo_2 = 0
  let bar_0 = 0
  foo_0

(* Missing types *)
type foo_1
type foo_2
type bar_0

let _ : foo_0 = ()


(* Missing constructors *)
type ctor = Foo1 | Foo2 | Bar0
let _ = Foo0

(* Missing type variables *)
let _ : forall 'foo_1 'foo_2 'bar_0. () -> 'foo_0 = ()
