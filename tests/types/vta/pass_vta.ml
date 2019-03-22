let id (x : 'a) : 'a = x

(* variable *)
let var = id @int

(* ascription *)
let asc = ((fun x -> x) : forall 'a. 'a -> 'a) @int

let let' = (let foo x = x in foo) @int
let let' = (let foo = fun x -> x in foo) @int
let let' = (let foo = id in foo : forall 'a. 'a -> 'a) @int
let let' = (let 2 + 2 = 5 in id) @int

(* begin *)
let begin' = (begin (); id end) @int

(* rank-n *)
let foo : forall 'a. 'a -> forall 'b. 'b -> 'b = fun _ x -> x
let rankn = foo 1 @int
let rankn = foo @int 1 2
let rankn = foo @int 1 @int 2

let bar (x : forall 'a. 'a -> unit) =
  x @int 1
  x @string "foo"

let baz (x : 'a) (y : 'b) = ()
let _ = baz @int @int 1 2
