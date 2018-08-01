type foo 'a = Foo of 'a
type bar 'a = Bar of { x : foo 'a, y : int }

(* Matching partially on a closed record would break Core Lint *)
let head_of_x (Bar { x = Foo e }) = e
