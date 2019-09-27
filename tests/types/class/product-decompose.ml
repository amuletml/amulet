type dict ('c : constraint) = Dict : 'c => dict 'c
type 'p :- 'q = Sub of ('p => dict 'q)

let weaken1 : forall 'a 'b. ('a * 'b) :- 'a = Sub Dict
let weaken2 : forall 'a 'b. ('a * 'b) :- 'b = Sub Dict
