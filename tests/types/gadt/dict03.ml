type dict ('c : constraint) = Dict : 'c => dict 'c
type 'p :- 'q = Sub of ('p => dict 'q)

class ord 'a begin end
class ord 'a => eq 'a begin end

let foo : forall 'a. eq 'a :- ord 'a = Sub Dict
