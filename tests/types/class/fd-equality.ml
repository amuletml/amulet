type dict ('c : constraint) = Dict : 'c => dict 'c
type 'p :- 'q = Sub of ('p => unit -> dict 'q)

class class_fd 'a 'c | 'a -> 'c begin end

let fd : forall 'a 'b 'c. class_fd 'a 'b * class_fd 'a 'c :- 'b ~ 'c =
  Sub (fun _ -> Dict)
