type dict 'c = Dict : 'c => dict 'c
type 'p :- 'q = Sub of ('p => unit -> dict 'q)

type function discrim 'a 'b begin
  discrim 'a 'a = int
  discrim 'a 'b = string
end ;;

class trivial begin end
instance trivial begin end

let trans (Dict : dict ('a ~ 'b)) (Dict : dict ('b ~ 'c)) : dict ('b ~ 'c) =
  Dict

let sym (Dict : dict ('a ~ 'b)) : dict ('b ~ 'a) = Dict

let sub (Dict : dict ('a ~ 'b)) (x : 'a) : 'b = x

let foo : forall 'a 'b. 'a ~ 'b :- (discrim 'a 'b ~ int) = Sub (fun _ -> Dict)
let no : forall 'a 'b. trivial :- (discrim 'a 'b ~ string) = Sub (fun _ -> Dict)
let oh_no : dict (string ~ int) =
  let Sub k = foo @type @int @int
  let Sub k2 = no @type @int @int
  sym @@ trans (k ()) (k2 ())

let no_no = sub oh_no "you were expecting an integer, but it was me, Dio!"
let bang = 123 + no_no
