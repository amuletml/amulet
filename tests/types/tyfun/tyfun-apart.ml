type dict 'c = Dict : 'c => dict 'c
type 'p :- 'q = Sub of ('p => unit -> dict 'q)

type function discrim 'a 'b begin
  discrim 'a 'a = int
  discrim 'a 'b = string
end ;;

class trivial begin end
instance trivial begin end

let foo : forall 'a 'b. 'a ~ 'b :- (discrim 'a 'b ~ int) = Sub (fun _ -> Dict)
let no : forall 'a 'b. trivial :- (discrim 'a 'b ~ string) = Sub (fun _ -> Dict)
