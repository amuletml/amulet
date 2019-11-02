type dict ('c : constraint) = Dict : 'c => dict 'c

class show 'a begin
  val show : 'a -> string
end

instance show () begin
  let show _ = "()"
end

let foo : dict (show ()) = Dict
let rec bar : unit -> dict (show int) = fun () -> bar ()

let with_d (x : dict ('c 'a)) (e : 'a) (foo : forall 'a. 'c 'a => 'a -> 'b) : 'b =
  match x with
  | Dict -> foo e

let x = with_d foo () (fun x -> show x)
