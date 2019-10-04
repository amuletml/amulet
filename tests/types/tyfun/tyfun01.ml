type function f 'a : type begin
  f int = string
  f string = int
end

type ev 'a =
  | Int : int -> ev int
  | Str : string -> ev string

let foo (x : ev 'a) : f 'a =
  match x with
  | Int _ -> ""
  | Str _ -> 0

type dict 'c = Dict : 'c => dict 'c
type 'p :- 'q = Sub of ('p => unit -> dict 'q)

let tf_equality1 : forall 'a. ('a ~ int) :- (f 'a ~ string) = Sub (fun _ -> Dict)
let tf_equality2 : forall 'a. ('a ~ string) :- (f 'a ~ int) = Sub (fun _ -> Dict)
let tf_refl : forall 'a. dict (f 'a ~ f 'a) = Dict
