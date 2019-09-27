(* Important properties of the category of constraints. The solver needs
 * to be able to type check all of these!
 *)

type dict 'c = Dict : 'c => dict 'c
type 'p :- 'q = Sub of ('p => unit -> dict 'q)

(* Pair decomposition rules *)
let weaken1 : forall 'a 'b. ('a * 'b) :- 'a = Sub (fun _ -> Dict)
let weaken2 : forall 'a 'b. ('a * 'b) :- 'b = Sub (fun _ -> Dict)

(* Structural rules *)
let contract : forall 'a. 'a :- 'a * 'a = Sub (fun _ -> Dict)
let ignore : forall 'a. 'a * 'a :- 'a = Sub (fun _ -> Dict)
let swap : forall 'a 'b. ('a * 'b) :- 'b * 'a = Sub (fun _ -> Dict)

let strengthen (Dict : dict 'b) (Sub k : 'a :- 'c) : 'a :- ('b * 'c) =
  Sub (fun _ -> match k () with | Dict -> Dict)

let strengthen2 (Dict : dict 'b) (Sub k : 'a :- 'c) : 'a :- ('c * 'b) =
  Sub (fun _ -> match k () with | Dict -> Dict)

(* Entailment is a reflexive, transitive property *)
let id : forall 'a. 'a :- 'a = Sub (fun _ -> Dict)
let trans (Sub p : 'a :- 'b) (Sub q : 'b :- 'c) : 'a :- 'c =
  Sub (fun _ -> match p () with | Dict -> match q () with | Dict -> Dict)

class c 'a begin
  val c_takes_type : 'a
end
class c 'a => d 'a begin end

let superclass : forall 'a. d 'a :- c 'a = Sub (fun _ -> Dict)

(* The category of constraints has initial and terminal morphisms *)
class string ~ int => impossible begin
  val no : 'a
end

class trivial begin end
instance trivial begin end

let initial : forall 'a. impossible :- 'a = Sub (fun _ -> no)
let terminal : forall 'a. 'a :- trivial = Sub (fun _ -> Dict)

(* The functor Dict is fully faithful *)
let map_dict (Sub k : 'a :- 'b) (Dict : dict 'a) : dict 'b =
  match k () with
  | Dict -> Dict

(* .. which means you can turn any mapping between dictionaries into an
 * entailment *)
let unmap_dict (f : dict 'a -> dict 'b) : 'a :- 'b =
  Sub (fun _ -> f Dict)

(* GADTs imply equality constraints: *)
type 'a :~: 'b = Refl : 'a :~: 'a

(* 'a :~: 'b is just as good as dict ('a ~ 'b) *)
let eq_dict (Refl : 'a :~: 'b) : dict ('a ~ 'b) =
  Dict

let dict_eq (Dict : dict ('a ~ 'b)) : 'a :~: 'b =
  Refl
