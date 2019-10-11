external val (+) : int -> int -> int = ""

let foo : (forall 'a. 'a -> 'a) * (forall 'a. 'a -> int) = (fun x -> x, fun x -> 1)

let fst (x, y) = x
let snd (x, y) = y

let main =
  fst foo 1 + snd foo ()
