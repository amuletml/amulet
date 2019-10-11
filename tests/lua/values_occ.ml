external val ( * ) : int -> int -> int = "function(x, y) return x * y end"

external val print : string -> unit = "print"
external val to_string : 'a -> string = "tostring"

type step 's 'a =
  | Yield of 'a * 's

type stream 'a =
  | Stream : forall 's. ('s -> step 's 'a) * 's -> stream 'a

let map f (Stream (go, start)) =
  let loop st =
    match go st with
    | Yield (x, s) -> Yield (f x, s)
  Stream (loop, start)

let sum_squares xs =
  map (fun x -> x * x) xs

let () =
  print (to_string sum_squares)
