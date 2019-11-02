external val (+) : int -> int -> int = ""
external val (==) : 'a -> 'a -> bool = ""
external val print : 'a -> unit = "print"
external val write : string -> unit = "io.write"
external val string_of : 'a -> string = "tostring"

let compose f g x = f (g x)

type list 'a =
  | Nil
  | Cons of 'a * list 'a

let id x = x

let map f =
  let rec go cont = function
    | Nil -> cont Nil
    | Cons (h, t) -> go (compose cont (fun x -> Cons (f h, x))) t
  go id

let print_list xs =
  let rec inner xs k =
    match xs with
    | Cons (h, t) ->
      write "Cons ("
      write (string_of h)
      write ", "
      inner t (compose k (fun _ -> write ")"))
    | Nil ->
      write "Nil)"
      k ()
  inner xs (fun _ -> print "")

let rec range start stop =
  if start == stop then
    Nil
  else
    Cons (start, range (start + 1) stop)

let _ = print_list (map (+ 1) (range 1 10))
