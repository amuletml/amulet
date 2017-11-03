external val print : 'a -> unit = "print" ;;
external val tostring : 'a -> string = "tostring" ;;

type list 'a =
  | Cons of 'a * list 'a
  | Nil ;;

let reverse =
  let reverse_accum acc x =
    match x with
      | Cons (h, t) -> reverse_accum (Cons (h, acc)) t
      | Nil -> acc
  in reverse_accum Nil ;;

let map f =
  let map_accum acc x =
    match x with
      | Cons (h, t) -> map_accum (Cons (f h, acc)) t
      | Nil -> reverse acc
  in map_accum Nil ;;

let print_list ls =
  match ls with
    | Cons (h, t) -> begin print h; print_list t end
    | Nil -> print "done" ;;

let range start stop =
  if start == stop then
      Nil
    else
      Cons (start, range (start + 1) stop) ;;

let main _ = print_list (map (+ 1) (range 1 10))
