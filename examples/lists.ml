external val print : 'a -> unit = "print"    ;;
external val write : string -> unit = "io.write" ;;
external val string_of : 'a -> string = "tostring" ;;

let compose f g x = f (g x) ;;

type list 'a =
  | Nil
  | Cons of 'a * list 'a ;;

let id x = x ;;

let map (f : 'a -> 'b) (xs : list 'a) : list 'b =
  let map_magic xs cont =
    match xs with
    | Nil -> cont Nil
    | Cons (h, t) -> map_magic t (compose cont (fun x -> Cons (f h, x)))
  in map_magic xs id ;;

let print_list xs =
  let inner xs k =
    match xs with
    | Cons (h, t) -> begin
      write "Cons (";
      write (string_of h);
      write ", ";
      inner t (compose k (fun _ -> write ")"))
    end
    | Nil -> begin
      write "Nil)";
      k ()
    end
  in inner xs (fun _ -> print "") ;;

let range start stop =
  if start == stop then
      Nil
    else
      Cons (start, range (start + 1) stop) ;;

let main _ = print_list (map (+ 1) (range 1 10))
