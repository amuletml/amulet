type box =
  | Box : ('a -> string) * 'a -> box ;;

let show_it (Box (f, x)) = f x
