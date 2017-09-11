val foreign print "print" : string -> unit;
val foreign string_of_number "tostring" : int -> string;

type list 'a =
  | Nil
  | Cons 'a * (list 'a) ;

let upTo s e
  = if s == e then
      Cons e Nil
    else
      Cons s (upTo (s + 1) e)
and sum xs
  = match xs with
    | Cons a b -> a + sum b
    | Nil -> 0
and main _ =
  let li = upTo 1 100
  and sum_of = sum li
  and num = string_of_number sum_of
  in print num
