val foreign print "print" : string -> unit;

type list 'a =
  | Nil
  | Cons 'a * (list 'a) ;

let upTo s e
  = if s == e then
      Cons e Nil
    else
      Cons s (upTo (s + 1) e)
and main _ = print (upTo 1 10)
