class category 'cat begin
  val id   : 'cat 'a 'a
  val (%)  : 'cat 'b 'c -> 'cat 'a 'b -> 'cat 'a 'c
  val (%>) : 'cat 'a 'b -> 'cat 'b 'c -> 'cat 'a 'c

  let f %> g = g % f
  let g % f = f %> g
end

instance category (->) begin
  let id x = x
  let f % g = fun x -> f (g x)
  let g %> f = fun x -> f (g x)
end

let uncurry f (x, y) = f x y
let curry f x y = f (x, y)

let private swap (x, y) = (y, x)
let private dup x = (x, x)

class category 'arr => arrow 'arr begin
  val arr     : ('a -> 'b) -> 'arr 'a 'b
  val first   : 'arr 'b 'c -> 'arr ('b * 'd) ('c * 'd)
  val second  : 'arr 'b 'c -> 'arr ('d * 'b) ('d * 'c)
  val ( *** ) : 'arr 'a 'b -> 'arr 'c 'd -> 'arr ('a * 'c) ('b * 'd)
  val ( &&& ) : 'arr 'a 'b -> 'arr 'a 'c -> 'arr 'a ('b * 'c)

  let first f  = f *** arr id
  let second f = arr swap %> first f %> arr swap

  let f *** g = first f %> second g
  let f &&& g = arr dup %> f *** g
end

instance arrow (->) begin
  let arr f = f
  let first f (x, y) = (f x, y)
  let second f (x, y) = (x, f y)
  let f *** g = fun (x, y) -> (f x, g y)
  let f &&& g = fun x -> (f x, g x)
end

let f ^>> x = arr f %> x
let x >>^ f = x %> arr f

let x <<^ f = x % arr f
let f ^<< x = arr f % x

(* use inside classes doesn't count for verify *)
let _ = (swap, dup)
